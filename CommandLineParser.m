BeginPackage["CommandLineParser`"];

ParseCommandLine;

NumericSpec;
StringSpec;
BooleanSpec;
EnumSpec;
RepeatedSpec;

Begin["`Private`"];

(******************************************************************)
(***************************** PARSER *****************************)
(******************************************************************)

ParseCommandLine[allSpecs_] := ParseCommandLine[allSpecs, getCommandLineArgs[]]
ParseCommandLine[{pos_, opts_}, args_] := ParseCommandLine[{pos, opts, ""}, args]
ParseCommandLine[spec_, $Failed] := (
	Message[ParseCommandLine::clfail];
	Abort[];
)

ParseCommandLine[{posSpecsRaw_, optSpecsRaw_, helpHeader_}, args_] := Module[
	{optPos, posArgs, optArgs, posParsed, optParsed},
	If[!MatchQ[args], {Repeated[_?StringQ]},
		Message[ParseCommandLine::nostring];
		Abort[];
	];
	checkRawSpecs[{posSpecsRaw, optSpecsRaw}];
	{posSpecs, optSpecs} = {toSpec /@ posSpecsRaw, toSpec /@ optSpecsRaw};
	If[MemberQ[args, "--help"],
		printHelp @@ {posSpecs, optSpecs, helpHeader};
		Exit[0]
	];

	optPos = First @ FirstPosition[
		args, 
		_?(StringQ[#] && StringStartsQ[#, "--"]&), 
		{Length[args] + 1}
	];
	{posArgs, optArgs} = TakeDrop[args, optPos - 1];

	posParsed = parsePosArgs[posSpecs, posArgs];
	optsParsed = parseOptArgs[optSpecs, optArgs];

	{posParsed, optsParsed}
];

checkRawSpecs[{pos_, opt_}] := Module[{variadicPos},
	If[!ListQ[pos],
		Message[ParseCommandLine::badspec, "Positional", pos];
		Abort[]		
	];
	If[!ListQ[opt],
		Message[ParseCommandLine::badspec, "Optional", opt];
		Abort[]		
	];
	Scan[checkRawSpec[#, False]&, pos];
	Scan[checkRawSpec[#, True]&, opt];
	variadicPos = Position[pos, "Variadic" -> True, {3}];
	If[
		Or[
			Length[variadicPos] > 1,
			Length[variadicPos] === 1 && variadicPos[[1, 1]] =!= Length[pos]
		],
		Message[ParseCommandLine::badvariadic];
		Abort[]
	]
]

checkRawSpec[spec:Rule[name_, data_], isOpt_] := Module[{test},
	test = And[
		2 <= Length[data] <= 2 + Length[Options[toSpec2]],
		Developer`EmptyQ @ Complement[
			Keys @ Cases[data, HoldPattern[s_ -> _]], 
			Keys @ Options[toSpec2]
		],
		If[isOpt,
			MatchQ[name, {_?StringQ, _?StringQ}],
			MatchQ[name, _?StringQ]		
		]
	];
	If[!test,
		Message[ParseCommandLine::badspec, If[isOpt, "Optional", "Positional"], spec];
		Abort[]
	]
];
checkRawSpec[spec_, isOpt_] := (
	Message[ParseCommandLine::badspec, If[isOpt, "Optional", "Positional"], spec];
	Abort[]
)

toSpec[name_ -> data_] := Apply[toSpec2, Prepend[data, name]]

Options[toSpec2] = {"Parser" -> ToExpression, "PostCheck" -> Function[True], 
	"Variadic" -> False};

toSpec2[name_, patt_, doc_, OptionsPattern[]] := Module[{name2, addDefault, default, res},
	If[ListQ[name],
		{name2, default} = name;
		addDefault = True,
		name2 = name;
		addDefault = False
	];
	res = <|"Name" -> name2, "StringPattern" -> patt, "Documentation" -> doc, 
		"Parser" -> OptionValue["Parser"], "PostCheck" -> OptionValue["PostCheck"],
		"Variadic" -> OptionValue["Variadic"]|>;
	If[addDefault,
		Append[res, "Default" -> default],
		res
	]
]

parsePosArgs[specs_, posArgs_] := Module[{isVariadic, specs2, parsed, most, variadic},
	isVariadic = specs[[-1, "Variadic"]];
	If[
		Or[
			!isVariadic && Length[posArgs] =!= Length[specs],
			isVariadic && Length[posArgs] < Length[specs] - 1
		],
		Message[ParseCommandLine::poslen, Length[posArgs], 
			If[isVariadic, "at least " <> ToString[Length[specs]], Length[specs]]];
		Abort[]
	];
	specs2 = If[isVariadic,
		PadRight[specs, Length[posArgs], Last[specs]],
		specs
	];
	parsed = MapThread[parseSingle, {specs2, posArgs, Range @ Length[specs2]}];
	If[isVariadic,
		{most, variadic} = TakeDrop[parsed, Length[specs] - 1];
		parsed = Append[most, variadic]
	];
	parsed
];

parseOptArgs[specs_, optArgs_] := Module[{provided, providedAssoc, unknown},
	provided = Transpose @ StringReplace[optArgs, 
		{
			"--" ~~ name__ ~~ "=" ~~ val___ :> {name, val},
			"--" ~~ name__ :> {name, "True"},
			_ :> (Message[ParseCommandLine::badopts, optArgs]; Abort[];)
		}
	][[All, 1]];
	providedAssoc = If[Developer`EmptyQ[provided], <||>, AssociationThread @@ provided];
	(* ^ <|optName -> val, ...|> *)
	If[!Developer`EmptyQ[unknown = Complement[Keys @ providedAssoc, specs[[All, "Name"]]]],
		Message[ParseCommandLine::unkopt, unknown];
		Abort[]
	];
	Association @ Map[
		#["Name"] -> parseSingle[
			#, 
			Lookup[providedAssoc, #["Name"], #["Default"]],
			None
		]&,
		specs
	]
];

parseSingle[spec_, arg_, pos_] := Module[{parsed},
	If[Not @ StringMatchQ[arg, spec["StringPattern"]],
		Message[ParseCommandLine::nomatch, 
			Replace[pos, {None -> "Optional", _ -> "Positional"}], 
			argNameWithPos[spec["Name"], pos], 
			arg, 
			spec["Documentation"]
		];
		Abort[]
	];
	parsed = spec["Parser"][arg];
	If[Not @ TrueQ @ spec["PostCheck"][parsed],
		Message[ParseCommandLine::failcheck, 
			Replace[pos, {None -> "Optional", _ -> "Positional"}], 
			argNameWithPos[spec["Name"], pos], 
			parsed, 
			spec["Documentation"]
		];
		Abort[]
	];
	parsed
];

argNameWithPos[name_, pos_] := StringJoin[
	name,
	Replace[pos, {None -> "", n_ :> StringJoin[" (position ", ToString[n], ")"]}]
]

getCommandLineArgs[] := Which[
	!Developer`EmptyQ[$ScriptCommandLine],
		Rest[$ScriptCommandLine],
	!Developer`EmptyQ[$CommandLine],
		$CommandLine[[First @ FirstPosition[$CommandLine, "-script", 0] + 2 ;; -1]],
	True,
		$Failed
];

printHelp[posSpecs_, optSpecs_, helpHeader_] := Module[{maxNameLen, maxDefLen},
	If[helpHeader =!= "",
		Print[helpHeader];
	];
	If[!Developer`EmptyQ[posSpecs],
		maxNameLen = Max @ StringLength @ posSpecs[[All, "Name"]];
		Print["Positional arguments:"];
		Scan[
			Print @ StringJoin[
				StringPadRight[#Name, maxNameLen],
				"    ",
				#Documentation
			]&, 
			posSpecs
		];
	];
	If[!Developer`EmptyQ[optSpecs],
		maxNameLen = Max @ StringLength @ optSpecs[[All, "Name"]];
		maxDefLen  = Max @ StringLength @ optSpecs[[All, "Default"]];
		Print["Optional arguments:"];
		Scan[
			Print @ StringJoin[
				StringPadRight[#Name, maxNameLen],
				"    ",
				StringPadRight[#Default, maxDefLen],
				"    ",
				#Documentation
			]&, 
			optSpecs
		];
	];
]

(******************************************************************)
(************************** SPEC HELPERS **************************)
(******************************************************************)

Options[NumericSpec] = {
	"Interval" -> {-Infinity, Infinity},
	"AllowInfinity" -> False,
	"Variadic" -> False
};
NumericSpec[type_, doc_, OptionsPattern[]] := Module[
	{interval, allowInf, patt, parser, checks, postCheck},
	{interval, allowInf} = {OptionValue["Interval"], OptionValue["AllowInfinity"]};
	patt = If[allowInf, 
		Alternatives[NumberString, "infinity", "Infinity", "-infinity", "-Infinity"],
		NumberString
	];
	parser = Function @ Replace[#, 
		{
			"infinity"|"Infinity" -> Infinity,
			"-infinity"|"-Infinity" -> -Infinity,
			n_ :> ToExpression[n]
		}
	];

	checks = With[{interval = interval},
		{IntervalMemberQ[Interval[interval], #]&}
	];
	Which[
		type === "Integer" && allowInf,
			AppendTo[checks, 
				Function[IntegerQ[#] || MatchQ[#, DirectedInfinity[-1|1]]]
			],
		type === "Integer",
			AppendTo[checks, Function[IntegerQ[#]]]
	];
	postCheck = With[{checks = checks}, Function[{val}, And @@ Map[#[val]&, checks]]];

	{patt, doc, "Parser" -> parser, "PostCheck" -> postCheck, 
		"Variadic" -> OptionValue["Variadic"]}
];

Options[StringSpec] = {"Variadic" -> False};
StringSpec[doc_, OptionsPattern[]] := {___, doc, "Parser" -> Identity, 
	"Variadic" -> OptionValue["Variadic"]
}

Options[BooleanSpec] = {"Variadic" -> False};
BooleanSpec[doc_, OptionsPattern[]] := {
	"true"|"false"|"True"|"False", 
	doc, 
	"Parser" -> Function[Replace[#, {"true"|"True" -> True, "false"|"False" -> False}]],
	"Variadic" -> OptionValue["Variadic"]
}

Options[EnumSpec] = {"Variadic" -> False};
EnumSpec[values_, doc_, OptionsPattern[]] := Module[
	{replacements, patt, parser, specDoc, outputDoc},
	replacements = Map[
		With[{s = ToString[#]},
			Apply[Alternatives, {s, ToLowerCase[s]}] -> #
		]&,
		values
	];
	patt = Alternatives @@ Keys[replacements];
	parser = With[{r = replacements}, Function[Replace[#, r]]];
	{patt, doc, "Parser" -> parser, "Variadic" -> OptionValue["Variadic"]}
]

RepeatedSpec[singleSpec_, separator_, doc_] := Module[
	{singlePatt, singleParser, singleCheck},
	singlePatt = First[singleSpec];
	singleParser = FirstCase[singleSpec, HoldPattern["Parser" -> p_] :> p, ToExpression];
	singleCheck = FirstCase[singleSpec, HoldPattern["PostCheck" -> c_] :> c, True&];
	{
		(RepeatedNull[singlePatt ~~ separator] ~~ singlePatt) | "",
		doc,
		"Parser" -> With[{p = singleParser}, 
			Function[Map[p, StringSplit[#, separator]]]
		],
		"PostCheck" -> With[{c = singleCheck}, Function[And @@ Map[c, #]]]
	}
]

$helpMsg = "Please add the flag --help for documentation.";
ParseCommandLine::badopts = "Optional arguments were not correctly formatted: ``.";
ParseCommandLine::badspec = "`` argument specification `` is not valid.";
ParseCommandLine::badvariadic = "Invalid argument specification: only one positional argument can be variadic and must appear as the end of the positional argument specification.";
ParseCommandLine::clfail = "Could not access the command line.";
ParseCommandLine::failcheck = "`` argument `` was parsed to ``, which is an invalid value. Documentation string for the argument is: \"``\".";
ParseCommandLine::nomatch = "`` argument `` was ``, which is an invalid value. Documentation string for the argument is: \"``\".";
ParseCommandLine::nostring = "Provided argument list was not a list of strings.";
ParseCommandLine::poslen = "`` positional arguments were passed but specification requires ``. " <> $helpMsg;
ParseCommandLine::unkopt = "Unknown options ``. " <> $helpMsg;

End[];
EndPackage[];