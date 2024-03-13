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
	If[!MatchQ[args, {RepeatedNull[_?StringQ]}],
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

checkRawSpecs[{args_, opts_}] := Module[{variadicPos, hasVariadic, split, test},
	If[!ListQ[args],
		Message[ParseCommandLine::badspec, "Positional", args];
		Abort[]		
	];
	If[!ListQ[opts],
		Message[ParseCommandLine::badspec, "Optional", opts];
		Abort[]		
	];
	Scan[checkRawSpec[#, False]&, args];
	Scan[checkRawSpec[#, True]&, opts];
	(* Only the last pos argument can be variadic and it must not have a default *)
	variadicPos = Flatten @ Position[args[[All, 2, 4]], True];
	Which[
		Developer`EmptyQ[variadicPos],
			hasVariadic = False,
		Or[
			Length[variadicPos] > 1,
			Length[variadicPos] === 1 && variadicPos =!= {Length[args]}
		],
			Message[ParseCommandLine::badvariadic1];
			Abort[],
		True,
			hasVariadic = True
	];
	(* Check position of positional arguments with optional values *)
	If[MemberQ[args, {_, _} -> _],
		split = Split[args, Length[First[#1]] === Length[First[#2]] &];
		test = If[hasVariadic,
			getLengths[split] === {0, 2, 0}|{2, 0},
			getLengths[split] === {0, 2}|{2}
		];
		If[!test,
			Message[ParseCommandLine::badposdef];
			Abort[]
		]
	]
];

getLengths[split_] := Flatten @ Map[DeleteDuplicates] @ Map[Length, split[[All, All, 1]], {2}]

checkRawSpec[spec:Rule[name_, data_], isOpt_] := Module[{test, hasVariadic},
	test = And[
		Length[data] === 5,
		If[isOpt,
			MatchQ[name, {_?StringQ, _?StringQ}],
			MatchQ[name, _?StringQ | {_?StringQ, _?StringQ}]
		]
	];
	If[!test,
		Message[ParseCommandLine::badspec, If[isOpt, "Optional", "Positional"], spec];
		Abort[]
	];
	hasVariadic = data[[4]];
	If[MatchQ[name, {_?StringQ, _?StringQ} && hasVariadic],
		Message[ParseCommandLine::badvariadic2];
		Abort[]
	]
];
checkRawSpec[spec_, isOpt_] := (
	Message[ParseCommandLine::badspec, If[isOpt, "Optional", "Positional"], spec];
	Abort[]
)

toSpec[name_ -> data_] := Apply[toSpec, Prepend[data, name]]
toSpec[name_, patt_, parser_, postCheck_, variadic_, doc_] := Module[
	{name2, addDefault, default, res},
	If[ListQ[name],
		{name2, default} = name;
		addDefault = True,
		name2 = name;
		addDefault = False
	];
	res = <|"Name" -> name2, "StringPattern" -> patt, "Documentation" -> doc, 
		"Parser" -> parser, "PostCheck" -> postCheck, "Variadic" -> variadic|>;
	If[addDefault,
		Append[res, "Default" -> default],
		res
	]
]

parsePosArgs[specs_, posArgs_] := Module[
	{hasVariadic, nOptional, nMandatory, posArgs2, specs2, parsed, most, variadic},
	hasVariadic = If[Length[specs] > 0,
		specs[[-1, "Variadic"]],
		False
	];
	nOptional = Count[specs, KeyValuePattern @ {"Default" -> _}];
	nMandatory = Length[specs] - nOptional - If[hasVariadic, 1, 0];
	If[
		Or[
			hasVariadic && !(nMandatory <= Length[posArgs]),
			!hasVariadic && !(nMandatory <= Length[posArgs] <= nMandatory + nOptional)
		],
		Message[ParseCommandLine::poslen, Length[posArgs], nMandatory,
			If[hasVariadic, "infinity", nMandatory + nOptional]];
		Abort[]
	];
	(* Add defaults *)
	posArgs2 = If[Length[posArgs] < Length[specs] && nOptional > 0,
		Join[
			posArgs,
			Take[specs, {Length[posArgs] + 1, If[hasVariadic, -2, -1]}][[All, "Default"]]
		],
		posArgs
	];
	(* This can shorten the specs in case the variadic has length 0 *)
	specs2 = If[hasVariadic,
		PadRight[specs, Length[posArgs2], Last[specs]],
		specs
	];
	parsed = MapThread[parseSingle, {specs2, posArgs2, Range @ Length[specs2]}];
	If[hasVariadic,
		{most, variadic} = TakeDrop[parsed, Length[specs] - 1];
		parsed = Append[most, variadic]
	];
	AssociationThread[specs[[All, "Name"]], parsed]
]

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

	{patt, parser, postCheck, OptionValue["Variadic"], doc}
];

Options[StringSpec] = {"Variadic" -> False};
StringSpec[doc_, OptionsPattern[]] := {___, Identity, True&, OptionValue["Variadic"], doc}

Options[BooleanSpec] = {"Variadic" -> False};
BooleanSpec[doc_, OptionsPattern[]] := {
	"true"|"false"|"True"|"False",
	Function[Replace[#, {"true"|"True" -> True, "false"|"False" -> False}]],
	True&,
	OptionValue["Variadic"],
	doc
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
	{patt, parser, True&, OptionValue["Variadic"], doc}
]

RepeatedSpec[singleSpec_, separator_, doc_] := Module[
	{singlePatt, singleParser, singleCheck},
	{singlePatt, singleParser, singleCheck} = singleSpec[[1 ;; 3]];
	{
		(RepeatedNull[singlePatt ~~ separator] ~~ singlePatt) | separator | "",
		With[{p = singleParser}, 
			Function[Map[p, StringSplit[#, separator]]]
		],
		With[{c = singleCheck}, Function[And @@ Map[c, #]]],
		False,
		doc
	}
]

$helpMsg = "Please add the flag --help for documentation.";
ParseCommandLine::badopts = "Optional arguments were not correctly formatted: ``.";
ParseCommandLine::badspec = "`` argument specification `` is not valid.";
ParseCommandLine::badposdef = "Invalid positional argument specification: positional arguments without defaults should appear before the ones with defaults. Only a single variadic argument can appear after.";
ParseCommandLine::badvariadic1 = "Invalid argument specification: only one positional argument can be variadic and must appear as the end of the positional argument specification.";
ParseCommandLine::badvariadic2 = "Invalid argument specification: variadic arguments cannot have a default. If nothing is passed they automatically default to an empty list.";
ParseCommandLine::clfail = "Could not access the command line.";
ParseCommandLine::failcheck = "`` argument `` was parsed to ``, which is an invalid value. Documentation string for the argument is: \"``\".";
ParseCommandLine::nomatch = "`` argument `` was ``, which is an invalid value. Documentation string for the argument is: \"``\".";
ParseCommandLine::nostring = "Provided argument list was not a list of strings.";
ParseCommandLine::poslen = "`` positional arguments were passed but specification requires between `` and ``. " <> $helpMsg;
ParseCommandLine::unkopt = "Unknown options ``. " <> $helpMsg;

End[];
EndPackage[];