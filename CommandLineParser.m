BeginPackage["CommandLineParser`"];

ParseCommandLine;

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
	{posSpecs, optSpecs} = {toSpec @@@ posSpecsRaw, toSpec @@@ optSpecsRaw};
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

Options[toSpec] = {"Parser" -> ToExpression, "PostCheck" -> Function[True]};

toSpec[name_, patt_, doc_, OptionsPattern[]] := Module[{name2, addDefault, default, res},
	If[ListQ[name],
		{name2, default} = name;
		addDefault = True,
		name2 = name;
		addDefault = False
	];
	res = <|"Name" -> name2, "StringPattern" -> patt, "Documentation" -> doc, 
		"Parser" -> OptionValue["Parser"], "PostCheck" -> OptionValue["PostCheck"]|>;
	If[addDefault,
		Append[res, "Default" -> default],
		res
	]
]

parsePosArgs[specs_, posArgs_] := (
	If[Length[posArgs] =!= Length[specs],
		Message[ParseCommandLine::poslen, Length[posArgs], Length[specs]];
		Abort[]
	];
	MapThread[parseSingle, {specs, posArgs, Range @ Length[specs]}]
);

parseOptArgs[specs_, optArgs_] := Module[{provided, providedAssoc, unknown},
	provided = Transpose @ StringReplace[optArgs, 
		{
			"--" ~~ name__ ~~ "=" ~~ val__ :> {name, val}, 
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

$helpMsg = "Add --help for documentation.";
ParseCommandLine::badopts = "Optional arguments were not correctly formatted: ``";
ParseCommandLine::clfail = "Could not access the command line.";
ParseCommandLine::failcheck = "`` argument `` was parsed to ``, which is an invalid value. Documentation string for the argument is: \"``\"";
ParseCommandLine::nomatch = "`` argument `` was ``, which is an invalid value. Documentation string for the argument is: \"``\"";
ParseCommandLine::nostring = "Provided argument list was not a list of strings.";
ParseCommandLine::poslen = "`` positional arguments were passed but specification requires ``. " <> $helpMsg;
ParseCommandLine::unkopt = "Unkown options ``. " <> $helpMsg;

End[];
EndPackage[];