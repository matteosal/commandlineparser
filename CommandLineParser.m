BeginPackage["CommandLineParser`"];

ParseCommandLine;

Begin["`Private`"];

ParseCommandLine[allSpecs_] := ParseCommandLine[allSpecs, getCommandLineArgs[]]
ParseCommandLine[{pos_, opts_}, args_] := ParseCommandLine[{pos, opts, ""}, args]
ParseCommandLine[spec_, $Failed] := (
	Message[ParseCommandLine::clfail];
	Abort[];
)

ParseCommandLine[{posSpecs_, optSpecs_, helpHeader_}, args_] := Module[
	{optPos, posArgs, optArgs, posParsed, optParsed},
	If[!MatchQ[args], {Repeated[_?StringQ]},
		Message[ParseCommandLine::nostring];
		Abort[];
	];
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

parsePosArgs[specs_, posArgs_] := (
	If[Length[posArgs] =!= Length[specs],
		Message[ParseCommandLine::poslen, Length[posArgs], Length[specs]];
		Abort[]
	];
	MapThread[parseSingle, {specs, posArgs, Range @ Length[specs]}]
);

parseOptArgs[specs_, optArgs_] := Module[{providedAssoc, unknown},
	providedAssoc = AssociationThread @@ Transpose @ StringReplace[optArgs, 
		{
			"--" ~~ name__ ~~ "=" ~~ val__ :> {name, val}, 
			_ :> (Message[ParseCommandLine::badopts, optArgs]; Abort[];)
		}
	][[All, 1]];
	(* ^ <|optName -> val, ...|> *)
	If[!Developer`EmptyQ[unknown = Complement[Keys @ providedAssoc, specs[[All, 1, 1]]]],
		Message[ParseCommandLine::unkopt, unknown];
		Abort[]
	];
	Association @ Map[
		#[[1, 1]] -> parseSingle[
			MapAt[First, #, 1], 
			Lookup[providedAssoc, #[[1, 1]], #[[1, 2]]],
			None
		]&,
		specs
	]
];

parseSingle[{name_, patt_, parser_, _}, arg_, pos_] := If[StringMatchQ[arg, patt],
	parser[arg],
	Message[ParseCommandLine::nomatch, 
		Replace[pos, {None -> "Optional", _ :> "Positional"}], 
		name <> Replace[pos, {None -> "", n_ :> StringJoin[" (position ", ToString[n], ")"]}], 
		arg, 
		patt
	];
	Abort[]
];

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
		maxNameLen = Max @ StringLength @ posSpecs[[All, 1]];
		Print["Positional arguments:"];
		Scan[
			Print @ StringJoin[
				StringPadRight[First[#], maxNameLen],
				"    ",
				Last[#]
			]&, 
			posSpecs
		];
	];
	If[!Developer`EmptyQ[optSpecs],
		maxNameLen = Max @ StringLength @ optSpecs[[All, 1, 1]];
		maxDefLen  = Max @ StringLength @ optSpecs[[All, 1, 2]];
		Print["Optional arguments:"];
		Scan[
			Print @ StringJoin[
				StringPadRight[#[[1, 1]], maxNameLen],
				"    ",
				StringPadRight[#[[1, 2]], maxDefLen],
				"    ",
				Last[#]
			]&, 
			optSpecs
		];
	];
]

$helpMsg = "Add --help for documentation.";
ParseCommandLine::badopts = "Optional arguments were not correctly formatted: ``";
ParseCommandLine::clfail = "Could not access the command line.";
ParseCommandLine::nomatch = "`` argument `` was ``, which doesn't match the associated string pattern ``. " <> $helpMsg;
ParseCommandLine::nostring = "Provided argument list was not a list of strings.";
ParseCommandLine::poslen = "`` positional arguments were passed but specification requires ``. " <> $helpMsg;
ParseCommandLine::unkopt = "Unkown options ``. " <> $helpMsg;

End[];
EndPackage[];