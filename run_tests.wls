asGreen[str_] := "\[RawEscape][92m" <> str <> "\[RawEscape][0m";
asRed[str_] := "\[RawEscape][31m" <> str <> "\[RawEscape][0m";
printReport[id_, mark_] := Print[ToString[id] <> " -> " <> mark]
passedMark = asGreen["\:2713"];
failedMark = asRed["\:00D7"];

reportIfFailure[report_, key_, msg_] := If[report[key] =!= {},
	ids = Map[
		#["TestID"]&,
		Lookup[report["TestResults"], report[key]]
	];
	Print["The following tests " <> msg <> ": " <> ToString[ids]]	
]

parserPath = FileNameJoin @ {DirectoryName[$InputFileName], "CommandLineParser.m"};
Get[parserPath];

testsPath = FileNameJoin @ {DirectoryName[$InputFileName], "Tests.m"};
report = TestReport[testsPath,
	HandlerFunctions -> <|
		"TestEvaluated" -> Function @ If[#Outcome === "Success",
			printReport[#TestObject["TestID"], passedMark]
			,
			printReport[#TestObject["TestID"], failedMark];
			type = #TestObject["FailureType"];
			Print["    Failure Type: " <> type];
			Switch[type,
				"SameTestFailure",
					Print["    Expected output: " <> ToString[#TestObject["ExpectedOutput"]]];
					Print["    Actual output: " <> ToString[#TestObject["ActualOutput"]]],
				"SameMessagesFailure",
					Print["    Expected messages: " <> ToString[#TestObject["ExpectedMessages"]]];
					Print["    Actual messages: " <> ToString[#TestObject["ActualMessages"]]]
			]
		]
	|>
];

Print[];

If[
	And[
		report["TestsFailedWrongResultsKeys"] === {},
		report["TestsFailedWithMessagesKeys"] === {},
		report["TestsNotEvaluatedKeys"] === {}
	],
	Print["All tests passed!"],
	reportIfFailure[report, "TestsFailedWrongResultsKeys", "failed"];
	reportIfFailure[report, "TestsFailedWithMessagesKeys", "produced messages"];
	reportIfFailure[report, "TestsNotEvaluatedKeys", "did not evaluate"];
]
