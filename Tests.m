(**************************************************)
(********************* HELPERS ********************)
(**************************************************)

makeArgSpecs[pos_, opt_] := {makeSpecsWithPrefix["pos", pos], makeSpecsWithPrefix["opt", opt]}

makeSpecsWithPrefix[prefix_, shortSpecs_] := 
	MapIndexed[makeSpec[#1, First @ #2, prefix]&, shortSpecs]

makeSpec[spec_, id_, prefix_] /; Length[spec] == 5 := Rule[
	StringJoin[prefix, "-", ToString[id]],
	spec
]
makeSpec[specWithDefault_, id_, prefix_] /; Length[specWithDefault] == 2 := Rule[
	{StringJoin[prefix, "-", ToString[id]], Last @ specWithDefault},
	First[specWithDefault]
]

makeAssoc[prefix_, values_] := AssociationThread[
	Table[prefix <> "-" <> ToString[i], {i, Length[values]}],
	values
]

makeTest[testId_, specs_, input_, {parsedPosArgs_, parsedOpts_}] := VerificationTest[
	ParseCommandLine[specs, input],
	{makeAssoc["pos", parsedPosArgs], makeAssoc["opt", parsedOpts]},
	TestID -> testId
]

makeFailureTest[testId_, specs_, input_, msg_] := VerificationTest[
	CheckAbort[ParseCommandLine[specs, input], CommandLineParserAborted],
	CommandLineParserAborted,
	MessageName[ParseCommandLine, msg],
	TestID -> testId
]

(**************************************************)
(****************** SINGLE SPECS ******************)
(**************************************************)

boolSpec1 = BooleanSpec[""];
enumSpec1 = EnumSpec[{None, Automatic, Inherited}, ""];
enumSpec2 = EnumSpec[{False, "Dog", 4}, ""];
numSpec1 = NumericSpec["Real", "", "Interval" -> {0, 1}];
numSpec2 = NumericSpec["Integer", "", "AllowInfinity" -> True];
repeatedSpec1 = RepeatedSpec[numSpec1, ",", ""];
repeatedSpec2 = RepeatedSpec[enumSpec2, "|", ""];
stringSpec1 = StringSpec[""];

varNumSpec1 = NumericSpec["Real", "", "Interval" -> {0, 1}, "Variadic" -> True];

(**************************************************)
(********************* TESTS **********************)
(**************************************************)

(* Basic specs *)

specs = makeArgSpecs[
	{stringSpec1, enumSpec1, numSpec1}, 
	{{numSpec2, "4"}, {boolSpec1, "false"}}
];

makeTest["Basic-1", specs, {"hello", "none", "0.4"}, {{"hello", None, 0.4}, {4, False}}];
makeTest["Basic-2", specs, 
	{"hello", "inherited", "0.97", "--opt-1=-12"}, 
	{{"hello", Inherited, 0.97}, {-12, False}}
];
makeTest["Basic-3", specs, 
	{"goodbye", "automatic", "0.97","--opt-1=infinity", "--opt-2=true"}, 
	{{"goodbye", Automatic, 0.97}, {Infinity, True}}
];
makeTest["Basic-4", specs, 
	{"x", "Automatic", "0.97", "--opt-1=-infinity", "--opt-2"}, 
	{{"x", Automatic, 0.97}, {-Infinity, True}}
];

specs2 = makeArgSpecs[{}, {{numSpec2, "4"}, {boolSpec1, "false"}}];

makeTest["Basic-5", specs2, {}, {{}, {4, False}}];
makeTest["Basic-6", specs2, {"--opt-1=-infinity", "--opt-2"}, {{}, {-Infinity, True}}];

makeFailureTest["BasicFailure-1", specs, {}, "poslen"];
makeFailureTest["BasicFailure-2", specs, {"a", "b", "c", "d"}, "poslen"];
makeFailureTest["BasicFailure-3", specs, {"x", "something", "0.97"}, "nomatch"];
makeFailureTest["BasicFailure-4", specs, {"x", "Automatic", "1.97"}, "failcheck"];
makeFailureTest["BasicFailure-5", specs, {"x", "Automatic", "0.97", "--boh"}, "unkopt"];
makeFailureTest["BasicFailure-6", specs, {"x", "Automatic", "0.97", "--opt-2", "-opt-1"}, "badopts"];
makeFailureTest["BasicFailure-7", specs, boh, "nostring"]
makeFailureTest["BasicFailure-8", {x, y}, {}, "badspec"]


(* Repeated specs *)

specs = makeArgSpecs[{repeatedSpec1}, {{repeatedSpec2, "dog|false"}}];

makeTest["Repeated-1", specs, {"0.1,0.2"}, {{{0.1, 0.2}}, {{"Dog", False}}}];
makeTest["Repeated-2", specs, {"0.1", "--opt-1=4"}, {{{0.1}}, {{4}}}];
makeTest["Repeated-3", specs, 
	{"0.1,0.65,0.82", "--opt-1=4|Dog|Dog"}, 
	{{{0.1, 0.65, 0.82}}, {{4, "Dog", "Dog"}}}
];
makeTest["Repeated-4", specs, {",", "--opt-1="}, {{{}}, {{}}}];
makeTest["Repeated-5", specs, {",", "--opt-1=|"}, {{{}}, {{}}}];

makeFailureTest["RepeatedFailure-1", specs, {"0.1|0.2"}, "nomatch"];
makeFailureTest["RepeatedFailure-2", specs, {"0.1", "--opt-1=,"}, "nomatch"];

(* Optional positional arguments *)

specs = makeArgSpecs[
	{boolSpec1, enumSpec2, {numSpec2, "-75"}, {boolSpec1, "true"}}, 
	{{stringSpec1, "default"}}
];

makeTest["OptionalPositional-1", specs, 
	{"true", "false"}, 
	{{True, False, -75, True}, {"default"}}
];
makeTest["OptionalPositional-2", specs, 
	{"true", "dog", "14"}, 
	{{True, "Dog", 14, True}, {"default"}}
];
makeTest["OptionalPositional-3", specs, 
	{"true", "Dog", "Infinity", "false"}, 
	{{True, "Dog", Infinity, False}, {"default"}}
];
makeTest["OptionalPositional-4", specs, 
	{"true", "4", "-Infinity", "True", "--opt-1=spaceship"}, 
	{{True, 4, -Infinity, True}, {"spaceship"}}
];
makeTest["OptionalPositional-5", specs, 
	{"true", "4", "-Infinity", "--opt-1=spaceship"}, 
	{{True, 4, -Infinity, True}, {"spaceship"}}
];

makeFailureTest["OptionalPositionalFailure-1", specs, {"true"}, "poslen"];
makeFailureTest["OptionalPositionalFailure-2", specs, {"1", "2", "3", "4", "5"}, "poslen"];
makeFailureTest["OptionalPositionalFailure-3", {{{numSpec2, "-75"}, {boolSpec1}}, {}},
	"badposdef"];

(* Variadic positional argument *)

specs = makeArgSpecs[{enumSpec1, varNumSpec1}, {}];

makeTest["VariadicPositional-1", specs, {"none", "0.4"}, {{None, {0.4}}, {}}];
makeTest["VariadicPositional-2", specs, 
	{"none", "0.4", "0.2", "0.12"},
	{{None, {0.4, 0.2, 0.12}}, {}}
];
makeTest["VariadicPositional-3", specs, {"none"}, {{None, {}}, {}}];

makeFailureTest["VariadicPositionalFailure-1", specs, {"none", "4", "2.5"}, "failcheck"]
makeFailureTest["VariadicPositionalFailure-2", 
	makeArgSpecs[{varNumSpec1, enumSpec1}, {}], 
	{}, 
	"badvariadic1"
];
makeFailureTest["VariadicPositionalFailure-3", 
	makeArgSpecs[{varNumSpec1, varNumSpec1}, {}], 
	{}, 
	"badvariadic1"
];
makeFailureTest["VariadicPositionalFailure-4", 
	makeArgSpecs[{enumSpec1, {varNumSpec1, "4"}}, {}], 
	{}, 
	"badvariadic2"
];

(* Optional positional argument + Variadic positional argument *)

specs = makeArgSpecs[{{numSpec2, "-75"}, {boolSpec1, "true"}, varNumSpec1}, {}];

makeTest["OptionalPositionalVariadicPositional-1", specs, 
	{"42", "False", "0.4", "0.94"},
	{{42, False, {0.4, 0.94}}, {}}
];
makeTest["OptionalPositionalVariadicPositional-2", specs, 
	{"42", "False"}, 
	{{42, False, {}}, {}}
];
makeTest["OptionalPositionalVariadicPositional-3", specs, {"42"}, {{42, True, {}}, {}}];
makeTest["OptionalPositionalVariadicPositional-4", specs, {}, {{-75, True, {}}, {}}];

makeFailureTest["OptionalPositionalVariadicPositionalFailure-1", 
	makeArgSpecs[{varNumSpec1, {numSpec2, "-75"}}, {}], 
	{}, 
	"badvariadic1"
];
