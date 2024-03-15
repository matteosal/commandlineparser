parserPath = FileNameJoin @ {
	ParentDirectory @ DirectoryName[$InputFileName],
	"CommandLineParser"
};
PacletDirectoryLoad[parserPath];
Get["CommandLineParser`"];

posArgSpec = {
    "my-string-arg" -> StringSpec["A generic string argument"],
    {"my-symbol-arg", "none"} -> EnumSpec[{None, Automatic, Inherited}, 
        "A special symbol. Can be None, Automatic, Inherited or their lower-case equivalents"
    ],
    "my-variadic-numeric-arg" -> NumericSpec["Integer",
        "Any sequence of positive integers, including none", "Interval" -> {0, Infinity},
        "Variadic" -> True
    ]
};
optArgSpec = {
    {"my-numeric-opt", "0.5"} -> NumericSpec["Real", "A real number between 0 and 1", 
        "Interval" -> {0, 1}],
    {"my-boolean-opt", "false"} -> BooleanSpec["A boolean"],
    "my-variadic-string-opt" -> StringSpec["A variadic string argument", "Variadic" -> True]
};
helpHeader = "This is a script that does something.";

ParseCommandLine[{posArgSpec, optArgSpec, helpHeader}];