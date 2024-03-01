# CommandLineParser

A simple utility to parse command line arguments in a WL script. 

* Main usage is `ParseCommandLine[spec]`, which will automatically try to grab the command line arguments from `$ScriptCommandLine` / `$CommandLine` and parse them according to `spec`
* Alternatively, `ParseCommandLine[spec, commandLineArgs]` can be used to provide an explicit list of command line arguments to parse as a list of strings (which should only contain the arguments to parse)
* `ParseCommandLine` returns `{parsedPosArgs, parsedOpts}` where `parsedPosArgs` is a list and `parsedOpts` is an association that contains all the option values including those not specified by the user.
* Argument `spec` has the form `{posArgSpecs, optArgSpecs, helpHeader}` where `helpHeader` can be omitted (see below)
* `posArgSpecs` is a list of specs for positional arguments, each having the form `{name, stringPattern, parser, docString}`
    * `name` is only used in the auto-generated help message (see below)
    * `stringPattern` is used to check user input. The user-provided string must `StringMatchQ` with this pattern
    * `parser` is a function that is applied to the user-provided string to produce the final value of the argument returned by `ParseCommandLine`. Its output can be any expression
    * `docString` is used by the auto-generated help message
* `optArgSpecs` is a list of specs for optional arguments, each having the form `{{name, default}, stringPattern, parser, docString}`
    * `name` is the name of the option, user-provided as `--name=val`
    * `default` is the default option value
    * everything else follows what listed for `posArgSpecs`
* User-provided arguments are expected to have the form `pos-arg-1 pos-arg-2 --opt-1=xxx --opt2=yyy`, with all the positional arguments in the correct order first and all the optional arguments in any order after
* If the list of arguments contains `--help` in any postion, an auto-generated help message is printed instead of running the script. The message uses the information provided in `spec` to document all available arguments and options. `helpHeader` is prepended to the message if present

As an example, consider this script.m file (have to set your local path to the package):
```
Get["/home/matteo/Git/commandlineparser/CommandLineParser.m"];

(* {name, stringPatt, parser, docString} *)
posArgSpec = {
	{"pos-arg-1", NumberString, FromDigits, "Doc string for pos arg 1"},
	{"pos-arg-2", "automatic"|"inherited", ToExpression @* Capitalize, 
		"Doc string for pos arg 2"}
};
(* {{name, default}, stringPatt, parser, docString} *)
optsSpec = {
	{{"opt-1", "1"}, NumberString|"infinity",
		Replace[#, {"infinity" -> Infinity, n_ :> FromDigits[n]}]&,
		"Doc string for option 1"
	},
	{{"opt-2", "true"}, "true"|"false", ToExpression @* Capitalize, "Doc string for option 2"}
};
helpHeader = "Help message header.";

{args, opts} = CommandLineParser`ParseCommandLine[{posArgSpec, optsSpec, helpHeader}];
Print[args];
Print[opts];
```
The command `math -script script.m 123 automatic --opt-1=infinity` produces the following:
```
{123, Automatic}
<|opt-1 -> Infinity, opt-2 -> True|>
```
The command `math -script script.m --help` prints the help message instead:
```
Help message header.
Positional arguments:
pos-arg-1    Doc string for pos arg 1
pos-arg-2    Doc string for pos arg 2
Optional arguments:
opt-1    1       Doc string for option 1
opt-2    true    Doc string for option 2
```