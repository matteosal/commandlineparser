# CommandLineParser

A small package to parse command line arguments in a Wolfram Language script. It's divided in two parts:

* A very flexible command line parsing logic that allows for arbitrary validation/transformation of provided data using low-level argument specifications
* A set of high-level helpers to generate said low-level specifications for common use cases (numeric arguments, boolean arguments, ...)

In order to give a generic feeling before the actual documentation, here's how a CLI with three positional arguments (the last being variadic) and two optional arguments is set up using the high-level helpers:

```
(* script.m *)
Get["/path/to/CommandLineParser.m"];

posArgSpec = {
	"my-string-arg" -> StringSpec["A generic string argument"],
	"my-symbol-arg" -> EnumSpec[{None, Automatic, Inherited}, 
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
	{"my-boolean-opt", "false"} -> BooleanSpec["A boolean"]
};
helpHeader = "This is a script that does something.";

{posArgs, optArgs} = ParseCommandLine[{posArgSpec, optArgSpec, helpHeader}];
Print[posArgs];
Print[optArgs];
```

And here are some results of running the above script with various arguments:
```
math -script script.m hello inherited 3 5 67 1

<|my-string-arg -> hello, my-symbol-arg -> Inherited, my-variadic-numeric-arg -> {3, 5, 67, 1}|>
<|my-numeric-opt -> 0.5, my-boolean-opt -> False|>
```
```
math -script script.m hello inherited 3 5 67 1 --my-numeric-opt=0.2

<|my-string-arg -> hello, my-symbol-arg -> Inherited, my-variadic-numeric-arg -> {3, 5, 67, 1}|>
<|my-numeric-opt -> 0.2, my-boolean-opt -> False|>
```
```
math -script script.m hello inherited 3 5 67 1 --my-numeric-opt=0.2 --my-boolean-opt=true

<|my-string-arg -> hello, my-symbol-arg -> Inherited, my-variadic-numeric-arg -> {3, 5, 67, 1}|>
<|my-numeric-opt -> 0.2, my-boolean-opt -> True|>
```
```
math -script script.m hello inherited 3 5 67 1 --my-numeric-opt=0.2 --my-boolean-opt

<|my-string-arg -> hello, my-symbol-arg -> Inherited, my-variadic-numeric-arg -> {3, 5, 67, 1}|>
<|my-numeric-opt -> 0.2, my-boolean-opt -> True|>
```

If the special flag `--help` is passed, an auto-generated help message is printed:
```
math -script script.m --help

This is a script that does something.

* Mandatory positional arguments:
NAME             DOCUMENTATION
my-string-arg    A generic string argument
my-symbol-arg    A special symbol. Can be None, Automatic, Inherited or their lower-case equivalents

* Optional positional arguments (must be passed in this order after the mandatory arguments):
NAME                       DEFAULT    DOCUMENTATION
my-variadic-numeric-arg    {}         Any sequence of positive integers, including none

Argument my-variadic-numeric-arg is variadic.

* Optional arguments (must be passed as --name=... in any order):
NAME              DEFAULT    DOCUMENTATION
my-numeric-opt    0.5        A real number between 0 and 1
my-boolean-opt    false      A boolean
```

## Usage

* Main usage is `ParseCommandLine[spec]`, which will automatically try to grab the command line arguments from `$ScriptCommandLine` / `$CommandLine` and parse them according to `spec`.
* Alternatively, `ParseCommandLine[spec, commandLineArgs]` can be used to provide an explicit list of command line arguments to parse as a list of strings (which should only contain the arguments to parse).
* `ParseCommandLine` returns `{parsedPosArgs, parsedOpts}` where `parsedPosArgs` and `parsedOpts` are both associations containing the names and parsed values of positional and optional arguments respectively. `parsedOpts` contains all the option values, including those not specified by the user.
* Argument `spec` has the form `{posArgSpecs, optArgSpecs, helpHeader}` or just `{posArgSpecs, optArgSpecs}`:
    * `posArgSpecs` is a list defining the specification for positional arguments. It has the form `{argName -> argSpec, ...}`, where `argName` is a string. Default values can be specified by using `{argName, default} -> argSpec` instead. Positional arguments without defaults cannot appear after positional arguments with defaults.
    * `optArgSpecs` is a list defining the specification for optional arguments. It has the form `{{argName, default} -> argSpec, ...}`, where `argName` and `default` are both strings. `default` is the mandatory option default.
    * `helpHeader` is used for documentation purposes.
* The command line arguments provided by the user are expected to have the form `pos-arg-1 pos-arg-2 --opt-1=xxx --opt2=yyy`, with all the positional arguments in the correct order first and all the desired optional arguments in any order after. Optional positional arguments can be omitted.
* For optional arguments `--opt-name` is interpreted as `--opt-name=True`
* If the list of arguments contains `--help` in any postion, an auto-generated help message is printed instead of running the script. The message uses the information provided in the argument specs to document all available arguments and options. `helpHeader` is prepended to the message if present.

## Low-level argument specification

* To summarize the previous section, the basic usage is `{parsedPosArgs, parsedOpts} = ParseCommandLine[{posArgSpecs, optArgSpecs}]`:
    * `parsedPosArgs` and `parsedOpts` are both associations containing the names and parsed values of positional and optional arguments respectively.
    * `posArgSpecs` is a list whose elements have the form `argName -> argSpec` or `{argName, default} -> argSpec`, where `argName` and `default` are strings. 
    * `optArgSpecs` is a list whose elements have the form `{argName, default} -> argSpec`, where `argName` and `default` are strings.
* In the low-level argument specification, `argSpec` is a list of the form `{stringPatt, parser, postCheck, variadic, docString}`
    * `stringPatt` is a string pattern that validates the input string from the command line. The input string must `StringMatchQ` with `stringPatt`, otherwise an error is generated.
    * `parser` is a `Function` object that is applied to the input string (after string pattern validation) and produces the desired final expression.
    * `postCheck` is a `Function` object that is applied to the output of the parser and can check its value. It should return `True` in case the check is successful, otherwise an error is generated.
    * `variadic` is a boolean that specifies if the given argument is variadic.
    * `docString` is a documentation string for the argument and its allowed values. It is shown in parsing error messages and in the auto-generated help message (`--help`).

## Variadic arguments

Variadic argument specifications have `True` as their 4th element. This feature fundamentally changes how arguments are treated by the parser. 
* Any number of optional arguments can be variadic but only one positional argument can, and it must appear as the last element in the specification.
* For variadic arguments, `ParseCommandLine` always returns a list of the objects produced by their parser.
* A positional variadic argument is passed from the command line as a sequence of values like `val1 val2 val3`. Optional variadic arguments require the option name to be repeated every time as in `--opt-name=val1 --opt-name=val2 --opt-name=val3`. In both cases `ParseCommandLine` will return the list of objects resulting from mapping the parser on `{val1, val2, val3}`.
* In case nothing is passed to either the positional or optional arguments an empty list will be returned.
* From the above point it follows that variadic arguments cannot have defaults: a specification of the form `{name, default} -> {..., ..., ..., True, ...}` will be rejected.

## High-level argument helpers

High level helpers which create argument specifications are available for the most common cases. These helpers are just functions that output low-level specifications described above.
* `StringSpec[docString]` generates a specification for a string that is taken verbatim from the command line. No transformation or check is performed.
    * `docString` is the documentation string for the argument
    * Can set the option `"Variadic" -> True` to make it variadic
    * Example:
```
(* script.m *)
posArgSpec = {"arg" -> StringSpec["A generic string argument"]};
{posArgs, optArgs} = ParseCommandLine[{posArgSpec, {}}];
Print[posArgs];
```
```
math -script script.m hello
<|arg -> hello|>
```
* `BooleanSpec[docString]` generates a specification for a boolean. The value returned by the parser is either the `True` or `False` symbol, and command line input can be `True`, `False` or their lower-cased equivalents
    * `docString` is the documentation string for the argument
    * Can set the option `"Variadic" -> True` to make it variadic
    * Example:
```
(* script.m *)
posArgSpec = {"arg" -> BooleanSpec["A variadic boolean argument", "Variadic" -> True]};
{posArgs, optArgs} = ParseCommandLine[{posArgSpec, {}}];
Print[posArgs];
```
```
math -script script.m True False true false
<|arg -> {True, False, True, False}|>
```
* `NumericSpec[type, docString]` generates a specification for a number. The value returned by the parser is either a number or `+/- Infinity`.
    * `type` is either `"Real"` or `"Integer"`
    * `docString` is the documentation string for the argument
    * Can set the option `"Interval" -> {a, b}` to specify an allowed interval (endpoints allowed). Can use `Infinity` for unbounded intervals
    * Can set the option `"AllowInfinity" -> True` to actually allow `Infinity` or `-Infinity` as input values (or their lower-cased equivalents). They will be parsed to their equivalent symbolic expressions
    * Can set the option `"Variadic" -> True` to make it variadic
    * Example:
```
(* script.m *)
posArgSpec = {
	"arg" -> NumericSpec["Real", 
		"Zero or more real numbers no larger than 3. -Infinity (or -infinity) is also accepted", 
		"Interval" -> {-Infinity, 3},
		"AllowInfinity" -> True,
		"Variadic" -> True
	]
};
{posArgs, optArgs} = ParseCommandLine[{posArgSpec, {}}];
Print[posArgs];
```
```
math -script script.m 2.5 -47.6 -infinity -1 -Infinity
<|arg -> {2.5, -47.6, -Infinity, -1, -Infinity}|>
```
* `EnumSpec[values, docString]` generates a specification for an argument having values that range on a fixed set of choices. The value returned by the parser is anything appearing in `values`, and lower-cased names are accepted as command line inputs. Essentially, `BooleanSpec` is an `EnumSpec` with values `True` and `False`
    * `values` is the list of desired values. Both symbols and strings can be used
    * `docString` is the documentation string for the argument
    * Can set the option `"Variadic" -> True` to make it variadic
```
posArgSpec = {
	"arg" -> EnumSpec[{"Hello", None, True}, 
		"Can be Hello, None, True or their lower-cased equivalents"]
};
{posArgs, optArgs} = ParseCommandLine[{posArgSpec, {}}];
Print[posArgs];
```
```
math -script script.m hello
<|arg -> Hello|>
```
* `RepeatedSpec[singleSpec, separator, docString]` generates a specification for a list of values each specified by `singleSpec`. It has a similar use to a variadic argument but has the advantage that any number of arguments like this can be specified while only one argument can be variadic. Values must be separated using the `separator` with no spaces, and the elements are collected into a list. It is possible to create an empty list by just passing `separator` as the argument value, or by `--opt-name=` for optional arguments.
    * `singleSpec` is the specification for the individual elements. It can be an explicit low-level specification or it can be generated by other high-level helpers. Each of the provided elements are parsed individually according to `singleSpec`
    * `separator` is a string of one or more characters used to separate individual values. It cannot be a whitespace
    * `docString` is the documentation string for the argument. The documentation string of `singleSpec` is ignored
    * Example:
```
(* script.m *)
posArgSpec = {
	"arg" -> RepeatedSpec[
		EnumSpec[{"Hello", None, True}, ""],
		",",
		"Comma-separated list whose elements are Hello|None|True or their lower-case equivalents"
	]
};
{posArgs, optArgs} = ParseCommandLine[{posArgSpec, {}}];
Print[posArgs];
```
```
math -script script.m hello,true,Hello,None,none,True
<|arg -> {Hello, True, Hello, None, None, True}|>
```