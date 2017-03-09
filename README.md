[![Build Status](https://travis-ci.org/morganey-lang/Morganey.svg?branch=master)](https://travis-ci.org/morganey-lang/Morganey)
[![Build status](https://ci.appveyor.com/api/projects/status/8gdrv2hsu2xd3vir/branch/master?svg=true)](https://ci.appveyor.com/project/rexim/morganey/branch/master)

# Morganey [![Status Ventis](https://img.shields.io/badge/status-ventis-yellow.svg)](https://github.com/ForNeVeR/andivionian-status-classifier)

[Lambda Calculus][wiki-lambda-calculus] based, interpreted programming
language that recognizes Church encoded structures.

## Usage ##

Before doing anything useful with Morganey you need to install
[sbt][scala-sbt] first.

### Samples ###

For language samples and examples please visit [docs/samples/][samples-dir] directory. To execute a sample run:

```console
$ sbt "run <path-to-sample>"
```

from the root of the source code directory. For example, to execute the 15 first fibonacci numbers sample run:

```console
$ sbt "run docs/samples/fib.mgn"
```

For more information on executing Morganey programs please see [Program Execution](#program-execution) section.

### REPL ###

To run the REPL just enter the following in the source code directory

```console
$ sbt run
```

and start entring REPL commands there. You can enter

- `<term>` (see [Language](#language))
- `<binding>` (see [Language](#language))
- `<loading>` (see [Language](#language))
- `:exit` - exits the REPL
- `:raw <term>` - prints the representation of the term
- `:unbind <regex>` - clears either the current context, or all bindings matching the regex

The REPL will take the entered lambda term, beta-reduce it with the
normal order reduction strategy and output the normal form of the
entered lambda term.

To quit the REPL just `^C` it.

### Program Execution ###

To execute a program written in Morganey do the following:

```console
$ sbt "run main.mgn"
```

Where `main.mgn` is the name of the file that contains the
program. The program should provide an entry point. An entry point is
a binding with the name `main` which is bound to function of one
argument:

    main := \input . ...

For more information see [Execution Semantic][execution-semantic]

### As a Thirdparty Dependency ###

Morganey is available on [JCenter][jcenter].

#### sbt ####

```scala
resolvers += Resolver.jcenterRepo

libraryDependencies ++= Seq(
  "me.rexim" %% "morganey" % "0.1.0"
)
```

### Unit Tests ###

To run the Unit Tests enter the following in the source code directory

```console
$ sbt clean coverage test
$ sbt coverageReport
$ sbt coverageAggregate
```

And after that you can take a look at the Unit Test coverage
results. Just open `target/scala-2.11/scoverage-report/index.html`
with your favorite browser.

### Functional Tests ###

```console
$ sbt funtests
```

### Build an uberjar ###

To build an uberjar run the following command

```console
$ sbt assembly
```

After that find the uberjar at `target/scala-2.11/morganey.jar`

### Custom sbt commands ###

This project introduces the following custom sbt commands:

- `sbt build`: Compiles the subprojects `kernel`, `macros` and
  `stdlib` and copies the resulting JAR files into the target directory
  of the root project.

- `sbt buildModuleIndex`: Traverses the stdlib subproject and creates
  a resource file containing a list of modules - the so-called
  [module-index]. This command is run automatically if you `sbt
  compile` the project.

- `sbt rebuild`: Alias for the execution of the commands `sbt clean`
  and `sbt build`.

- `sbt retest`: Alias for the execution of the commands `sbt rebuild`
  and `sbt test`.

## Language ##

### BNF ###

    <term> ::= <function>
             | <application>
             | <literal>
             | <variable>
             | "(" <term> "," <term> ")"
             | "(" <term> ")"
    <binding> ::= "def" <variable> ":=" <term>
    <loading> ::= "load" <module-path>
    <module-path> := [a-zA-Z][a-zA-Z0-9.]*

    <literal> ::= <numeric-literal>
                | <character-literal>
                | <string-literal>
                | <list-literal>
    <numeric-literal> ::= [0-9]+
    <character-literal> ::= '[\u0020-\u00B0]'
                          | '\\[\\'"bfnrt]'
    <string-literal> ::= <java-string-literal>
    <list-literal> ::= "[" [ term { "," term } ] "]"
                     | "[" <term> [ "," <term> ] ".." <term> "]"
    <number-or-character> ::= <numeric-literal>
                            | <character-literal>

    <variable> ::= [a-zA-Z][a-zA-Z0-9]*
    <function> ::= <lambda-symbol> <variable> "." { <variable> "." } <term>
    <application> ::= <term> <term> { <term> }
    <lambda-symbol> ::= "λ"
                      | "\"

### Comments ###

In Morganey there are two types of comments: single-line (starting
with double slash `//`, extending to the end of the line) and
multi-line comments (any text, surrounded by `/*` and `*/`).

## License ##

Copyright (C) 2015–2016 Codingteam

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

[wiki-lambda-calculus]: https://en.wikipedia.org/wiki/Lambda_calculus
[scala-sbt]: http://www.scala-sbt.org/
[execution-semantic]: https://github.com/rexim/Morganey/wiki/Execution-Mode-Semantic
[samples-dir]: docs/samples/
[jcenter]: https://bintray.com/morganey-lang/maven/morganey
[module-index]: https://github.com/morganey-lang/Morganey/blob/master/docs/MorganeyIndex.md

<!-- waffles eaten: 14 -->
