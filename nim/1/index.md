[Nim](http://nim-lang.org/) is a young and exciting imperative programming
language that is nearing its 1.0 release. My main motivation for using Nim is
its performance / productivity ratio and the joy writing in Nim brings. In this
guide I'm going to show you how I start a Nim project.

For this purpose we will write a small interpreter for the [brainfuck
language](https://en.wikipedia.org/wiki/Brainfuck).  While Nim is a practical
language with many interesting features, brainfuck is the opposite: It's
impractical to write in and its features consist of 8 single-character
commands. Still, brainfuck is great for us, since its extreme simplicity makes
it easy to write an interpreter for it. Later we will even write a
high-performance compiler that transforms brainfuck programs into Nim at
compile time. We will put all of this into a nimble package and publish it
online.

## Installation

Installing Nim is straightforward, you can follow the [official
instructions](http://nim-lang.org/download.html). Binaries for Windows are
provided. On other operating systems you can run the `build.sh` script to
compile the generated C code, which should take less than 1 minute on a modern
system.

This brings us to the first interesting fact about Nim: It compiles to C
primarily (C++, ObjectiveC and even JavaScript as well) and then uses the
highly optimizing C compiler of your choice to generate the actual program. You
get to benefit from the mature C ecosystem for free.

If you opt for bootstrapping the [Nim compiler](https://github.com/Araq/Nim),
which is written exclusively in Nim itself, you get to witness the compiler
build itself with a few simple steps (in less than 2 minutes):

```bash
$ git clone https://github.com/Araq/Nim
$ cd Nim
$ git clone --depth 1 https://github.com/nim-lang/csources
$ cd csources && sh build.sh
$ cd ..
$ bin/nim c koch
$ ./koch boot -d:release
```

If you haven't done so already, now is a good time to install `git` as well.
Most nimble packages are available on github, so we will need `git` to get
them. On a Debian based distribution (like Ubuntu) we can install it like this:

```bash
$ sudo apt-get install git
```

After you've finished the installation, you should have the `nim` binary in
your path. If you use bash, this is what to do:

```bash
$ export PATH=$PATH:$your_install_dir/bin >> ~/.profile
$ source ~/.profile
$ nim
Nim Compiler Version 0.10.2 (2014-12-29) [Linux: amd64]
Copyright (c) 2006-2014 by Andreas Rumpf
::

    nim command [options] [projectfile] [arguments]

Command:
  compile, c                compile project with default code generator (C)
  doc                       generate the documentation for inputfile
  doc2                      generate the documentation for the whole project
  i                         start Nim in interactive mode (limited)
...
```

If `nim` reports its version and usage, we're good to continue. Now the modules
from [Nim's standard library](http://nim-lang.org/lib.html) are just an import
away. All other packages can be retrieved with
[nimble](https://github.com/nim-lang/nimble), Nim's package manager. Let's
follow the [simple installation
instructions](https://github.com/nim-lang/nimble#nimble). Again, for Windows a
[prebuilt archive](https://github.com/nim-lang/nimble/releases) is available,
while building from source is quite comfortable as well:

```bash
$ git clone https://github.com/nim-lang/nimble
$ cd nimble
$ nim c -r src/nimble install
```

Nimble's binary directory wants to be added to your path as well:

```bash
$ export PATH=$PATH:$HOME/.nimble/bin >> ~/.profile
$ source ~/.profile
$ nimble update
Downloading package list from https://github.com/nim-lang/packages/raw/master/packages.json
Done.
```

Now we can browse the available [nimble
packages](http://nim-lang.org/lib.html#nimble) or search for them on the
command line:

```bash
$ nimble search docopt
docopt:
  url:         git://github.com/docopt/docopt.nim (git)
  tags:        commandline, arguments, parsing, library
  description: Command-line args parser based on Usage message
  license:     MIT
  website:     https://github.com/docopt/docopt.nim
```

Let's install this nice [docopt library](https://github.com/docopt/docopt.nim)
we found before, maybe we'll need it later:

```bash
$ nimble install docopt
...
docopt installed successfully.
```

Notice how quickly the library is installed (less than 1 second for me). This
is another nice effect of Nim. Basically the source code of the library is just
downloaded, nothing resembling a shared library is compiled. Instead the
library will simply be compiled statically into our program once we use it.

There is [editor support](https://github.com/Araq/Nim/wiki/Editor-Support) for
most of the popular editors out there, like Emacs
([nim-mode](https://github.com/reactormonk/nim-mode)), Vim
([nimrod.vim](https://github.com/zah/nimrod.vim/), my choice) and Sublime
([NimLime](https://github.com/Varriount/NimLime)). For the scope of this guide
any text editor will do.

## Project Setup

Now we're ready to get our project started:

```bash
$ mkdir brainfuck
$ cd brainfuck
```

First step: To get `Hello World` on the terminal, we create a `hello.nim`
with the following content:

```nimrod
echo "Hello World"
```

We compile the code and run it, first in two separate steps:

```bash
$ nim c hello
$ ./hello
Hello World
```

Then in a single step, by instructing the Nim compiler to conveniently run the
resulting binary immediately after creating it:

```bash
$ nim c -r hello
Hello World
```

Let's make our code do something slightly more complicated, that should take a
bit longer to run:

```nimrod
var x = 0
for i in 1 .. 100_000_000:
  inc x

echo "Hello World ", x
```

Now we're initializing the variable `x` to 0 and increase it by 1 a whole 100
million times. Try to compile and run it again. Notice how long it takes to run
now. Is Nim's performance that abysmal? Of course not, quite the opposite!
We're just currently building the binary in full debug mode, adding checks for
integer overflows, array out of bounds and much more, as well as not optimizing
the binary at all. The `-d:release` switch allows us to switch into release
mode, giving us full speed:

```bash
$ nim c hello
$ ./hello
Hello World 100000000
./hello  2.01s user 0.00s system 99% cpu 2.013 total
$ nim -d:release c hello
$ ./hello
Hello World 100000000
./hello  0.00s user 0.00s system 74% cpu 0.002 total
```

That's a bit too fast actually. The C compiler optimized away the entire `for`
loop.

To start a new project `nimble init` can generate a basic package config file:

```bash
$ nimble init brainfuck
```
The newly created `brainfuck.nimble` should look like this:

```
[Package]
name          = "brainfuck"
version       = "0.1.0"
author        = "Anonymous"
description   = "New Nimble project for Nim"
license       = "BSD"

[Deps]
Requires: "nim >= 0.10.0"
```

Let's add our name as the actual author, description as well as the requirement
for docopt, as described in [nimble's developers
  info](https://github.com/nim-lang/nimble/blob/master/developers.markdown).
  Most importantly, let's set the binary we want to create:

```
[Package]
name          = "brainfuck"
version       = "0.1.0"
author        = "The 'How I Start Nim' Team"
description   = "A brainfuck interpreter"
license       = "MIT"

bin           = "brainfuck"

[Deps]
Requires: "nim >= 0.10.0, docopt >= 0.1.0"
```

Since we have git installed already, we'll want to keep revisions of our source
code and may want to publish them online at some point, let's initialize a git
repository:

```bash
$ git init
$ git add hello.nim brainfuck.nimble .gitignore
```

Where I just initialized the `.gitignore` file to this:

```
nimcache/
*.swp
```

We tell git to ignore vim's swap files, as well as the `nimcache` directory
that contains the generated C code for our project. Check it out if you're
curious how Nim compiles to C.

To see what nimble can do, let's initialize `brainfuck.nim`, our main program:

```nimrod
echo "Welcome to brainfuck"
```

We could compile it as we did before for `hello.nim`, but since we already set
our package up to include the `brainfuck` binary, let's make `nimble` do the
work:

```bash
$ nimble build
Looking for docopt (>= 0.1.0)...
Dependency already satisfied.
Building brainfuck/brainfuck using c backend...
...
$ ./brainfuck
Welcome to brainfuck
```

`nimble install` can be used to install the binary on our system, so that we can run it from anywhere:

```bash
$ nimble install
...
brainfuck installed successfully.
$ brainfuck
Welcome to brainfuck
```

This is great for when the program works, but `nimble build` actually does a
release build for us. That takes a bit longer than a debug build, and leaves
out the checks which are so important during development, so `nim c -r
brainfuck` will be a better fit for now. Feel free to execute our program quite
often during development to get a feeling for how everything works.

## Coding

While programming Nim's [documentation](http://nim-lang.org/documentation.html)
comes in handy. If you don't know where to find what yet, there's a
[documentation index](http://nim-lang.org/theindex.html), in which you can
search.

Let's start developing our interpreter by changing the `brainfuck.nim` file:

```nimrod
import os
```

First we import the [os module](http://nim-lang.org/os.html), so that we can
read command line arguments.

```nimrod
let code = if paramCount() > 0: readFile paramStr(1)
           else: readAll stdin
```

`paramCount()` tells us about the number of command line arguments passed to
the application. If we get a command line argument, we assume it's a filename,
and read it in directly with `readFile paramStr(1)`. Otherwise we read
everything from the standard input. In both cases, the result is stored in the
`code` variable, which has been declared immutable with the `let` keyword.

To see if this works, we can `echo` the code:

```nimrod
echo code
```

And try it out:

```
$ nim c -r brainfuck
...
Welcome to brainfuck
I'm entering something here and it is printed back later!
I'm entering something here and it is printed back later!
```

After you've entered your "code" finish up with a newline and ctrl-d. Or you
can pass in a filename, everything after `nim c -r brainfuck` is passed as
command line arguments to the resulting binary:

```
$ nim c -r brainfuck .gitignore
...
Welcome to brainfuck
nimcache/
*.swp
```

On we go:

```nimrod
var
  tape = newSeq[char]()
  codePos = 0
  tapePos = 0
```

We declare a few variables that we'll need. We have to remember our current
position in the `code` string (`codePos`) as well as on the `tape` (`tapePos`).
Brainfuck works on an infinitely growing `tape`, which we represent as a `seq`
of `char`s. Sequences are Nim's dynamic length arrays, other than with `newSeq`
they can also be initialized using `var x = @[1, 2, 3]`.

Let's take a moment to appreciate that we don't have to specify the type of our
variables, it is automatically inferred. If we wanted to be more explicit, we
could do so:

```nimrod
var
  tape: seq[char] = newSeq[char]()
  codePos: int = 0
  tapePos: int = 0
```

Next we write a small procedure, and call it immediately afterwards:

```nimrod
proc run(skip = false): bool =
  echo "codePos: ", codePos, " tapePos: ", tapePos

discard run()
```

There are a few things to note here:

- We pass a `skip` parameter, initialized to `false`.
- Obviously the parameter must be of type `bool` then.
- The return type is `bool` as well, but we return nothing? Every result is
  initialized to binary 0 by default, meaning we return `false`.
- We can use the implicit `result` variable in every proc and set `result = true`.
- Control flow can be changed by using `return true` to return immediately.
- We have to explicitly `discard` the returned bool value when calling `run()`.
  Otherwise the compiler complains with `brainfuck.nim(16, 3) Error: value of
  type 'bool' has to be discarded`. This is to prevent us from forgetting to
  handle the result.

Before we continue, let's think about how brainfuck works. This may look
familiar if you've encountered Turing machines before. We have an input string
`code` and a `tape` of chars that can grow infinitely in one direction. These
are the 8 commands that can occur in the input string:

| Op  | Meaning                                                               | Nim equivalent                   |
|:---:| --------------------------------------------------------------------- | -------------------------------- |
| `>` | move right on tape                                                    | `inc tapePos`                    |
| `<` | move left on tape                                                     | `dec tapePos`                    |
| `+` | increment value on tape                                               | `inc tape[tapePos]`              |
| `-` | decrement value on tape                                               | `dec tape[tapePos]`              |
| `.` | output value on tape                                                  | `stdout.write tape[tapePos]`     |
| `,` | input value to tape                                                   | `tape[tapePos] = stdin.readChar` |
| `[` | if value on tape is `\0`, jump forward to command after matching `]`  |                                  |
| `]` | if value on tape is not `\0`, jump back to command after matching `[` |                                  |

With this alone, brainfuck is one of the simplest Turing complete programming
languages.

The first 6 commands can easily be converted into a case distinction in Nim:

```nimrod
proc run(skip = false): bool =
  case code[codePos]
  of '+': inc tape[tapePos]
  of '-': dec tape[tapePos]
  of '>': inc tapePos
  of '<': dec tapePos
  of '.': stdout.write tape[tapePos]
  of ',': tape[tapePos] = stdin.readChar
  else: discard
```

We are handling a single character from the input so far, let's make this a
loop to handle them all:

```nimrod
proc run(skip = false): bool =
  while tapePos >= 0 and codePos < code.len:
    case code[codePos]
    of '+': inc tape[tapePos]
    of '-': dec tape[tapePos]
    of '>': inc tapePos
    of '<': dec tapePos
    of '.': stdout.write tape[tapePos]
    of ',': tape[tapePos] = stdin.readChar
    else: discard

    inc codePos
```

Let's try a simple program, like this:

```bash
$ echo ">+" | nim -r c brainfuck
Welcome to brainfuck
Traceback (most recent call last)
brainfuck.nim(26)        brainfuck
brainfuck.nim(16)        run
Error: unhandled exception: index out of bounds [IndexError]
Error: execution of an external program failed
```

What a shocking result, our code crashes! What did we do wrong? The tape is
supposed to grow infinitely, but we haven't increased it's size at all! That's
an easy fix right above the `case`:

```nimrod
    if tapePos >= tape.len:
      tape.add '\0'
```

The last 2 commands, `[` and `]` form a simple loop. We can encode them into
our code as well:

```nimrod
proc run(skip = false): bool =
  while tapePos >= 0 and codePos < code.len:
    if tapePos >= tape.len:
      tape.add '\0'

    if code[codePos] == '[':
      inc codePos
      let oldPos = codePos
      while run(tape[tapePos] == '\0'):
        codePos = oldPos
    elif code[codePos] == ']':
      return tape[tapePos] != '\0'
    elif not skip:
      case code[codePos]
      of '+': inc tape[tapePos]
      of '-': dec tape[tapePos]
      of '>': inc tapePos
      of '<': dec tapePos
      of '.': stdout.write tape[tapePos]
      of ',': tape[tapePos] = stdin.readChar
      else: discard

    inc codePos
```

If we encounter a `[` we recursively call the `run` function itself, looping
until the corresponding `]` lands on a `tapePos` that doesn't have `\0` on the
tape.

And that's it. We have a working brainfuck interpreter now. To test it, we
create an `examples` directory containing these 3 files:
[helloworld.b](examples/helloworld.b), [rot13.b](examples/rot13.b),
[mandelbrot.b](examples/mandelbrot.b).

```bash
$ nim -r c brainfuck examples/helloworld.b
Welcome to brainfuck
Hello World!
$ ./brainfuck examples/rot13.b
Welcome to brainfuck
You can enter anything here!
Lbh pna ragre nalguvat urer!
ctrl-d
$ ./brainfuck examples/mandelbrot.b
```
![](/static/images/nim/1/mandelbrot.png)

With the last one you will notice how slow our interpreter is. Compiling with
`-d:release` gives a nice speedup, but still takes about 90 seconds on my
machine to draw the Mandelbrot set. For a great speedup, later we will compile
brainfuck to Nim instead of interpreting it. Nim's metaprogramming capabilities
are great for this.

But let's keep it simple for now. Our interpreter is working, now we can turn
our work into a reusable library. All we have to do is surround our code with a
big `proc`:

```nimrod
proc interpret*(code: string) =
  var
    tape = newSeq[char]()
    codePos = 0
    tapePos = 0

  proc run(skip = false): bool =
    ...

  discard run()

when isMainModule:
  import os

  echo "Welcome to brainfuck"

  let code = if paramCount() > 0: readFile paramStr(1)
             else: readAll stdin

  interpret code
```

Note that we also added a `*` to the proc, which indicates that it is exported
and can be accessed from outside of our module. Everything else is hidden.

At the end we still kept the code for our binary. `when isMainModule` ensures
that this code is only compiled when this module is the main one. After a quick
`nimble install` our brainfuck library can be used from anywhere on your
system, just like this:

```nimrod
import brainfuck
interpret "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
```

We're pretty proud of the work we've done so far and should share this code
with others. But before we can do that, let's not forget to document it:

```nimrod
proc interpret*(code: string) =
  ## Interprets the brainfuck `code` string, reading from stdin and writing to
  ## stdout.
  ...
```

`nim doc brainfuck` builds the documentation, which you can [see
online](http://hookrace.net/nim-brainfuck/brainfuck.html) in its full glory.

So that others can use our library easily, we upload it on github
or bitbucket and create a pull request to have it included in the [nimble
packages](https://github.com/nim-lang/packages).

## Metaprogramming

As I said before, our interpreter is still pretty slow for the mandelbrot
program. Let's write a procedure that creates a Nim code AST at compiletime
instead:

```nimrod
import macros

proc compile(code: string): PNimrodNode {.compiletime.} =
  var stmts = @[newStmtList()]

  template addStmt(text): stmt =
    stmts[stmts.high].add parseStmt(text)

  addStmt "var tape: array[1_000_000, char]"
  addStmt "var codePos = 0"
  addStmt "var tapePos = 0"

  for c in code:
    case c
    of '+': addStmt "inc tape[tapePos]"
    of '-': addStmt "dec tape[tapePos]"
    of '>': addStmt "inc tapePos"
    of '<': addStmt "dec tapePos"
    of '.': addStmt "stdout.write tape[tapePos]"
    of ',': addStmt "tape[tapePos] = stdin.readChar"
    of '[': stmts.add newStmtList()
    of ']':
      var loop = newNimNode(nnkWhileStmt)
      loop.add parseExpr("tape[tapePos] != '\\0'")
      loop.add stmts.pop
      stmts[stmts.high].add loop
    else: discard

  result = stmts[0]
  echo result.repr
```

The template `addStmt` is just there to reduce boilerplate. We could also
explicitly write the same operation at each position that currently uses
`addStmt`. `parseStmt` turns a piece of Nim code into its corresponding AST,
which we store in a list.

Most of the code is similar to the interpreter, except we're not executing the
code now, but generating it, and adding it to a list of statements. `[` and `]`
are more complicated: They get translated into a while loop surrounding the
code inbetween.

We're cheating a bit here because we use a fixed size `tape` now and don't
check for under- and overflows anymore. To see what this code does, the last
line, namely `echo result.repr` prints the Nim code we generated.

Try it out by calling:

```nimrod
static: discard compile "+>+[-]>,."
```

During compilation the generated code is printed:

```nimrod
var tape: array[1000000, char]
var codePos = 0
var tapePos = 0
inc tape[tapePos]
inc tapePos
inc tape[tapePos]
while tape[tapePos] != '\0': 
  dec tape[tapePos]
inc tapePos
tape[tapePos] = stdin.readChar
stdout.write tape[tapePos]
```

Generally useful for writing macros is the `dumpTree` macro, which prints the
AST of a piece of code, for example:

```nimrod
import macros

dumpTree:
  while tape[tapePos] != '\0':
    inc tapePos
```

This shows us the following Tree:

```
StmtList
  WhileStmt
    Infix
      Ident !"!="
      BracketExpr
        Ident !"tape"
        Ident !"tapePos"
      CharLit 0
    StmtList
      Command
        Ident !"inc"
        Ident !"tapePos"
```

That's how I knew that we would need a `StmtList`, for example.

Macros can be used to insert the generated code into a program directly:

```nimrod
macro compileString*(code: string): stmt =
  ## Compiles the brainfuck `code` string into Nim code that reads from stdin
  ## and writes to stdout.
  compile code.strval

macro compileFile*(filename: string): stmt =
  ## Compiles the brainfuck code read from `filename` at compile time into Nim
  ## code that reads from stdin and writes to stdout.
  compile staticRead(filename.strval)
```

We can now compile the mandelbrot program into Nim easily:

```nimrod
proc mandelbrot = compileFile "examples/mandelbrot.b"

mandelbrot()
```

Compiling with full optimizations takes quite long now (about 4 seconds),
because the mandelbrot program is huge and GCC needs time to optimize it, but
instead the program runs in just 1 second:

```
$ nim -d:release c brainfuck
$ ./brainfuck
```

## Using the C backend for more

By default Nim compiles the C code with GCC, but clang usually compiles faster
and may even yield more efficient code. It's always worth a try. To compile
once with clang, use `nim -d:release --cc:clang c hello`. If you want to keep
compiling with clang, create a `hello.nim.cfg` file with the content `cc =
clang`. To change the default backend compiler, edit `config/nim.cfg` in Nim's
directory.

Another advantage is that we can use debuggers with C support, like GDB. Simply
compile your program with `nim c --linedir:on --debuginfo c hello` and `gdb
./hello` can be used to debug your program.

## Command line argument parsing

So far we've been parsing the command line argument by hand. Since we already
installed the [docopt.nim](https://github.com/docopt/docopt.nim) library
before, we can use it now:

```nimrod
when isMainModule:
  import docopt, tables, strutils

  proc mandelbrot = compileFile("examples/mandelbrot.b")

  let doc = """
brainfuck

Usage:
  brainfuck mandelbrot
  brainfuck interpret [<file.b>]
  brainfuck (-h | --help)
  brainfuck (-v | --version)

Options:
  -h --help     Show this screen.
  -v --version  Show version.
"""

  let args = docopt(doc, version = "brainfuck 1.0")

  if args["mandelbrot"]:
    mandelbrot()

  elif args["interpret"]:
    let code = if args["<file.b>"]: readFile($args["<file.b>"])
               else: readAll stdin

    interpret(code)
```

The nice thing about docopt is that the documentation is also the
specification. Pretty simple to use:

```bash
$ nimble install
...
brainfuck installed successfully.
$ brainfuck -h
brainfuck

Usage:
  brainfuck mandelbrot
  brainfuck interpret [<file.b>]
  brainfuck (-h | --help)
  brainfuck (-v | --version)

Options:
  -h --help     Show this screen.
  -v --version  Show version.
$ brainfuck interpret examples/helloworld.b
Hello World!
```

The full program and library we created are [available on
Github](https://github.com/def-/nim-brainfuck/).

## Conclusion

This is the end of our tour through the Nim ecosystem, I hope you enjoyed it
and found this as interesting as it was to write for me.

If you still want to learn more about Nim, I have written about [what is
special about Nim](http://hookrace.net/blog/what-is-special-about-nim/) and
[what makes Nim practical](http://hookrace.net/blog/what-makes-nim-practical/).

If you're interested in a more traditional start into Nim, the [official
tutorial](http://nim-lang.org/tut1.html) and [Nim by
Example](https://nim-by-example.github.io/) can guide you.

The [Nim community](http://nim-lang.org/community.html) is very welcoming and
helpful.
