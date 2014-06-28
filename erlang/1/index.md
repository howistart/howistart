## Intro

Erlang releases are a bit like magnets. Everyone who thinks about them shares
the same thought: f**king releases, how do they work?

Fortunately, since the years of Emakefiles, reltool and systools, the Erlang
community has stood up and improved its tooling continuously.

Rebar has been improving non-stop and keeps getting better for many functions.
It does depend on Reltool for releases, and despite its power, Reltool is hardly
usable.

That's why Relx came out after the fact. It's a tool to help build releases
easily.

Along with [installing Erlang](https://www.erlang-solutions.com/downloads)
(version R16B03-1 at least), getting a hold of rebar and Relx is all you're
gonna need.

For rebar, just [download it and follow the
instructions](https://github.com/rebar/rebar/#downloading). Rebar will
basically generate a self-executable that you can store in your repository, or
install globally on your computer. This tutorial expects that you're using a
recent version (>= 2.2.0), older ones execute commands differently and will
explode in your face.

In the case of [relx](http://relx.org/), its presence isn't nearly as pervasive
for Erlang users, so I tend to still include it in repositories I ship it with
for people who want to build it.

Once they're all installed somewhere in your system, arm yourself with the text
editor or IDE of your choice (mine is Vim, because I'm a terrible person) and
get ready to write a few things.

## My Environment

![](/static/images/erlang/1/env.png)

Despite you being free to develop on whatever you want, I'm gonna go through
whatever my setup is.

I use zsh with [oh-my-zsh](https://github.com/robbyrussell/oh-my-zsh), using
a [custom theme](https://gist.github.com/ferd/9905862) (depends on
[hg-prompt](https://bitbucket.org/sjl/hg-prompt/src) as a script), and stuck
in vi-mode, because vim vim vim.

For vim itself, to work with Erlang, I use these two lines in my .vimrc file:

```VimL
autocmd BufRead,BufNewFile *.erl,*.es.*.hrl,*.yaws,*.xrl set expandtab
au BufNewFile,BufRead *.erl,*.es,*.hrl,*.yaws,*.xrl setf erlang
```

And I depend on these two plugins:

1. [vimerl](https://github.com/jimenezrick/vimerl)
2. [erlang-motions](https://github.com/edkolev/erlang-motions.vim)

I don't use a lot of material outside of that, and the OS will tend to be
my IDE -- for projects that I tend to work on a lot, I will use tmux scripts
(see [this blog post for an example](http://blog.htbaa.com/news/tmux-scripting))
-- to get everything going early.

## The Project

To avoid the usual Hello World stuff, this tutorial will use a somewhat more fun
application to get up and running from a basic Erlang app that can be run within
a module, to a proper OTP library that can be included by other projects, to a
release than can be self-executing and distributed to client's computer, or on a
server.

Our project will be the replication of one of the most well-known software
programs in popular history, used in life-critical situations: Homer Simpson's
console in the episode where he's so fat he can work at home.

![](/static/images/erlang/1/homer-computer.gif)

From this episode we can infer the following about the software:

- When the program boots, it asks you to press any key.
- The program will ask you questions that can be answered by `yes/no`, but
  also `Y/N` or `y/n`
- Most questions can be turned into commands. Each assertion is equivalent to
  answering a given question positively. For example, `Vent radioactive gas? Yes/No`
  can be turned into the `Vent gas` command.
- Nothing should go wrong if you keep pressing `Y` all the time
- After a given delay, a new question is asked
- Too many times without venting radioactive gas risks exploding everything
- Some behaviours aren't defined by the TV show, so we go somewhat
  anyway we feel like

Out of this, a finite-state machine can be created. The one that follows
explains what I understood as possible, but you'll notice I'm not really good
at having a consistent notation for events, states, and so on:

                    [press any key]
                           |
                     (key pressed)
                           |
                 [check core temperature (first)]
                     \________,________/
                              |
                           (yes/no)
                              |
                 [venting radioactive gases (first)]
                    |                   |
                  (yes)      ,-<-,     (no)
                    |        |   |      |
           [gas blows away crop] |  [venting prevents explosions]
                    |            |      |           |
                    |            '--<-(yes)       (no)
                    \                              /
                     \______________,_____________/
                                    V
                                    |
                           [wait for command]<--------,
                                /       \             |
                          (get data)   (timeout)      |
                               |         |            |
                               |     [ask question]   |
                               |        /      \      |
                               |    (Yes)     (No)    |
                               |     /          |     |
                               +----'           '-----+
                               |                      |
                              [show result] -->-------'

Based on this, we'll be able to draw up a first prototype with all the required
state transitions. I've also looked for transcripts of the show and extracted
the following questions and consequences:

1. Check core temperature. yes/no:
   - `yes`: Core temperature normal.
   - `no`: --

2. Vent radioactive gas?
   - `yes`: \*gas blows away corn crop\*
   - `no`: venting prevents explosion (allow `yes`, show only the first time?)

3. Sound alertness horn?
   - `yes`: \*horn sounds in the distance\*
   - `no`: --

4. Decalcify calcium ducts?
   - `yes`: --
   - `no`: --

5. Special case: after denying venting too many times, the valve must
   be disabled manually.

The simplest way to write a basic FSM for this one is to use a bunch of function
calls. Given Erlang has last call optimization (a call that happens as a return
value does not leave a stack trace, and therefore can happen infinitely many
times), this is more than adequate.

The sequence of states `a -> b -> c` can be programmed as:

```erlang
a() ->
    b().

b() ->
    c().

c() ->
    done.
```

Of course, there's going to be more data in our case.

## The Prototype

Our glorious application will be called 'muumuu'. Whenever I don't exactly know
where I'm going, I decide to prototype stuff. And here I stress the importance
of *prototype*. Despite this fact, it will often end up being in production, but
yeah -- that's to be avoided.

I decide to start with the basic stuff to prototype, state transitions. I go for
them in a fairly simple manner, top-down:

```erlang
-module(muumuu_fsm).
-export([start/0]).

-define(MAX_NO_VENT, 5).

start() ->
    %% Seed PRNG
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed(A,B,C),
    wait_any_key().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% States and Transitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
wait_any_key() ->
    io:get_line("To Start, Press Any Key.\n> "),
    first_core_check().

first_core_check() ->
    case option("Check core temperature?") of
        yes -> core_temperature();
        no -> noop()
    end,
    first_gas_vent().

first_gas_vent() ->
    case option("Vent radioactive gas?") of
        yes -> blow_crops_away();
        no -> venting_prevents_explosions()
    end,
    wait_for_command().

wait_for_command() ->
    case wait_cmd(10000) of
        timeout ->
            {Opt, Yes, No} = random_option(),
            case option(Opt) of
                yes -> Yes();
                no -> No()
            end;
        Cmd ->
            case match_option(Cmd) of
                {_, Yes, _} -> Yes();
                _ -> noop
            end
    end,
    wait_for_command().
```

In this bit of code, we can see our 4 main states:

1. `wait_any_key`
2. `first_core_check`
3. `first_gas_event`
4. `wait_for_command`

The rest of the code is more or less going to be events and input management to
check the transitions:

- printing questions and getting responses (`option/1`)
- eventually waiting for a command (`wait_cmd/1` and `match_option/1`)
- or, if it takes too long, generate an option randomly (`random_option/1`)

You can look at the code, find whatever you want about it disgusting. So that's
the general idea I want in the code. Time to add all that option management
stuff:

```erlang
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Options and Response Handling %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
option(Prompt) ->
    show_option(Prompt),
    Data = io:get_line(""),
    case iolist_to_binary(Data) of
        <<"Y", _/binary>> -> yes;
        <<"y", _/binary>> -> yes;
        <<"N", _/binary>> -> no;
        <<"n", _/binary>> -> no;
        _ -> ambiguous
    end.

show_option(Str) -> io:format("~s (Y/N)~n> ", [Str]).

wait_cmd(Timeout) ->
    Parent = self(),
    Pid = spawn(fun() -> Parent ! io:get_line("") end),
    receive
        Data -> Data
    after Timeout ->
        exit(Pid, kill),
        timeout
    end.

random_option() ->
    Pos = random:uniform(tuple_size(opts())),
    {_, Val} = element(Pos, opts()),
    Val.

match_option(Data) ->
    case [Vals || {Pattern, Vals} <- tuple_to_list(opts()),
                  nomatch =/= re:run(Data, Pattern, [caseless])] of
        [Opt|_] -> Opt;
        [] -> invalid_opt
    end.
```

Cool. Not fantastic looking yet. Basically, an option will only fetch a line of
text entered by the user, look at the first response, and return what it is.
Showing the options just wraps things up so they look like a prompt.

Interestingly enough, the command has to be waited for in a different process.
The problem with this it that Erlang's standard library doesn't support a
timeout mode for `io` operations, which would tell us "wait 10 seconds for
input or quit". Therefore, there is a need to move this to a process.

The rest relies on an elusive `opts()` function that apparently returns all
questions and options offered to the user:

```erlang
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Defining Options/Events %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
opts() ->
    {{"(check|core|temp)",
      {"Check core temperature?",
       fun core_temperature/0,
       fun noop/0}},
     {"(vent|rad|gas)",
      {"Vent radioactive gas?",
       fun vent_gas/0,
       fun no_venting/0}},
     {"(sound|alert|horn)",
      {"Sound alertness horn?",
       fun sound_horn/0,
       fun noop/0}},
     {"(calc|duct)",
      {"Decalcify calcium ducts?",
       fun noop/0,
       fun noop/0}}}.
```

This basically is a tuple (I use a tuple because it makes random selection with
a fixed position more efficient) of all questions, positive and negative
response and consequences, paired up with a regular expression that represents
fuzzy matching -- for example, someone typing it `check temperature` should
match `Check core temperature?` as a question, and return both options. The
code back in `wait_for_command/0` will only execute the `core_temperature/0`
function.

Finally, all actions and consequences can be implemented:

```erlang
noop() -> ok.

venting_prevents_explosions() ->
    case option("Venting prevents explosion.") of
        yes -> blow_crops_away();
        no -> noop()
    end.

core_temperature() -> io:format("Core temperature normal.~n").

blow_crops_away() -> io:format("*Gas blows away corn crop*~n").

sound_horn() -> io:format("*horn sounds in the distance*~n").

pressure_too_high() -> io:format("Pressure too high. Tank must be shut down manually.~n").

vent_gas() ->
    %% After ?MAX_NO_VENT, pressure has to be shut down
    %% manually -- unsupported in this here program!
    case get(missed) of
        ?MAX_NO_VENT ->
            pressure_too_high();
        _ ->
            put(missed, 0),
            blow_crops_away()
    end.

no_venting() ->
    case get(missed) of
        undefined -> put(missed, 1);
        N -> put(missed, N+1)
    end.
```

Here the two last functions implement the special last requirement: after
denying venting too many times, the valve must be disabled manually.

Here we use a dirty ugly counter for prototyping's sake. In fact I had
forgotten about that requirement at the time and just bolted it on that way.
The prototype helped figure that requirement out, and the final version can now
be designed with this in mind.

You can run the code and try it from a shell:

```
λ → erlc src/muumuu_fsm.erl && erl -s muumuu_fsm -noshell
To Start, Press Any Key.
> .
Check core temperature? (Y/N)
> N
Vent radioactive gas? (Y/N)
> No
Venting prevents explosion. (Y/N)
> yes
*Gas blows away corn crop*
Sound alertness horn? (Y/N)
> Y
*horn sounds in the distance*
```

That works. Using `-s <module>` runs the `start/0` function from that module,
and using `-noshell` makes it so that the Erlang VM won't fight with all the
`io` calls I'm doing for user input ownership.

Sadly, the implementation is kind of ugly and shouldn't go in production.

![](/static/images/erlang/1/muumuu.gif)


## Making it a library

There are two ways to make something reach production: distributing yourself, or
distributing it as a library other Erlang developers can use.

The latter can be a prerequisite for the former, so we're going to start there.
By default, everyone using Erlang in the open source community uses [OTP
applications](http://learnyousomeerlang.com/building-otp-applications).

OTP is kind of often treated as a super advanced topic, so what I'm gonna show
here is how to take any non-OTP compliant code and turn it into an OTP
application. Fun fun.

First, the directory structure:

```
src/
  - muumuu_fsm.erl
```

That's all you need in terms of structure if you have rebar installed in your
system.

Add a file in `src/` called `muumuu.app.src`. This file is basically
telling Erlang (and rebar) what the library is:

```erlang
{application, muumuu, [
  {description, "Too fat to go to the power plant app"},
  {vsn, "0.1.0"},
  {registered, []},
  {applications, [kernel, stdlib, crypto]},
  {mod, {muumuu_app, []}},
  {env, []}
]}.
```

The `registered` entry specifies what processes are going to be globally
registered on the node. In this case, none. The `applications` tuple is a list
of all applications we depend on. All applications depend on both `kernel` and
`stdlib`. These entries have to always be in there. On the other hand, `crypto`
is optional to most apps, but we need it because we use it to seed our
pseudo-random number generator in `start/0`.

The `env` tuple can contain [configuration
values](http://erlang.org/doc/man/app.html), but we need none right now.

The other option considered here is `mod`. If your library requires no process
to be started and you're just shipping code around, you're done. In our case
however, we're starting a process (or we want to), and therefore we specify an
application module named `muumuu_app`. This module is also in `src/`:

```erlang
-module(muumuu_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    muumuu_sup:start_link().

stop(_) ->
    ok.
```

That module is basically giving callbacks to the Erlang VM. See it a bit as the
`main` function in C, except you also have to provide a `stop` function that
will clean up once the process exits. In this case we need nothing.

What's the `muumuu_sup` module? That's the final step to be glued in OTP. OTP
has a concept called
[`supervisors`](http://learnyousomeerlang.com/supervisors). Supervisors are in
charge of checking OTP-compliant processes, to start them, stop them, and
[provide guarantees regarding their
state](http://ferd.ca/it-s-about-the-guarantees.html).

Unfortunately, our process isn't OTP-compliant. The guys at Ericsson have long
ago hit that problem and developed a [supervisor
bridge](http://www.erlang.org/doc/man/supervisor_bridge.html), which basically
acts as a wrapper. This is what we could use if I were not the kind of person
to want my OTP processes done correctly everywhere.

For the time being, I'll stick with a regular supervisor and will rewrite the
FSM right after:

```erlang
-module(muumuu_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 1, 5},
          [{console,
            {muumuu_fsm, start_link, []},
            permanent, 5000, worker, [muumuu_fsm]}]}}.
```

This will start `muumuu_fsm` as a permanent worker that can die once every 5
seconds before the entire system crashes. I don't have a good way to pick
frequencies, but 1 in 5 seconds sounds like something reasonable for someone to
mash keys in ways bad enough it causes errors.

So then comes the rewrite from prototype to
[`gen_fsm`](http://learnyousomeerlang.com/finite-state-machines). This is stuff
that has been covered in multiple tutorials before, so I'm going to skip most
of it. You can instead look at books and docs for `gen_fsm`, follow along the
final module,
[muumuu_fsm.erl](https://github.com/ferd/howistart-erlang1-code/blob/master/library/src/muumuu_fsm.erl),
and see for yourself.

The biggest changes there, outside of providing the `gen_fsm` callbacks
required by the OTP behavior, are related to the general information flow.
Rather than being really direct sequences of functions doing whatever they
want, the OTP version of the module becomes a lot more declarative.

We no longer enter a state function, ask a question, and wait for the response
within the same context. The logic has moved so that an event in a state (say
`first_gas_vent`) causes a question to be asked before transitioning to the
state that will handle that response.

This doesn't make the code particulalry harder to read, just different:

```erlang
init([]) ->
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed(A,B,C),
    {ok, wait_any_key, prompt(wait_any_key, #state{})}.

%% [...]

wait_any_key(_, State) ->
    {next_state, first_core_check, prompt(first_core_check, State)}.

first_core_check(no, State) ->
    {next_state, first_gas_vent, prompt(first_gas_vent, State)};
first_core_check(yes, State) ->
    show_core_temperature(),
    {next_state, first_gas_vent, prompt(first_gas_vent, State)}.

first_gas_vent(no, State) ->
    StateName = venting_prevents_explosions,
    {next_state, StateName, prompt(StateName, State)};
first_gas_vent(yes, State) ->
    show_blow_crops_away(),
    {next_state, wait_for_command, prompt(wait_for_command, State), 10000}.
```

This form, along with the experience gained in the prototype, allows for
simpler state management via the `State` variable, which allows us to be more
transparent about our usage of venting limits, for example. We also instantly
benefit from everything OTP gives us in terms of transparency: tracing,
logging, statistics, and so on (see [the `sys`
module](http://www.erlang.org/doc/man/sys.html))

With that code in place, we can compile and run the entire application:

```
λ → rebar compile
==> how-i-start (compile)
Compiled src/muumuu_app.erl
Compiled src/muumuu_sup.erl
Compiled src/muumuu_fsm.erl
```

With this compiled we can run it, with a funky command:

```
λ → erl -pa ebin -eval 'application:ensure_all_started(muumuu).' -noshell
To Start, Press Any Key.
> any
Check core temperature? (Y/N)
> y
Core temperature normal.
Vent radioactive gas? (Y/N)
> y
*Gas blows away corn crop*
```

That's kind of an ugly command to run the app, but the app is now something
other people can use to pull it within their own systems.

In order to run it ourselves and actually ship it to customers, we will need to
build a release.

![](/static/images/erlang/1/y-y-y.gif)


## Releases

The directory structure we've been using was for an application and turns out
looking like:

```
src/
ebin/
```

At the simplest level. A release is basically a group of applications put
together. For this reason, we'll change the directory structure a bit and add
the `relx` executable in there (the `relx` adoption is still low enough that
it's worth shipping with the code):

```
apps/
    - muumuu/
        - src/
        - ebin/
rebar.config
relx
relx.config
```

All applications you need will go into `apps/`. Here I just moved `src/` to
`apps/muumuu/`.

The rebar.config file looks like this:

```erlang
%% Tell rebar about this directory's structure
{lib_dirs, ["apps", "deps"]}.
{sub_dirs, ["apps/*"]}.

%% Build a release when compiling
{post_hooks,[{compile, "./relx"}]}.
```

This basically means that calling `rebar compile` will indirectly call `relx` to
build the release.

to build the release, `relx.config` contains all the instructions:

```erlang
{paths, ["apps", "deps"]}.
{default_release, muumuu, "0.1.0"}.

%% comment this line for a release that ships its own Erlang VM
{include_erts, false}.
%% uncomment this line to ship a release without the source code included
% {include_src, false}.


{release, {muumuu, "0.1.0"},
 %% list of apps to include
 [muumuu]}.
```

This will create a release that will only include Erlang source code, but use
the currently installed Erlang VM to run things. Then the magic happens:

```
λ → rebar compile
==> muumuu (compile)
==> how-i-start (compile)
Starting relx build process ...
Resolving OTP Applications from directories:
    /Users/ferd/code/self/how-i-start/apps
    /Users/ferd/.kerl/builds/R16B03-1/release_R16B03-1/lib

Resolving available releases from directories:
    /Users/ferd/code/self/how-i-start/apps
    /Users/ferd/.kerl/builds/R16B03-1/release_R16B03-1/lib

Resolved muumuu-0.1.0
release successfully created!
```

And a release is born! To run it:

```
λ → ./_rel/muumuu/bin/muumuu -noshell
To Start, Press Any Key.
>
```

Pretty cool. This can now be shipped and distributed to people.

I want to make the release a bit fancier though. As you've just seen, we still
need to put the `-noshell` by hand, which is totally unacceptable.

To fix this, add a `config/` repository, and I open the `vm.args` file in vim in
there:

```bash
# only show the programmed prompt
-noshell

# for remote access & debugging
-name nucular_plant@127.0.0.1

# not needed
-smp disable
+A 1
```

Arguments in there I merged into one. A good practice for any Erlang system is
to give it a name, which will let you connect to it while it's running. In this
case I could go in and debug the console as the user is maintaining the
powerplant.

The last arguments (`-smp disable +A 1`) are basically optimizations for this
very app: they remove Erlang parallelism (I'm running a single active process
for the thing, so why bother?) and removes the number of asynchronous threads
for IO to a single one (for the same reason -- one active process, why
bother?).

In more serious apps, tweaking your VM options can be worthwhile, but outside of
this text's scope.

The relx config file needs an update too:

```erlang
{paths, ["apps", "deps"]}.
{default_release, muumuu, "1.0.0"}.

%% comment this line for a release that ships its own Erlang VM
{include_erts, false}.
%% uncomment this line to ship a release without the source code included
% {include_src, false}.

{release, {muumuu, "1.0.0"},
 %% list of apps to include
 [muumuu]}.

{vm_args, "./config/vm.args"}.
```

The last line is the new one. Compile again and the arguments should implicitly
be passed to the node:

```
λ → rebar compile
==> muumuu (compile)
==> how-i-start (compile)
===> Starting relx build process ...
===> Resolving OTP Applications from directories:
          /Users/ferd/code/self/how-i-start/apps
          /Users/ferd/.kerl/builds/R16B03-1/release_R16B03-1/lib
===> Resolving available OTP Releases from directories:
          /Users/ferd/code/self/how-i-start/apps
          /Users/ferd/.kerl/builds/R16B03-1/release_R16B03-1/lib
===> Resolved muumuu-1.0.0
===> release successfully created!
λ → ./_rel/muumuu/bin/muumuu
To Start, Press Any Key.
> <Tab>
Check core temperature? (Y/N)
>
```

Cool, everything works. I now have a binary executable I can link to from
anywhere in the system and will require no magical arguments to work!

## Tests

As much as I like to try and get testing done ahead of time -- it's the only
time it's not super terrible and crappy -- I often end up adding it after the
fact when I know I'll have to maintain it.

For this, each app should have its tests, so I'll have to add a `test/`
directory in `apps/muumuu/`.

My tool of choice is [Common
Test](http://learnyousomeerlang.com/common-test-for-uncommon-tests), which
while it is kind of full of annoying overheads for unit testing and is mostly
useless for shell output (you gotta deal with HTML files), it scales fairly
well for integration and system tests.

The test suite in there is going to be `muumuu_SUITE.erl`:

```erlang
-module(muumuu_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

%% Copy/pasting from the suite
-record(state, {no_vent_count=0,
                pid,
                yes,
                no}).

all() ->
    [demo_session].
```

So at first I'm just gonna make one run-through test. Testing `muumuu` is going
to be hard because it's purely a side-effectful application.

Before going further, I'll say that the trick to getting this working is to use
`meck`, which is pretty much the best code-mocking application around.

Adding `meck` can be done by declaring `rebar.config` dependencies:

```erlang
{deps, [
    {meck, "0.8.*", {git, "https://github.com/eproxus/meck.git", {tag, "0.8.1"}}}
]}.
```

Rebar pulls stuff from github, so at the very least, stick a tag or a commit
hash in there, and not a branch that can be mutable. Call in `rebar get-deps
compile` and it will be available for tests.

Now back to `muumuu_SUITE`. Time to set up the state:

```erlang
init_per_testcase(demo_session, Config) ->
    mock_io(),
    {ok, Pid} = muumuu_fsm:start_link(),
    [{pid, Pid} | Config].

end_per_testcase(_, Config) ->
    meck:unload(io),
    Pid = ?config(pid, Config),
    unlink(Pid),
    exit(Pid, shutdown),
    wait_for_death(Pid).
```

Mocking the `io` system is a fun way to basically take it and make it return
messages we can look at. That all takes place in `mock_io()`, and after that's
in place, we start a `muumuu` instance directly (no application needed):


```erlang
mock_io() ->
    %% For this one we mock the IO system so that instead of
    %% printing messages and getting input to and from the user,
    %% we instead have a message-passing interface that will
    %% be inspectable.
    %%
    %% Note that because the `io` module is pre-compiled by the
    %% VM, we have to 'unstick' it first, and be careful to keep
    %% it mocked as little as possible.
    Parent = self(),
    code:unstick_dir(filename:dirname(code:where_is_file("io.beam"))),
    meck:new(io, [passthrough, no_link]),
    meck:expect(io, format, fun(Str) ->
        Parent ! {out, Str},
        ok
    end),
    meck:expect(io, format, fun(Str, Args) ->
        Parent ! {out, io_lib:format(Str,Args)},
        ok
    end),
    meck:expect(io, get_line, fun(_Prompt) ->
        Parent ! {in, self()},
        receive {Parent, In} -> In end
    end).
```

Ugly. The first step is unstickying the directory for Erlang code. Most modules
don't require that, only those in Erlang's standard library. Unstickying allows
to load new versions of code at run time, which `meck` dynamically does.

Here what I'm doing is mocking the functions `io:format/1`, `io:format/2` and
`io:get_line/1` to send messages of the form `{in, Msg}` and `{out, Msg}` from
input and output, respectively. `meck:unload(io)` will undo that.

We also had the `wait_for_death/1` call. I'm using these *everywhere* in tests.
Timers are the enemy of good concurrent testing, and if you rely on a
`timer:sleep(1000)` of some sort to make sure everything is clean, you're doing
it wrong.

Here the function polls to return ASAP, with a tiny sleep to not heat up your
room too much via the CPU:

```erlang
wait_for_death(Pid) ->
    case is_process_alive(Pid) of
        true ->
            timer:sleep(10),
            wait_for_death(Pid);
        false ->
            ok
    end.
```

With this done, I can start planning more for the test. This here is something I
always want to write a library for, and maybe some day I will, but right now I
re-do that crap by hand every time:

```erlang
%%%%%%%%%%%%%%%%%%
%%% TEST CASES %%%
%%%%%%%%%%%%%%%%%%

%% Pressing a given key through the message-passing interface
%% will yield expected output. There should be a prompt waiting
%% for a key.
%% All states can be cycled through using only Y/N answers.
demo_session(Config) ->
    Pid = ?config(pid, Config),
    out("press.*any.*key.*>"),
    in("<tab>"), % the characters shouldn't matter
    out("check.*core.*temp.*>"),
    in("Y"),
    out("temperature.*normal"),
    out("vent.*radioactive.*gas.*>"),
    in("no"),
    out("venting.*prevents.*explosion.*>"),
    in("yES"),
    out("gas.*blows.*crop.*"),
    gen_fsm:send_event(Pid, timeout), % force a timeout faster
    out(".*Y/N.*>"), % some question
    in("No"), % who cares
    in("vent gAs"), % force a command
    out("gas.*blows.*crop.*").
```

I basically just write the test the way I want it to look like. I will start
expecting messages that will match the regex `"press.*any.*key.*>"` being
output, after which I'll insert `<tab>`. Rinse and repeat.

Here, my desire is pretty much to turn the interactions I'd write in the shell
into a bunch of function calls and matches.

That's why I planned having a message-passing interface. I can now write
functions to wrap that functionality:

```erlang
%%%%%%%%%%%%%%%
%%% HELPERS %%%
%%%%%%%%%%%%%%%

in(Input) ->
    receive
        {in, Pid} -> Pid ! {self(), Input}
    after 1000 ->
        ct:pal("MBOX: ~p", [process_info(self(), messages)]),
        error({too_long, {in, Input}})
    end.
```

If we look back into the mocked function, the mocked function sends us `{in,
ProcessThatWaitsForInput}`. We take the `Input` argument, and send it back to
the mocked function (which runs in its own process).

If we never receive the `in` message, we crash, but printing the debugging
information. Interestingly here the function I use is `ct:pal`. It works exactly
like `io:format`, except:

1. It outputs to both the shell and HTML logs for Common Test
2. It's not gonna be used in production systems and it's surely never going to
   be mocked (unlike `io`).

The `out/1` helper is slightly more complex:

```erlang
%% fuzzily match the input string, waiting 1s at most
out(Expected) ->
    receive
        {out, Prompt} ->
            ct:pal("Expected: ~p~nPrompt: ~p", [Expected, Prompt]),
            {match, _} = re:run(Prompt, Expected, [dotall, caseless, global])
    after 1000 ->
        ct:pal("MBOX: ~p", [process_info(self(), messages)]),
        error({too_long, {out, Expected}})
    end.
```

That one makes an assertion on a regular expression with `re:run/3`, and the
rest is similar to what we did in `in/1`. We receive the output, match it, and
that's it.

And there we go, we can run the tests (remember, you need to have called
`rebar get-deps compile` before getting here, so meck is there and built):

```
λ → rebar ct -r skip_deps=true
==> muumuu (ct)
DONE.
Testing apps.muumuu: TEST COMPLETE, 1 ok, 0 failed of 1 test cases

WARN:  'ct' command does not apply to directory /Users/ferd/code/self/how-i-start
```

For a non-release (something where `src/` is still at the top level), just
calling `rebar ct` would work, without testing all the dependencies, which is
legitimately a crapload nicer. To skip testing the dependencies but still
accounting for them, the `-r skip_deps=true` arguments need to be added.

In practice, it will often make sense to develop all the applications
independently, running their own tests, and only later pull them all in a
release structure to build and ship a release.

After this, I go do something else because I'm pretty much done. You can see all the [code here](https://github.com/ferd/howistart-erlang1-code).

![](/static/images/erlang/1/outdoors.gif)
