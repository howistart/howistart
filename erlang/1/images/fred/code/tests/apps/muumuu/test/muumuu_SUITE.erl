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

wait_for_death(Pid) ->
    case is_process_alive(Pid) of
        true ->
            timer:sleep(10),
            wait_for_death(Pid);
        false ->
            ok
    end.

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
