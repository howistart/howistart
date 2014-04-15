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
