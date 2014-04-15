%%% Homer's program goes a bit like this:
%%%
%%% 1          [press any key]
%%%                   |
%%%             (key pressed)
%%%                   |
%%% 2       [check core temperature (first)]
%%%               \________,________/
%%%                        |
%%%                     (yes/no)
%%%                        |
%%% 3         [venting radioactive gases (first)]
%%%            |                   |
%%%          (yes)      ,-<-,     (no)
%%%            |        |   |      |
%%%   [gas blows away crop] |  [venting prevents explosions]
%%%            |            |      |           |
%%%            |            '--<-(yes)       (no)
%%%            \                              /
%%%             \______________,_____________/
%%%                            v
%%%                            |
%%% 4                 [wait for command]<--------,
%%%                        /       \             |
%%%                  (get data)   (timeout)      |
%%%                       |         |            |
%%%                       |     [ask question]   |
%%%                       |        /      \      |
%%%                       |    (Yes)     (No)    |
%%%                       |     /          |     |
%%%                       +----'           '-----+
%%%                       |                      |
%%%                      [show result] -->-------'
%%%
%%% Simple isn't it?

-module(muumuu_fsm).
-behaviour(gen_fsm).

-define(MAX_NO_VENT, 5).
-record(state, {no_vent_count=0,
                pid,
                yes,
                no}).

-export([start_link/0]).
-export([init/1, terminate/3, code_change/4, % setup/teardown/upgrade
         handle_event/3, handle_sync_event/4, handle_info/3, % global events
         %% only async events
         wait_any_key/2, first_core_check/2, first_gas_vent/2,
         venting_prevents_explosions/2, wait_for_command/2]).

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GEN_FSM CALLBACKS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed(A,B,C),
    {ok, wait_any_key, prompt(wait_any_key, #state{})}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {next_state, StateName, State}.

handle_info(_Event, StateName, State) ->
    {next_state, StateName, State}.

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

venting_prevents_explosions(no, State) ->
    {next_state,
     wait_for_command,
     prompt(wait_for_command, State#state{no_vent_count=1}),
     10000};
venting_prevents_explosions(yes, State) ->
    first_gas_vent(yes, State).

wait_for_command(timeout, State) ->
    {Opt, Yes, No} = random_option(),
    {next_state, wait_for_command, prompt(Opt, State#state{yes=Yes, no=No})};
wait_for_command({_Event, Yes, _No}, State) ->
    %% Random event result
    NewState = Yes(State),
    {next_state, wait_for_command, prompt(wait_for_command, NewState), 10000};
wait_for_command(Event, State) ->
    Action = case Event of
        no -> State#state.no;
        yes -> State#state.yes;
        invalid_opt -> fun noop/1
    end,
    NewState = Action(State),
    {next_state, wait_for_command, prompt(wait_for_command, NewState), 10000}.


%% We make the prompt asynchronous by spawning a linked one-off process
%% that messages back to the parent calling.
prompt(Opt, State=#state{pid=undefined}) ->
    case Opt of
        wait_any_key -> io:format("To Start, Press Any Key.~n> ");
        first_core_check -> show_option("Check core temperature?");
        first_gas_vent -> show_option("Vent radioactive gas?");
        venting_prevents_explosions -> show_option("Venting prevents explosion.");
        core_check -> show_option("Check core temperature?");
        gas_vent -> show_option("Vent radioactive gas?");
        sound_horn -> show_option("Sound alertness horn?");
        decalcify -> show_option("Decalcify calcium ducts?");
        wait_for_command -> ok
    end,
    State#state{pid=get_input()};
prompt(Opt, State=#state{pid=Pid}) when is_pid(Pid) ->
    unlink(Pid),
    exit(Pid, kill),
    prompt(Opt, State#state{pid=undefined}).

get_input() ->
    Parent = self(),
    spawn_link(fun() ->
        gen_fsm:send_event(Parent, option(io:get_line("")))
    end).

option(List) when is_list(List) -> option(iolist_to_binary(List));
option(Bin) when byte_size(Bin) =< 4 -> % 4 including \n
    case Bin of
        <<"Y", _/binary>> -> yes;
        <<"y", _/binary>> -> yes;
        <<"N", _/binary>> -> no;
        <<"n", _/binary>> -> no;
        _ -> ambiguous
    end;
option(Bin) ->
    match_option(Bin).

noop(State) -> State.

core_temperature(State) ->
    show_core_temperature(),
    State.

vent_gas(State=#state{no_vent_count=?MAX_NO_VENT}) ->
    show_pressure_too_high(),
    State;
vent_gas(State) ->
    show_blow_crops_away(),
    State#state{no_vent_count=0}.

no_venting(State=#state{no_vent_count=Count}) ->
    State#state{no_vent_count=Count+1}.

sound_horn(State) ->
    show_sound_horn(),
    State.


show_core_temperature() -> io:format("Core temperature normal.~n").

show_blow_crops_away() -> io:format("*Gas blows away corn crop*~n").

show_sound_horn() -> io:format("*horn sounds in the distance*~n").

show_pressure_too_high() -> io:format("Pressure too high. Tank must be shut down manually.~n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Options and Response Handling %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

show_option(Str) -> io:format("~s (Y/N)~n> ", [Str]).

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
      {core_check,
       fun core_temperature/1,
       fun noop/1}},
     {"(vent|rad|gas)",
      {gas_vent,
       fun vent_gas/1,
       fun no_venting/1}},
     {"(sound|alert|horn)",
      {sound_horn,
       fun sound_horn/1,
       fun noop/1}},
     {"(calc|duct)",
      {decalcify,
       fun noop/1,
       fun noop/1}}}.


