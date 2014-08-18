-module(prison).
-export([run_prison_experiment/1, run_prison_experiment/0]).
% Export for warning supression
-export([prisoner/1,add_a_day/1,all_inmates_have_visited_yard/2]).
-export([light/1,check_light/1,toggle_light/1]).
-define(NUM_PRISONERS, 100).

%%% what if there are n lights?
%%% scrap of paper
%%% partitioned into wings that can communicate internally
%%%  
%State: Prisoners, YardedPrisoners, NumDays

%
% Guard
%
run_prison_experiment() ->
	LightPid = screw_in_lightbulb(),
	Prisoners = imprison_prisoners(?NUM_PRISONERS),
	run_prison_experiment(Prisoners, [], 0, LightPid).
run_prison_experiment(NumPrisoners) ->
	LightPid = screw_in_lightbulb(),
	Prisoners = imprison_prisoners(NumPrisoners),
	run_prison_experiment(Prisoners, [], 0, LightPid).
run_prison_experiment(Prisoners, YardVisitors, DayCount, LightPid) ->
	YardPrisoner = lists:nth(random:uniform(length(Prisoners)), Prisoners),
	go_to_cell(Prisoners--[YardPrisoner]),
	io:format("Day ~p\n", [DayCount]),
	case go_to_yard(YardPrisoner, LightPid) of
		all_visited ->
			case Prisoners -- (YardVisitors++[YardPrisoner]) of
				[] ->
					io:format("Prisoner was right. Let 'em go\n"),
					go_free(Prisoners);
				_ ->
					io:format("WRONG. Execution day\n"),
					go_die(Prisoners)
			end;
		_ ->
			run_prison_experiment(Prisoners, YardVisitors++[YardPrisoner], DayCount+1, LightPid)
	end.

%yet_to_visit_yard(Prisoners, YardList) ->
%	[X || X <- Prisoners, lists:member(X, YardList) =:= false].
	
imprison_prisoners(0) -> [];
imprison_prisoners(NumPrisoners) -> [spawn_prisoner() | imprison_prisoners(NumPrisoners-1)].


go_free(Prisoners) ->
	[Prisoner ! {self(), {goFree}} || Prisoner <- Prisoners].
go_die(Prisoners) ->
	[Prisoner ! {self(), {executionDay}} || Prisoner <- Prisoners].

go_to_yard(Pid, LightPid) -> 
	Pid ! {self(), {yardDay, LightPid}},
	receive
		{_, {all_inmates_visited}} ->
			all_visited;
		_ -> ok
	end.

go_to_cell(Prisoners) -> 
	[let_prisoner_rot_in_cell(Prisoner) || Prisoner <- Prisoners].

let_prisoner_rot_in_cell(Pid) ->
	Pid ! {self(), {normalDay}},
	receive
		_ -> ok
	end.

%
% Prisoner
%
spawn_prisoner() ->
	spawn(?MODULE, prisoner, [{0,true,false,0,false}]).

prisoner(State) ->
	%% Prisoner state: number of days imprisoned
	%% am i leader, numtimes visited yard, 
	receive
		{From, {yardDay, LightPid}} ->
			UpdatedState = all_inmates_have_visited_yard(LightPid, State),
			{_,_,_,_,AllVisited} = UpdatedState, 
			case AllVisited of
				true ->
					From ! {self(), {all_inmates_visited}},
					io:format("~p says we've all been in!\n", [self()]);
				_ ->
					From ! {self(), {ok}}
			end,
			prisoner(UpdatedState);
		{From, {normalDay}} ->
			From ! {self(), {ok}},
			prisoner(add_a_day(State));
		{_, {executionDay}} ->
			io:format("~p. dead\n", [self()]),
			ok;
		{_, {goFree}} ->
			io:format("Free at last. Free at last. ~p is free at last\n", [self()]),
			ok;
		X ->
			io:format("Error!! Bad message received ~p ~p\n", [self(), X])
	end.

add_a_day({Days_in_prison,A,B,C,D}) -> {Days_in_prison+1,A,B,C,D}.


all_inmates_have_visited_yard(LightPid, State) ->
% 
% The prisoner is the leader if he goes into the exercise yard on the first(0th) day
% Light's initial position is unknown
% Leader will leave it in the off position always
% If the light is on, the leader turns it off and increments the count by one 
	{Days_in_prison, Havent_sent_signal, Leader, NumVisited, _} = State,
	% All prisoners do this once
	Havent_sent_signal_return = case Havent_sent_signal and (check_light(LightPid) =:= off) of
		true ->
			toggle_light(LightPid),
			false;
		false ->
			true
	end,
	case Leader orelse Days_in_prison =:=0 of
		true -> 
			io:format("I am the leader! Pid: ~p\n", [self()]),
			LeaderReturn=true,
			case check_light(LightPid) of
				on ->
					toggle_light(LightPid),
					io:format("Light is on. Turn it off and +1: ~p\n", [self()]),
					NumVisitedReturn = NumVisited+1;
				off ->
					io:format("Light is off\n", []),
					NumVisitedReturn = NumVisited
			end;
		_ ->
			LeaderReturn=false,
			NumVisitedReturn = 0
	end,
	{Days_in_prison+1, Havent_sent_signal_return, LeaderReturn, NumVisitedReturn, NumVisitedReturn+1=:=?NUM_PRISONERS}.

%       %
% LIGHT %
%       %
screw_in_lightbulb() ->
	spawn(?MODULE, light, [on]).

light(State) ->
	receive
		{From, {toggle}} ->
			From ! {self(), ok},
			case State of
				on ->
					light(off);
				off ->
					light(on)
			end;
		{From, {query}} ->
			From ! {self(), State},
			light(State);
		terminate ->
			ok
	end.

check_light(Pid) ->
	Pid ! {self(), {query}},
	receive
		{_, on} -> 
			on;
		{_, off} ->
			off
	end.

toggle_light(Pid) ->			
	Pid ! {self(), {toggle}},
	receive
		{_, ok} -> 
			io:format("light toggled~n")
	end.
