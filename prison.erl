-module(prison).
-export([run/1, run/0]).
% Export for warning supression
-export([prisoner/1,do_nothing/1,yard_logic/3, yard_logic2/3]).
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
run() ->
	LightPid = screw_in_lightbulb(),
	Prisoners = imprison_prisoners(?NUM_PRISONERS),
	run(Prisoners, [], 0, LightPid).
run(NumPrisoners) ->
	LightPid = screw_in_lightbulb(),
	Prisoners = imprison_prisoners(NumPrisoners),
	run(Prisoners, [], 0, LightPid).
run(Prisoners, YardVisitors, DayCount, LightPid) ->
	YardPrisoner = lists:nth(random:uniform(length(Prisoners)), Prisoners),
	go_to_cell(Prisoners--[YardPrisoner]),
	%io:format("Day ~p\n", [DayCount]),
	case go_to_yard(YardPrisoner, LightPid) of
		all_visited ->
			case Prisoners--(YardVisitors++[YardPrisoner]) of
				[] ->
					io:format("Prisoner was right. Let 'em go\n"),
					go_free(Prisoners);
				_ ->
					io:format("WRONG. Execution day\n"),
					go_die(Prisoners)
			end;
		ok ->
			run(Prisoners, YardVisitors++[YardPrisoner], DayCount+1, LightPid)
	end.

%yet_to_visit_yard(Prisoners, YardList) ->
%	[X || X <- Prisoners, lists:member(X, YardList) =:= false].
	
imprison_prisoners(NumPrisoners) -> imprison_prisoners(NumPrisoners, NumPrisoners). 
imprison_prisoners(0, _) -> [];
imprison_prisoners(NumPrisoners, TotalPrisoners) -> [spawn_prisoner(TotalPrisoners) | imprison_prisoners(NumPrisoners-1,TotalPrisoners)].


go_free(Prisoners) ->
	[Prisoner ! {self(), {goFree}} || Prisoner <- Prisoners].
go_die(Prisoners) ->
	[Prisoner ! {self(), {executionDay}} || Prisoner <- Prisoners].

go_to_yard(Pid, LightPid) -> 
	Pid ! {self(), {yardDay, LightPid}},
	receive
		{Pid, {all_inmates_visited}} ->
			all_visited;
		{Pid, {ok}} -> ok
	end.

go_to_cell(Prisoners) -> 
	[let_prisoner_rot_in_cell(Prisoner) || Prisoner <- Prisoners].

let_prisoner_rot_in_cell(Pid) ->
	Pid ! {self(), {normalDay}},
	receive
		{Pid, {ok}} -> ok
	end.

%
% Prisoner
%
spawn_prisoner(NumPrisoners) ->
	spawn(?MODULE, prisoner, [{0,true,false,0,NumPrisoners}]).

prisoner(State) ->
	%% Prisoner state: number of days imprisoned
	%% am i leader, numtimes visited yard, 
	receive
		{From, {yardDay, LightPid}} ->
			prisoner(yard_logic2(From, LightPid, State));
			%prisoner(yard_logic2(From, LightPid, State));
		{From, {normalDay}} ->
			From ! {self(), {ok}},
			prisoner(do_nothing(State));
		{_, {executionDay}} ->
			io:format("~p. dead\n", [self()]),
			ok;
		{_, {goFree}} ->
			io:format("Free at last. Free at last. ~p is free at last\n", [self()]),
			ok;
		X ->
			io:format("Error!! Bad message received ~p ~p\n", [self(), X])
	end.

do_nothing({Days_in_prison,A,B,C,D}) -> {Days_in_prison+1,A,B,C,D}.

years(Number_of_days) ->
	Number_of_days / 365.

yard_logic2(Guard, LightPid, State) ->
% The prisoner is the leader if he goes into the exercise yard on the second day
% Light's initial position is unknown but will be set to on by first prisoner in yard
% Leader will leave it in the off position always
% If the light is on, the leader turns it off and increments the count by one 
	{Days_in_prison,Havent_sent_signal,Leader,NumVisited,NumPrisoners} = State,
	Havent_sent_signalReturn = case Havent_sent_signal of
		true ->
			case check_light(LightPid) of
				on when Days_in_prison=:=0 -> false;
				on -> true;
				off ->
					toggle_light(LightPid),
					false
			end;
		false -> false
	end,
	case LeaderReturn = (Leader orelse Days_in_prison=:=1) of
		true ->
			io:format("I am the leader! Pid: ~p\n", [self()]),
			case check_light(LightPid) of
				on ->
					io:format("~p years elapsed. Light is on. Turn it off and +1: ~p\n", [years(Days_in_prison), self()]),
					toggle_light(LightPid),
					case Days_in_prison=:=1 of
						true -> NumVisitedReturn=2;
						false -> NumVisitedReturn=NumVisited+1
					end,
					case NumVisitedReturn=:=NumPrisoners of
						true ->
							io:format("we've all been in!\n", []),
							Guard ! {self(), {all_inmates_visited}};
						false -> 
							io:format("~p out of ~p have been in\n", [NumVisitedReturn,NumPrisoners])
					end;
				off ->
					io:format("Light is off\n", []),
					NumVisitedReturn = NumVisited
			end;
		false -> NumVisitedReturn = 0
	end,
	Guard ! {self(), {ok}},
	{Days_in_prison+1, Havent_sent_signalReturn, LeaderReturn, NumVisitedReturn, NumPrisoners}.


yard_logic(Guard, LightPid, State) ->
% * Prisoner is leader if she goes into the 
% exercise yard on the first(0th) day
% * Light's initial position is unknown
% * Leader will leave it in the off position always
% * All other prisoners switch light on during their 
% first visit to yard
% * If light is on, leader turns it off and increments 
% the count
	{Days_in_prison,_,Leader,_,_} = State,
	case Leader orelse Days_in_prison=:=0 of
		true ->
			leader_logic(Guard,LightPid,State);
		false ->
			normal_prisoner_logic(Guard, LightPid, State)
	end.
%	{Days_in_prison+1, A, Leader, NumVisitedReturn, NumPrisoners}.

normal_prisoner_logic(Guard, LightPid, State) ->
	{A,Havent_sent_signal,B,C,D} = State,
	Havent_sent_signalReturn = case check_light(LightPid) of
		off ->
			case Havent_sent_signal of
				true ->
					toggle_light(LightPid),
					false;
				false ->
					Havent_sent_signal
			end;
		on ->
			Havent_sent_signal
	end,
	Guard ! {self(), {ok}},
	{A,Havent_sent_signalReturn,B,C,D}.


leader_logic(Guard,LightPid,State) ->
	{Days_in_prison,_,_,NumVisited,NumPrisoners}=State,
	io:format("I am the leader! Pid: ~p\n", [self()]),
	case check_light(LightPid) of
		on ->
		io:format("Day ~p, Light is on. Turn it off and +1: ~p\n", [Days_in_prison, self()]),
		toggle_light(LightPid),
			case (NumVisitedReturn=NumVisited+1)=:=NumPrisoners of
				true ->
					io:format("we've all been in!\n", []),
					Guard ! {self(), {all_inmates_visited}};
				false -> 
					io:format("~p out of ~p have been in\n", [NumVisited+1,NumPrisoners])
			end;
		off ->
			io:format("Light is off\n", []),
			NumVisitedReturn = NumVisited
	end,
	Guard ! {self(), {ok}},
	{Days_in_prison+1, false, true, NumVisitedReturn, NumPrisoners}.

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
			io:format("light toggled. Now ~p~n", [check_light(Pid)])
	end.
