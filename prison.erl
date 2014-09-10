-module(prison).
-export([run/1, run/0]).
% Export for warning supression
-export([new_prisoner/2,prisoner/1,do_nothing/1,yard_logic/3, yard_logic2/3]).
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
% Want to know: Number of days taken
% Was the prisoner correct
% Which day was the first day they'd all been in
% List of prisoners in the yard (order)
% 
run() ->
	LightPid = screw_in_lightbulb(),
	Prisoners = imprison_prisoners(?NUM_PRISONERS),
	run(Prisoners, [], 0, LightPid, undefined).
run(NumPrisoners) ->
	LightPid = screw_in_lightbulb(),
	Prisoners = imprison_prisoners(NumPrisoners),
	run(Prisoners, [], 0, LightPid, undefined).
run(Prisoners, YardVisitors, DayCount, LightPid, FirstDayAllIn) ->
	% select prisoner
	YardPrisoner = lists:nth(random:uniform(length(Prisoners)), Prisoners),
	% have they all been in yet?
	FirstDayAllInReturn = case FirstDayAllIn of
		undefined ->
			case Prisoners--(YardVisitors++[YardPrisoner]) of
				[] -> DayCount+1;
				_ -> undefined
			end;
		_ -> FirstDayAllIn
	end,			
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
			end,
			[length(Prisoners), DayCount+1, YardVisitors++[YardPrisoner], FirstDayAllInReturn];
		ok ->
			run(Prisoners, YardVisitors++[YardPrisoner], DayCount+1, LightPid, FirstDayAllInReturn)
	end.

%yet_to_visit_yard(Prisoners, YardList) ->
%	[X || X <- Prisoners, lists:member(X, YardList) =:= false].
	
imprison_prisoners(NumPrisoners) -> imprison_prisoners(NumPrisoners, NumPrisoners). 
imprison_prisoners(0, _) -> [];
imprison_prisoners(NumPrisoners, TotalPrisoners) -> [spawn_prisoner(TotalPrisoners, NumPrisoners) | imprison_prisoners(NumPrisoners-1,TotalPrisoners)].


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
spawn_prisoner(NumPrisoners, PrisonerId) ->
	spawn(?MODULE, new_prisoner, [NumPrisoners,PrisonerId]).

new_prisoner(NumPrisoners,PrisonerId) ->
	prisoner({NumPrisoners,PrisonerId,0,{}}).
prisoner(State) ->
	%% Prisoner state: number of days imprisoned
	%% am i leader, numtimes visited yard, 
	receive
		{From, {yardDay, LightPid}} ->
			prisoner(modulo_day_logic(From, LightPid, State));
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

do_nothing({Total,ID,Days_in_prison,MethodState}) -> {Total,ID,Days_in_prison+1,MethodState}.

years(Number_of_days) ->
	Number_of_days / 365.


modulo_day_logic(Guard,LightPid,State) ->
	{TotalPrisoners,ID,DayCount,MethodState} = State,
	PrisonersVisited = case MethodState of
		{} ->
			[ID];
		X ->
			X
	end,

	% Days are numbered Day%NumPrisoners
	CurrentDay = DayCount + TotalPrisoners,

	% If a prisoner goes to yard on the same daynumber as their prisoner number
	%	% then turn on light
	%	% else turn it off
	% When prisoner goes to the yard, if the light is on, they know that prisoner number daynumber-1
	% has been in. If the light is on, turn off the light. 
	case check_light(LightPid) of
		on -> 
			PrisonerVisitedReturn = PrisonersVisited++[CurrentDay-1],
			case length(PrisonerVisitedReturn)=:=TotalPrisoners of
				true ->
					Guard ! {self(), {all_inmates_visited}};
				false ->
					ok
			end,
			case CurrentDay=:=ID of
				true -> ok;
				false -> toggle_light(LightPid)
			end;
		off when CurrentDay=:=ID -> 
			toggle_light(LightPid);
		off -> ok
	end.


yard_logic2(Guard, LightPid, State) ->
% The prisoner is the leader if he goes into the exercise yard on the second day
% Light's initial position is unknown but will be set to on by first prisoner in yard
% Leader will leave it in the off position always
% If the light is on, the leader turns it off and increments the count by one 
	{NumPrisoners,ID,Days_in_prison,MethodState} = State,
	{Havent_sent_signal,Leader,NumVisited} = case MethodState of
		{} ->
			{true,false,0};
		X ->
			X
	end,
	%{NumPrisoners,ID,Days_in_prison,{Havent_sent_signal,Leader,NumVisited}} = State,
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
	{NumPrisoners,ID,Days_in_prison+1,{Havent_sent_signalReturn, LeaderReturn, NumVisitedReturn}}.


yard_logic(Guard, LightPid, State) ->
% * Prisoner is leader if she goes into the 
% exercise yard on the first(0th) day
% * Light's initial position is unknown
% * Leader will leave it in the off position always
% * All other prisoners switch light on during their 
% first visit to yard
% * If light is on, leader turns it off and increments 
% the count
	{_,_,Days_in_prison,{_,Leader,_}} = State,
	case Leader orelse Days_in_prison=:=0 of
		true ->
			leader_logic(Guard,LightPid,State);
		false ->
			normal_prisoner_logic(Guard, LightPid, State)
	end.

normal_prisoner_logic(Guard, LightPid, State) ->
	{Total,ID,DaysIn,{Havent_sent_signal,C,D}} = State,
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
	{Total,ID,DaysIn,{Havent_sent_signalReturn,C,D}}.


leader_logic(Guard,LightPid,State) ->
	{NumPrisoners,ID,Days_in_prison,{_,_,NumVisited}}=State,
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
	{NumPrisoners,ID,Days_in_prison+1,{false,true,NumVisitedReturn}}.

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
