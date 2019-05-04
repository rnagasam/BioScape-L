-module(channel).
-export([build_channels/1, channel/4]).

pick_random([]) ->
    error({pick_random, "No elements to pick from"});
pick_random(Ls) ->
    Index = rand:uniform(length(Ls)),
    lists:nth(Index, Ls).

build_channels([]) ->
    [];
build_channels([{C, Radius}|Cs]) ->
    CPid = spawn(?MODULE, channel, [C, [], queue:new(), Radius]),
    [{C, CPid} | build_channels(Cs)].

get_location(Proc) ->
    case whereis(simul) of
	undefined -> error({no_simulation_running});
	SPid -> SPid ! {get_location, Proc, self()}
    end,
    receive
	{ok, {_Name, Loc}} -> Loc;
	error -> error({location_not_found, Proc})
    end.

can_react(P, Q, Radius) ->
    PLoc = get_location(P),
    QLoc = get_location(Q),
    geom:within(PLoc, QLoc, Radius).

channel(Name, Listeners, MsgBox, Radius) ->
    receive
	{send, SName, SPid, Msg} ->
	    case Listeners of
		[] ->
		    Msgs = queue:in({SPid, SName, Msg}, MsgBox),
		    channel(Name, Listeners, Msgs, Radius);
		_ ->
		    {R, RPid} = pick_random(Listeners),
		    Ls = case can_react(SPid, RPid, Radius) of
			     true -> RPid ! {self(), SName, Msg},
				     SPid ! {msg_sent},
				     lists:delete({R, RPid}, Listeners);
			     _ -> Listeners
			 end,
		    channel(Name, Ls, MsgBox, Radius)
	    end;
	{recv, RName, RPid} ->
	    case queue:is_empty(MsgBox) of
		true ->
		    Ls = [{RName, RPid} | Listeners],
		    channel(Name, Ls, MsgBox, Radius);
		_ -> % TODO: Make random choice, don't send to latest recv'r.
		    {{value, {SPid, SName, Msg}}, Q} = queue:out(MsgBox),
		    case can_react(SPid, RPid, Radius) of
			true -> RPid ! {self(), SName, Msg},
				SPid ! {msg_sent};
			_ -> ok
		    end,
		    channel(Name, Listeners, Q, Radius)
	    end
    end.
