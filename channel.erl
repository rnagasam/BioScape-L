-module(channel).
-export([build_channels/1, channel/4]).
-define(DEFAULT_RADIUS, 1).

pick_random([]) ->
    error({pick_random, "No elements to pick from"});
pick_random(Ls) ->
    Index = rand:uniform(length(Ls)),
    lists:nth(Index, Ls).

build_channels([]) ->
    [];
build_channels([C|Cs]) ->
    CPid = spawn(?MODULE, channel, [C, [], queue:new(), ?DEFAULT_RADIUS]),
    [{ C, CPid } | build_channels(Cs)].

get_location(Proc) ->
    case whereis(simul) of
	undefined -> error({no_simulation_running});
	SPid -> SPid ! { get_location, Proc, self() }
    end,
    receive
	{ ok, Loc } -> Loc;
	error -> error({location_not_found, Proc})
    end.

channel(Name, Listeners, MsgBox, Radius) ->
    receive
	{ send, ProcName, ProcPid, Msg } ->
	    case Listeners of
		[] ->
		    Msgs = queue:in({ ProcPid, ProcName, Msg }, MsgBox),
		    channel(Name, Listeners, Msgs, Radius);
		_ ->
		    % Get current sender location
		    SenderLoc = get_location(ProcPid),
		    { Q, QPid } = pick_random(Listeners),
		    QLoc = get_location(QPid),
		    io:format("Channel: Location of ~p: ~p~n", [Q, QLoc]),
		    QPid    ! { self(), ProcName, Msg },
		    ProcPid ! { msg_sent },
		    Ls = lists:delete({ Q, QPid }, Listeners),
		    channel(Name, Ls, MsgBox, Radius)
	    end;
	{ recv, ProcName, ProcPid } ->
	    case queue:is_empty(MsgBox) of
		true ->
		    Ls = [{ ProcName, ProcPid } | Listeners],
		    channel(Name, Ls, MsgBox, Radius);
		_ -> % TODO: Make random choice, don't send to latest recv'r.
		    {{ value, { QPid, QName, Msg }}, Q } = queue:out(MsgBox),
		    ProcPid ! { self(), QName, Msg },
		    SenderLoc = get_location(QPid),
		    ProcLoc = get_location(ProcPid),
		    QPid    ! { msg_sent },
		    channel(Name, Listeners, Q, Radius)
	    end
    end.
