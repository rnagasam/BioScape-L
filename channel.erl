-module(channel).
-export([build_channels/1, channel/3]).

pick_random([]) ->
    error({pick_random, "No elements to pick from"});
pick_random(Ls) ->
    Index = rand:uniform(length(Ls)),
    lists:nth(Index, Ls).

build_channels([]) ->
    [];
build_channels([C|Cs]) ->
    CPid = spawn(?MODULE, channel, [C, [], queue:new()]),
    [{ C, CPid } | build_channels(Cs)].


channel(Name, Listeners, MsgBox) ->
    receive
	{ send, ProcName, ProcPid, Msg, _Location } ->
	    case Listeners of
		[] ->
		    Msgs = queue:in({ ProcPid, ProcName, Msg }, MsgBox),
		    channel(Name, Listeners, Msgs);
		_ ->
		    { Q, QPid } = pick_random(Listeners),
		    QPid    ! { self(), ProcName, Msg },
		    ProcPid ! { msg_sent },
		    Ls = lists:delete({ Q, QPid }, Listeners),
		    channel(Name, Ls, MsgBox)
	    end;
	{ recv, ProcName, ProcPid, _Location } ->
	    case queue:is_empty(MsgBox) of
		true ->
		    Ls = [{ ProcName, ProcPid } | Listeners],
		    channel(Name, Ls, MsgBox);
		false -> % TODO: Make random choice, don't send to latest recv'r.
		    {{ value, { QPid, QName, Msg }}, Q } = queue:out(MsgBox),
		    ProcPid ! { self(), QName, Msg },
		    QPid    ! { msg_sent },
		    channel(Name, Listeners, Q)
	    end
    end.
