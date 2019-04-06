-module(pi).
-compile([export_all]).

% TERM ::= { null }
%        | { send, Chan, Msg, TERM }
%        | { recv, Chan, Msg, TERM }
%        | { new, Chan, TERM }
%        | { simul, TERM, TERM }
%        | { repeat, TERM }

freevars({null}) ->
    sets:new();
freevars({send, Chan, Msg, P}) ->
    sets:union(sets:from_list([Chan, Msg]), freevars(P));
freevars({recv, Chan, Msg, P}) ->
    sets:union(sets:from_list([Chan]), sets:del_element(Msg, freevars(P)));
freevars({new, Chan, P}) ->
    sets:del_element(Chan, freevars(P));
freevars({simul, P, Q}) ->
    sets:union(freevars(P), freevars(Q));
freevars({repeat, P}) ->
    freevars(P).

fv(Expr) ->
    sets:to_list(freevars(Expr)).

pick_random([]) ->
    error({pick_random, "No elements to pick from"});
pick_random(Ls) ->
    Index = rand:uniform(length(Ls)),
    lists:nth(Index, Ls).

lookup(Key, Env) ->
    case lists:keyfind(Key, 1, Env) of
	{_, Val} -> Val;
	_Else -> error({key_not_found, Key, Env})
    end.

channel(Name, Listeners, MsgBox) ->
    receive
	{send, PName, Msg} ->
	    case Listeners of
		[] -> channel(Name, Listeners, 
			      queue:in({self(), PName, Msg}, MsgBox));
		_ -> {Q, QPid} = pick_random(Listeners),
		     QPid ! {self(), PName, Msg},
		     lists:delete({Q, QPid}, Listeners)
	    end;
	{recv, PName, PPid} ->
	    case queue:is_empty(MsgBox) of
		true -> channel(Name, [{PName, PPid}|Listeners], MsgBox);
		false -> {value, Msg} = queue:out(MsgBox),
			 PPid ! Msg
	    end
    end,
    channel(Name, Listeners, MsgBox).

build_channels([]) ->
    [];
build_channels([C|Cs]) ->
    CPid = spawn(?MODULE, channel, [C, [], queue:new()]),
    [{C, CPid} | build_channels(Cs)].

value([prog, Cs, Ps]) ->
    InitEnv = build_channels(Cs) ++ Ps,
    [spawn(?MODULE, value, [Name, P, InitEnv]) || {Name, P} <- Ps].

value(Name, {null}, _) ->
    io:format("Process ~p terminated.", [Name]),
    exit(normal);
value(Name, {send, Chan, Msg, P}, Env) ->
    CPid = lookup(Chan, Env),
    CPid ! {send, Name, Msg},
    receive
	{msg_sent} -> value(Name, P, Env)
    end;
value(Name, {recv, Chan, Bind, P}, Env) ->
    CPid = lookup(Chan, Env),
    CPid ! {recv, Name, self()},
    receive
	{CPid, _, Msg} ->
	    value(Name, P, [{Bind, Msg} | Env])
    end;
value(_, {spawn, P}, Env) ->
    Proc = lookup(P, Env),
    spawn(?MODULE, value, [P, Proc, Env]),
    exit(normal).
