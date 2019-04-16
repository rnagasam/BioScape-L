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
	{send, PName, PPid, Msg} ->
	    case Listeners of
		[] -> channel(Name, Listeners, 
			      queue:in({PPid, PName, Msg}, MsgBox));
		_ -> {Q, QPid} = pick_random(Listeners),
		     QPid ! {self(), PName, Msg},
		     PPid ! {msg_sent},
		     Ls = lists:delete({Q, QPid}, Listeners),
		     channel(Name, Ls, MsgBox)
	    end;
	{recv, PName, PPid} ->
	    case queue:is_empty(MsgBox) of
		true -> channel(Name, [{PName, PPid}|Listeners], MsgBox);
		false -> {{value, {QPid, QName, Msg}}, Q} = queue:out(MsgBox),
			 PPid ! {self(), QName, Msg},
			 QPid ! {msg_sent},
			 channel(Name, Listeners, Q)
	    end
    end.

build_channels([]) ->
    [];
build_channels([C|Cs]) ->
    CPid = spawn(?MODULE, channel, [C, [], queue:new()]),
    [{C, CPid} | build_channels(Cs)].

value([prog, Cs, Ps]) ->
    InitEnv = build_channels(Cs) ++ Ps,
    [spawn(?MODULE, value, [Name, P, InitEnv]) || {Name, P} <- Ps].

value(Name, {null}, _) ->
    io:format("Process ~p terminated.~n", [Name]),
    exit(normal);
value(Name, {send, Chan, Msg, P}, Env) ->
    CPid = lookup(Chan, Env),
    CPid ! {send, Name, self(), Msg},
    io:format("Process ~p sent message ~p on chan ~p.~n", [Name, Msg, Chan]),
    receive
	{msg_sent} -> value(Name, P, Env)
    end;
value(Name, {recv, Chan, Bind, P}, Env) ->
    CPid = lookup(Chan, Env),
    CPid ! {recv, Name, self()},
    io:format("Process ~p waiting to recv message on chan ~p.~n",
	      [Name, Chan]),
    receive
	{CPid, _, Msg} ->
	    io:format("Process ~p recv'd message ~p on chan ~p.~n",
		      [Name, Msg, Chan]),
	    value(Name, P, [{Bind, Msg} | Env])
    end;
value(Name, {spawn, Ps}, Env) ->
    Procs = lists:map(fun(X) -> lookup(X, Env) end, Ps),
    [spawn(?MODULE, value, [P, Proc, Env]) || {P, Proc} <- lists:zip(Ps, Procs)],
    io:format("Process ~p spawn'd processes ~p.~n", [Name, Ps]),
    exit(normal).

test_prog() ->
    [prog, [a], [{p, {send, a, ack, {null}}}, {q, {recv, a, x, {null}}}]].

% A + B <-> C
test_prog1() ->
    [prog, [a, b], [{procA, {send, a, ack, {null}}},
		    {procB, {recv, a, ack, {send, b, ack, {spawn, [procC]}}}},
		    {procC, {recv, b, ack, {spawn, [procA, procB]}}}]].

