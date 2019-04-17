-module(pi).
-compile([export_all]).

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

value([prog, Cs, Ps]) ->
    InitEnv = channel:build_channels(Cs) ++ Ps,
    [spawn(eval, eval, [Name, P, InitEnv]) || { Name, P } <- Ps].

test_prog() ->
    [prog, [a], [{p, {send, a, ack, {null}}}, {q, {recv, a, x, {null}}}]].

% A + B <-> C
test_prog1() ->
    [prog, [a, b], [{procA, {send, a, ack, {null}}},
		    {procB, {recv, a, ack, {send, b, ack, {spawn, [procC]}}}},
		    {procC, {recv, b, ack, {spawn, [procA, procB]}}}]].

test_prog2() ->
    [prog, [a], [{procA, {send, a, ack, {recv, a, ack, {null}}}},
		 {procB, {recv, a, ack, {null}}}]].
