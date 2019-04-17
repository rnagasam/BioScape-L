-module(pi).
-export([value/1, test_prog/0, test_prog1/0, test_prog2/0]).

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
