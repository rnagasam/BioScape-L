-module(pi).
-export([run/1, test_prog/0, test_prog1/0, test_prog2/0, test_prog3/0, parse_string/1]).

run([prog, ChanNames, ProcDefs, RunCmds]) ->
    Chans = channel:build_channels(ChanNames),
    Procs = [{ Name, eval:build_process(Name, Def) }
	     || { define, Name, _Geom, Def } <- ProcDefs],
    NProcs = lists:foldr(fun ({ _P, X }, Acc) -> X + Acc end, 0, RunCmds),
    ChansEnv = [{ chan, C, Chan } || { C, Chan } <- Chans],
    ProcsEnv = [{ proc, P, Proc } || { P, Proc } <- Procs],
    Simul = spawn(simul, simul, [Chans, NProcs]),
    register(simul, Simul),
    [spawn_entity(P, N, ChansEnv ++ ProcsEnv) || { P, N } <- RunCmds].

spawn_entity(P, N, InitEnv) ->
    case lists:keyfind(P, 2, InitEnv) of
	{ proc, _P, Proc } ->
	    [spawn(eval, eval, [Proc, InitEnv]) || _ <- lists:seq(1, N)],
	    io:format("Run: spawn'd ~p ~p's~n", [N, P]);
	_Else ->
	    error({entity_not_found, { P, InitEnv }})
    end.

parse_string(Str) ->
    {_ResultL, Tks, _L} = lexer:string(Str),
    parser:parse(Tks).

test_prog() ->
    [prog, [a],
     [{define, p, geomP, {send, a, "ack", {null}}},
      {define, q, geomQ, {recv, a, x, {null}}}],
     [{p, 1}, {q, 1}]].

test_prog1() -> % A + B <-> C
    [prog, [a, b],
     [{define, procA, geomA, {send, a, "ack", {null}}},
      {define, procB, geomB, {recv, a, ack, {send, b, "ack", {spawn, [procC], {null}}}}},
      {define, procC, geomC, {recv, b, ack, {spawn, [procA, procB], {null}}}}],
     [{procA, 1}, {procB, 1}]].

test_prog2() ->
    [prog, [a],
     [{define, procA, geomA, {send, a, "ack", {recv, a, ack, {null}}}},
      {define, procB, geomB, {recv, a, ack, {null}}}],
     [{procA, 1}, {procB, 1}]].

test_prog3() ->
    [prog, [a],
     [{define, procA, geomA, {send, a, a, {recv, a, ack, {null}}}},
      {define, procB, geomB, {recv, a, x, {send, x, "ack", {null}}}}],
     [{procA, 1}, {procB, 1}]].
