-module(pi).
-export([run/1, spawn_entity/3, parse_string/1]).
-register(simul).

run([prog, ChanDefs, ProcDefs, RunCmds]) ->
    Chans = channel:build_channels(ChanDefs),
    Procs = [{Name, eval:build_process(Name, Def), Geom}
	     || {define, Name, Geom, Def} <- ProcDefs],
    NProcs = lists:foldr(fun ({_P, X}, Acc) -> X + Acc end, 0, RunCmds),
    ChansEnv = [{chan, C, Chan} || {C, Chan} <- Chans],
    ProcsEnv = [{proc, P, {Proc, PGeom}} || {P, Proc, PGeom} <- Procs],
    Simul = spawn(simul, simul, [Chans, NProcs, dict:new(), "/tmp/bioscape.out"]),
    register(simul, Simul),
    [spawn_entity(P, N, ChansEnv ++ ProcsEnv) || {P, N} <- RunCmds],
    ok.

spawn_entity(P, N, InitEnv) ->
    case lists:keyfind(P, 2, InitEnv) of
	{proc, _P, {Proc, PGeom}} ->
	    [spawn(eval, eval, [Proc, InitEnv, PGeom]) || _ <- lists:seq(1, N)];
	_Else ->
	    error({entity_not_found, {P, InitEnv}})
    end.

parse_string(Str) ->
    {_ResultL, Tks, _L} = lexer:string(Str),
    parser:parse(Tks).
