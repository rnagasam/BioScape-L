-module(pi).
-export([run/1, spawn_entity/3, parse_string/1, get_tokens/2, parse_file/1, simplify/1, simpl/1]).
-register(simul).
-define(STEP_SIZE, 5).

run([prog, ChanDefs, ProcDefs, RunCmds]) ->
    Chans = channel:build_channels(ChanDefs),
    Procs = [{Name, eval:build_process(Name, Def), Geom}
	     || {define, Name, Geom, Def} <- ProcDefs],
    NProcs = lists:foldr(fun ({_P, X}, Acc) -> X + Acc end, 0, RunCmds),
    ChansEnv = [{chan, C, Chan} || {C, Chan} <- Chans],
    ProcsEnv = [{proc, P, {Proc, PGeom}} || {P, Proc, PGeom} <- Procs],
    Simul = spawn(simul, simul, [Chans, NProcs, "/tmp/bioscape.out", ?STEP_SIZE]),
    register(simul, Simul),
    [spawn_entity(P, N, ChansEnv ++ ProcsEnv) || {P, N} <- RunCmds],
    ok.

spawn_entity(P, N, InitEnv) ->
    case lists:keyfind(P, 2, InitEnv) of
	{proc, _P, {Proc, PGeom}} ->
	    [spawn(eval, eval, [P, Proc, InitEnv, PGeom]) || _ <- lists:seq(1, N)];
	_Else ->
	    error({entity_not_found, {P, InitEnv}})
    end.

parse_string(Str) ->
    {_ResultL, Tks, _L} = lexer:string(Str),
    parser:parse(Tks).

get_tokens(InFile, Acc) ->
    case io:request(InFile, {get_until,prompt,lexer,token,[1]}) of
	{ok, Token, _EndLine} ->
	    get_tokens(InFile, Acc ++ [Token]);
	{error, token} ->
	    exit(scanning_error);
	{eof, _} ->
	    Acc
    end.

parse_file(FileName) ->
    {ok, InFile} = file:open(FileName, [read]),
    Acc = get_tokens(InFile,[]),
    file:close(InFile),
    {Result, AST} = parser:parse(Acc),
    case Result of
	ok -> AST;
	_ -> io:format("Parse error: ~p~n", [AST])
    end.
