-module(pi).
-export([run/2, spawn_entity/3, parse_string/1, get_tokens/2, parse_file/1, run_file/1]).
-register(simul).
-define(DEFAULT_STEP_SIZE, 5).

run_file(FileName) ->
    {Prog, StepSize} = simplify(parse_file(FileName)),
    run(Prog, StepSize).

run([prog, ChanDefs, ProcDefs, RunCmds], StepSize) ->
    Chans = channel:build_channels(ChanDefs),
    Procs = [{Name, eval:build_process(Name, Def), Geom}
	     || {define, Name, Geom, Def} <- ProcDefs],
    NProcs = lists:foldr(fun ({_P, X}, Acc) -> X + Acc end, 0, RunCmds),
    ChansEnv = [{chan, C, Chan} || {C, Chan} <- Chans],
    ProcsEnv = [{proc, P, {Proc, PGeom}} || {P, Proc, PGeom} <- Procs],
    Simul = spawn(simul, simul, [Chans, NProcs, "/tmp/bioscape.out", StepSize]),
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

simplify([[{chans, Chans}, Definitions], Commands]) ->
    Chns = lists:map(fun walk/1, Chans),
    Defs = lists:map(fun walk/1, Definitions),
    Coms = lists:map(fun walk/1, Commands),
    case lists:keyfind(step, 1, Coms) of
	{step, StepSize} ->
	    {[prog, Chns, Defs, lists:delete({step, StepSize}, Coms)],
	     StepSize};
	_ ->
	    {[prog, Chns, Defs, Coms], ?DEFAULT_STEP_SIZE}
    end.

walk({name, X}) ->
    X;
walk({num, N}) ->
    N;
walk({id, X}) ->
    X;
walk({this}) ->
    this;
walk({run, Name, Num}) ->
    {walk(Name), floor(walk(Num))};
walk({step, Num}) ->
    {step, floor(walk(Num))};
walk({send, Name, Msg, Exp}) ->
    {send, walk(Name), walk(Msg), walk(Exp)};
walk({recv, Name, Var, Exp}) ->
    {recv, walk(Name), walk(Var), walk(Exp)};
walk({spawn, Ents, Exp}) ->
    {spawn, lists:map(fun (X) -> walk(X) end, Ents), walk(Exp)};
walk({choice, Options}) ->
    {choice, lists:map(fun (X) -> walk(X) end, Options)};
walk({define, Name, Geom, Exps}) ->
    {define, walk(Name), walk(Geom), walk(Exps)};
walk({A, B, C}) ->
    {walk(A), walk(B), walk(C)};
walk({A, B}) ->
    {walk(A), walk(B)};
walk(Other) ->
    Other.
