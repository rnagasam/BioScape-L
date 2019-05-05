-module(eval).
-export([lookup/2, build_process/2, eval/4]).
-define(DEFAULT_MOVE_LIMIT, 10).
-define(DEFAULT_DIFFUSE_RATE, 10).
-define(START_TRANSLATE, 25).

get_simul() ->
    case whereis(simul) of
	undefined ->
	    error({no_simulation_running});
	SPid -> SPid
    end.

eval(Name, P, Env, Geom) ->
    PGeom = case Geom of
		origin -> geom:default();
		{geom,_Pos,_Radius} -> Geom;
		_ -> geom:from_tuple(Geom)
	    end,
    SPid = get_simul(),
    SPid ! {ready, Name, self(), PGeom},
    receive
	ok ->
	    P(Env, PGeom);
	move_again ->
	    InitGeom = geom:random_translate(PGeom, ?START_TRANSLATE),
	    SPid ! {ready, Name, self(), InitGeom},
	    eval(Name, P, Env, InitGeom)
    end.

update_location(Name, Pid, Loc) ->
    SPid = get_simul(),
    SPid ! {update, Name, Pid, Loc}.

lookup(Key, Env) ->
    case lists:keyfind(Key, 2, Env) of
	{Type, _Key, Val} ->
	    {Type, Val};
	_Else ->
	    error({key_not_found, Key, Env})
    end.

get_channel(Chan, Env) ->
    case lookup(Chan, Env) of
	{_Type, Ent} when is_pid(Ent) ->
	    Ent;
	Else -> error({not_a_channel, Else})
    end.

spawn_to_loc(Loc, SpawnTo) ->
    case SpawnTo of
	this ->
	    Loc;
	{geom,_Pos,_Radius} ->
	    SpawnTo;
	_ ->
	    geom:add_pos(Loc, geom:from_tuple(SpawnTo))
    end.

move_from(Loc, Lim) ->
    To = geom:random_translate(Loc, Lim),
    SPid = get_simul(),
    SPid ! {moveto, self(), To},
    receive
	ok ->
	    To;
	no ->
	    NewLoc = geom:random_translate(To, Lim),
	    move_from(NewLoc, Lim)
    end.

build_process(Name, {null}) ->
    fun (_Env, _Geom) ->
	    io:format("Proc ~p done~n", [Name]),
	    SPid = get_simul(),
	    SPid ! {done, Name, self()}
    end;
build_process(Name, {send, Chan, Msg, P}) ->
    PProc = build_process(Name, P),
    fun (Env, Geom) ->
	    Loc = move_from(Geom, ?DEFAULT_DIFFUSE_RATE),
	    update_location(Name, self(), Loc),
	    CPid = get_channel(Chan, Env),
	    case Msg of
		this -> CPid ! {send, Name, self(), Loc};
		_ when is_atom(Msg) ->
		    {_, Val} = lookup(Msg, Env),
		    CPid ! {send, Name, self(), Val};
		_ -> CPid ! {send, Name, self(), Msg}
	    end,
	    receive
		{msg_sent} ->
		    PProc(Env, Loc);
		{msg_dropped} ->
		    NewLoc = move_from(Loc, ?DEFAULT_DIFFUSE_RATE),
		    update_location(Name, self(), NewLoc),
		    PProc(Env, NewLoc)
	    end
    end;
build_process(Name, {recv, Chan, Bind, P}) ->
    PProc = build_process(Name, P),
    fun (Env, Geom) ->
	    Loc = move_from(Geom, ?DEFAULT_DIFFUSE_RATE),
	    update_location(Name, self(), Loc),
	    CPid = get_channel(Chan, Env),
	    CPid ! {recv, Name, self()},
	    receive
		{CPid, _ProcName, Msg} ->
		    PProc([{var, Bind, Msg} | Env], Loc)
	    end
    end;
build_process(Name, {spawn, Ps, Q}) ->
    PProc = build_process(Name, Q),
    fun (Env, Geom) ->
	    Loc = move_from(Geom, ?DEFAULT_DIFFUSE_RATE),
	    update_location(Name, self(), Loc),
	    Procs = entity_infos(Ps, Env, []),
	    [spawn(eval, eval, [P, Proc, Env, spawn_to_loc(Loc, SLoc)])
	     || {P, {_, {Proc, _PGeom}}, SLoc} <- Procs],
	    PProc(Env, Loc)
    end;
build_process(Name, {move, P}) ->
    PProc = build_process(Name, P),
    fun (Env, Geom) ->
	    Loc = move_from(Geom, ?DEFAULT_MOVE_LIMIT),
	    update_location(Name, self(), Loc),
	    PProc(Env, Loc)
    end;
build_process(Name, {choice, Ps}) ->
    fun (Env, Geom) ->
	    Route = lists:nth(rand:uniform(length(Ps)), Ps),
	    (build_process(Name, Route))(Env, Geom)
    end.

entity_infos([], _Env, Procs) ->
    Procs;
entity_infos([{EName, SpawnTo}|Ents], Env, Procs)
  when is_atom(SpawnTo), SpawnTo /= this ->
    {var, Val} = lookup(SpawnTo, Env),
    Proc = {EName, lookup(EName, Env), Val},
    entity_infos(Ents, Env, [Proc|Procs]);
entity_infos([{EName,SpawnTo}|Ents], Env, Procs) ->
    Proc = {EName, lookup(EName, Env), SpawnTo},
    entity_infos(Ents, Env, [Proc|Procs]).
