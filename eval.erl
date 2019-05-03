-module(eval).
-export([lookup/2, build_process/2, eval/3]).
-define(DEFAULT_MOVE_LIMIT, 2).
-define(DEFAULT_DIFFUSE_RATE, 0.5).

eval(P, Env, Geom) ->
    PGeom = case Geom of
		origin -> geom:default();
		{geom,_Pos,_Radius} -> Geom;
		_ -> geom:from_tuple(Geom)
	    end,
    P(Env, PGeom).

update_location(Name, Pid, Loc) ->
    case whereis(simul) of
	undefined -> error({no_simulation_running});
	SPid -> SPid ! {update, Name, Pid, Loc}
    end.

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
	_ ->
	    geom:add_pos(Loc, geom:from_tuple(SpawnTo))
    end.

build_process(Name, {null}) ->
    fun (_Env, Geom) ->
	    whereis(simul) ! {done, Name, self()},
	    io:format("Process ~p terminated at ~p.~n",
		      [Name, geom:get_pos(Geom)])
    end;
build_process(Name, {send, Chan, Msg, P}) ->
    PProc = build_process(Name, P),
    fun (Env, Geom) ->
	    Loc = geom:random_translate(Geom, ?DEFAULT_DIFFUSE_RATE),
	    update_location(Name, self(), Loc),
	    CPid = get_channel(Chan, Env),
	    case Msg of
		this -> CPid ! {send, Name, self(), Loc};
		_ when is_atom(Msg) ->
		    {_, Val} = lookup(Msg, Env),
		    CPid ! {send, Name, self(), Val};
		_ -> CPid ! {send, Name, self(), Msg}
	    end,
	    io:format("Process ~p sent message ~p on chan ~p.~n",
		      [Name, Msg, Chan]),
	    receive
		{msg_sent} ->
		    PProc(Env, Loc)
	    end
    end;
build_process(Name, {recv, Chan, Bind, P}) ->
    PProc = build_process(Name, P),
    fun (Env, Geom) ->
	    Loc = geom:random_translate(Geom, ?DEFAULT_DIFFUSE_RATE),
	    update_location(Name, self(), Loc),
	    CPid = get_channel(Chan, Env),
	    CPid ! {recv, Name, self()},
	    io:format("Process ~p waiting to recv message on chan ~p.~n",
		      [Name, Chan]),
	    receive
		{CPid, _ProcName, Msg} ->
		    io:format("Process ~p recv'd message ~p on chan ~p.~n",
			      [Name, Msg, Chan]),
		    PProc([{var, Bind, Msg} | Env], Loc)
	    end
    end;
build_process(Name, {spawn, Ps, Q}) ->
    PProc = build_process(Name, Q),
    fun (Env, Geom) ->
	    Loc = geom:random_translate(Geom, ?DEFAULT_DIFFUSE_RATE),
	    update_location(Name, self(), Loc),
	    Procs = [{P, lookup(P, Env), SpawnTo} || {P, SpawnTo} <- Ps],
	    Ents = [{P, spawn(eval, eval, [Proc, Env, spawn_to_loc(Loc, SLoc)])}
		    || {P, {_,{Proc, _PGeom}}, SLoc} <- Procs],
	    %% TODO update with actual location of spawn'd process
	    %% (using move_loc)
	    [whereis(simul) ! {create, PNam, Pid, Loc} || {PNam, Pid} <- Ents],
	    io:format("Process ~p spawn'd processes ~p.~n", [Name, Ps]),
	    PProc(Env, Loc)
    end;
build_process(Name, {move, P}) ->
    PProc = build_process(Name, P),
    fun (Env, Geom) ->
	    Loc = geom:random_translate(Geom, ?DEFAULT_MOVE_LIMIT),
	    update_location(Name, self(), Loc),
	    PProc(Env, Loc)
    end;
build_process(Name, {choice, Ps}) ->
    fun (Env, Geom) ->
	    Route = lists:nth(rand:uniform(length(Ps)), Ps),
	    io:format("Chose: ~p~n", [Route]),
	    (build_process(Name, Route))(Env, Geom)
    end.
