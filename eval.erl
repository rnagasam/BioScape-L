-module(eval).
-export([lookup/2, build_process/2, eval/2]).
-define(DEFAULT_MOVE_LIMIT, 2).
-define(DEFAULT_DIFFUSE_RATE, 0.5).

eval(P, Env) ->
    InitGeom = geom:default(),
    P(Env, InitGeom).

lookup(Key, Env) ->
    case lists:keyfind(Key, 2, Env) of
	{ Type, _Key, Val } ->
	    {Type, Val};
	_Else ->
	    error({key_not_found, Key, Env})
    end.

get_channel(Chan, Env) ->
    case lookup(Chan, Env) of
	{ _Type, Ent } when is_pid(Ent) ->
	    Ent;
	Else -> error({not_a_channel, Else})
    end.

build_process(Name, { null }) ->
    fun (_Env, Geom) ->
	    whereis(simul) ! { done },
	    io:format("Process ~p terminated at ~p.~n",
		      [Name, geom:get_pos(Geom)])
    end;
build_process(Name, { send, Chan, Msg, P }) ->
    PProc = build_process(Name, P),
    fun (Env, Geom) ->
	    Loc = geom:random_translate(Geom, ?DEFAULT_DIFFUSE_RATE),
	    CPid = get_channel(Chan, Env),
	    case Msg of
		this -> CPid ! { send, Name, self(), none, Loc };
		_ when is_atom(Msg) ->
		    { _, Val } = lookup(Msg, Env),
		    CPid ! { send, Name, self(), Val, Loc };
		_ -> CPid ! { send, Name, self(), Msg, Loc }
	    end,
	    io:format("Process ~p sent message ~p on chan ~p.~n",
		      [Name, Msg, Chan]),
	    receive
		{ msg_sent } ->
		    PProc(Env, Loc)
	    end
    end;
build_process(Name, { recv, Chan, Bind, P }) ->
    PProc = build_process(Name, P),
    fun (Env, Geom) ->
	    Loc = geom:random_translate(Geom, ?DEFAULT_DIFFUSE_RATE),
	    CPid = get_channel(Chan, Env),
	    CPid ! { recv, Name, self(), Loc },
	    io:format("Process ~p waiting to recv message on chan ~p.~n",
		      [Name, Chan]),
	    receive
		{ CPid, _ProcName, Msg } ->
		    io:format("Process ~p recv'd message ~p on chan ~p.~n",
			      [Name, Msg, Chan]),
		    PProc([{ var, Bind, Msg } | Env], Loc)
	    end
    end;
build_process(Name, { spawn, Ps, Q }) ->
    PProc = build_process(Name, Q),
    fun (Env, Geom) ->
	    Loc = geom:random_translate(Geom, ?DEFAULT_DIFFUSE_RATE),
	    Procs = [lookup(P, Env) || P <- Ps],
	    [spawn(?MODULE, eval, [Proc, Env]) || { _, Proc } <- Procs],
	    [whereis(simul) ! { create } || _P <- Ps],
	    io:format("Process ~p spawn'd processes ~p.~n", [Name, Ps]),
	    PProc(Env, Loc)
    end;
build_process(Name, { move, P }) ->
    PProc = build_process(Name, P),
    fun (Env, Geom) ->
	    Loc = geom:random_translate(Geom, ?DEFAULT_MOVE_LIMIT),
	    PProc(Env, Loc)
    end;
build_process(Name, { choice, Ps }) ->
    fun (Env, Geom) ->
	    Route = lists:nth(rand:uniform(length(Ps)), Ps),
	    io:format("Chose: ~p~n", [Route]),
	    (build_process(Name, Route))(Env, Geom)
    end.
