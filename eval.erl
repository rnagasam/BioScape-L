-module(eval).
-export([lookup/2, build_process/2, eval/2]).

eval(P, Env) ->
    P(Env).

lookup(Key, Env) ->
    case lists:keyfind(Key, 2, Env) of
	{ Type, _Key, Val } ->
	    {Type, Val};
	_Else ->
	    error({key_not_found, Key, Env})
    end.

build_process(Name, { null }) ->
    fun (_Env) ->
	    whereis(simul) ! { done },
	    io:format("Process ~p terminated.~n", [Name])
    end;
build_process(Name, { send, Chan, Msg, P }) ->
    PProc = build_process(Name, P),
    fun (Env) ->
	    { _, CPid } = lookup(Chan, Env),
	    case is_atom(Msg) of
		true  -> { _, Val } = lookup(Msg, Env),
			 CPid ! { send, Name, self(), Val };
		false -> CPid ! { send, Name, self(), Msg }
	    end,
	    io:format("Process ~p sent message ~p on chan ~p.~n",
		      [Name, Msg, Chan]),
	    receive
		{ msg_sent } ->
		    PProc(Env)
	    end
    end;
build_process(Name, { recv, Chan, Bind, P }) ->
    PProc = build_process(Name, P),
    fun (Env) ->
	    { chan, CPid } = lookup(Chan, Env),
	    CPid ! { recv, Name, self() },
	    io:format("Process ~p waiting to recv message on chan ~p.~n",
		      [Name, Chan]),
	    receive
		{ CPid, _ProcName, Msg } ->
		    io:format("Process ~p recv'd message ~p on chan ~p.~n",
			      [Name, Msg, Chan]),
		    PProc([{ var, Bind, Msg } | Env])
	    end
    end;
build_process(Name, { spawn, Ps, Q }) ->
    QProc = build_process(Name, Q),
    fun (Env) ->
	    Procs = [lookup(P, Env) || P <- Ps],
	    [spawn(?MODULE, eval, [Proc, Env]) || { _, Proc } <- Procs],
	    [whereis(simul) ! { create } || _P <- Ps],
	    io:format("Process ~p spawn'd processes ~p.~n", [Name, Ps]),
	    QProc(Env)
    end.
