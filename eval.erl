-module(eval).
-export([lookup/2, build_process/2, eval/3]).

eval(Name, P, InitEnv) ->
    (build_process(Name, P))(InitEnv).

lookup(Key, Env) ->
    case lists:keyfind(Key, 1, Env) of
	{ _, Val } ->
	    Val;
	_Else ->
	    error({key_not_found, Key, Env})
    end.

build_process(Name, { null }) ->
    fun (_) ->
	    whereis(simul) ! { done },
	    io:format("Process ~p terminated.~n", [Name])
    end;
build_process(Name, { send, Chan, Msg, P }) ->
    PProc = build_process(Name, P),
    fun (Env) ->
	    CPid = lookup(Chan, Env),
	    CPid ! { send, Name, self(), Msg },
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
	    CPid = lookup(Chan, Env),
	    CPid ! { recv, Name, self() },
	    io:format("Process ~p waiting to recv message on chan ~p.~n",
		      [Name, Chan]),
	    receive
		{ CPid, _, Msg } ->
		    io:format("Process ~p recv'd message ~p on chan ~p.~n",
			      [Name, Msg, Chan]),
		    PProc([{ Bind, Msg } | Env])
	    end
    end;
build_process(Name, { spawn, Ps }) ->
    fun (Env) ->
	    Procs = lists:map(fun (X) -> lookup(X, Env) end, Ps),
	    [spawn(?MODULE, eval, [P, Proc, Env])
	     || { P, Proc } <- lists:zip(Ps, Procs)],
	    [whereis(simul) ! { create } || _ <- Ps],
	    io:format("Process ~p spawn'd processes ~p.~n", [Name, Ps])
    end.
