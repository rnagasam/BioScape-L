-module(simul).
-export([simul/3]).
-define(TIMEOUT, 5000).

simul(Chans, N, ProcsInfo) ->
    simul(Chans, N, ProcsInfo, 0).

simul(Chans, 0, ProcsInfo, Time) ->
    io:format("Simul: ~p~n", [dict:to_list(ProcsInfo)]),
    io:format("Simulation: ran for ~p time steps~n", [Time]),
    dict:map(fun (Pid, _) -> exit(Pid, kill) end, ProcsInfo),
    [exit(C, kill) || {_, C} <- Chans];
simul(Chans, N, ProcsInfo, Time) ->
    receive
	{done, _Name, ProcPid} ->
	    io:format("Simul: ~p is done.~n", [ProcPid]),
	    Info = dict:erase(ProcPid, ProcsInfo),
	    simul(Chans, N-1, Info, Time+1);
	{create, Name, ProcPid, Location} ->
	    Info = dict:store(ProcPid, {Name, Location}, ProcsInfo),
	    simul(Chans, N+1, Info, Time+1);
	{update, Name, ProcPid, Location} ->
	    Info = dict:store(ProcPid, {Name, Location}, ProcsInfo),
	    simul(Chans, N, Info, Time+1);
	{get_location, ProcPid, From} ->
	    % `channel' should check for failure in recv'd message
	    From ! dict:find(ProcPid, ProcsInfo),
	    simul(Chans, N, ProcsInfo, Time+1);
	{inspect_state} ->
	    io:format("Simul: N: ~p, ProcsInfo: ~p~n",
		      [N, dict:to_list(ProcsInfo)]),
	    simul(Chans, N, ProcsInfo, Time)
    after ?TIMEOUT ->
	    io:format("Simul: exit"),
	    simul(Chans, 0, ProcsInfo, Time)
    end.
