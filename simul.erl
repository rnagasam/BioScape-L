-module(simul).
-export([simul/3]).
-define(TIMEOUT, 5000).

simul(Chans, N, ProcsInfo) ->
    simul(Chans, N, ProcsInfo, 0).

simul(Chans, 0, ProcsInfo, Time) ->
    io:format("Simul: ~p~n", [dict:to_list(ProcsInfo)]),
    io:format("Simulation: ran for ~p time steps~n", [Time]),
    dict:map(fun (Pid, _) -> exit(Pid, kill) end, ProcsInfo),
    [exit(C, kill) || { _, C } <- Chans];
simul(Chans, N, ProcsInfo, Time) ->
    receive
	{ done, ProcPid } ->
	    io:format("Simul: ~p is done.~n", [ProcPid]),
	    % simul(Chans, N-1, ProcsInfo, Time+1);
	    simul(Chans, N-1, dict:erase(ProcPid, ProcsInfo), Time+1);
	{ create, ProcPid, Location } ->
	    simul(Chans, N+1, dict:store(ProcPid, Location, ProcsInfo), Time+1);
	{ update, ProcPid, Location } ->
	    simul(Chans, N, dict:store(ProcPid, Location, ProcsInfo), Time+1);
	{ get_location, ProcPid, From } ->
	    % `channel' should check for failure in recv'd message
	    From ! dict:find(ProcPid, ProcsInfo),
	    simul(Chans, N, ProcsInfo, Time+1);
	{ inspect_state } ->
	    io:format("Simul: N: ~p, ProcsInfo: ~p~n",
		      [N, dict:to_list(ProcsInfo)]),
	    simul(Chans, N, ProcsInfo, Time)
    after ?TIMEOUT ->
	    io:format("Simul: exit"),
	    simul(Chans, 0, ProcsInfo, Time)
    end.
