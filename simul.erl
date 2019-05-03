-module(simul).
-export([simul/3]).
-define(TIMEOUT, 5000).

% For now, `simul' is used to cleanup channels after finishing a run.
% `simul' additionally maintains a dictionary of process info objects,
% with entity Pids as keys and locations as values.  It is the
% responsibility of the process/entity to update `simul' with its
% location information.
simul(Chans, 0, ProcsInfo) ->
    io:format("Simul: ~p~n", [dict:to_list(ProcsInfo)]),
    dict:map(fun (Pid, _) -> exit(Pid, kill) end, ProcsInfo),
    [exit(C, kill) || { _, C } <- Chans];
simul(Chans, N, ProcsInfo) ->
    receive
	{ done, ProcPid } ->
	    io:format("Simul: ~p is done.~n", [ProcPid]),
	    simul(Chans, N-1, ProcsInfo);
	    % simul(Chans, N-1, dict:erase(ProcPid, ProcsInfo));
	{ create, ProcPid, Location } ->
	    simul(Chans, N+1, dict:store(ProcPid, Location, ProcsInfo));
	{ update, ProcPid, Location } ->
	    simul(Chans, N, dict:store(ProcPid, Location, ProcsInfo));
	{ get_location, ProcPid, From } ->
	    % `channel' should check for failure in recv'd message
	    From ! dict:find(ProcPid, ProcsInfo),
	    simul(Chans, N, ProcsInfo);
	{ inspect_state } ->
	    io:format("Simul: N: ~p, ProcsInfo: ~p~n",
		      [N, dict:to_list(ProcsInfo)]),
	    simul(Chans, N, ProcsInfo)
    after ?TIMEOUT ->
	    io:format("Simul: exit"),
	    simul(Chans, 0, ProcsInfo)
    end.
