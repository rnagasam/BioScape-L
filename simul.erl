-module(simul).
-export([simul/2]).

% For now, `simul' is used to cleanup channels after finishing a run.
% `simul' additionally maintains a dictionary of process info objects,
% with entity Pids as keys and locations as values.  It is the
% responsibility of the process/entity to update `simul' with its
% location information.
simul(Chans, 0, _ProcsInfo) ->
    [exit(C, kill) || { _, C } <- Chans];
simul(Chans, N, ProcsInfo) ->
    receive
	{ done, ProcPid } ->
	    io:format("Simul: ~p is done.~n", [ProcPid]),
	    simul(Chans, N-1, dict:erase(ProcPid, ProcsInfo));
	{ create, ProcPid, Location } ->
	    simul(Chans, N+1, dict:store(ProcPid, Location, ProcsInfo));
	{ update, ProcPid, Location } ->
	    simul(Chans, N, dict:store(ProcPid, Location, ProcsInfo));
	{ get_location, ProcPid, From } ->
	    ProcLoc = dict:find(ProcPid, ProcsInfo),
	    % `channel' should check for failure in recv'd message
	    From ! { location, ProcLoc },
	    simul(Chans, N, ProcsInfo)
    end.
