-module(simul).
-export([simul/3, write_state/1]).
-define(TIMEOUT, 5000).

simul(Chans, N, ProcsInfo) ->
    Writer = spawn(?MODULE, write_state, [5]),
    simul(Chans, N, ProcsInfo, 0, Writer).

simul(Chans, 0, ProcsInfo, Time, Writer) ->
    Writer ! {Time, ProcsInfo, final},
    io:format("Simulation: ran for ~p time steps~n", [Time]),
    dict:map(fun (Pid, _) -> exit(Pid, kill) end, ProcsInfo),
    [exit(C, kill) || {_, C} <- Chans];
simul(Chans, N, ProcsInfo, Time, Writer) ->
    receive
	{done, _Name, ProcPid} ->
	    Info = dict:erase(ProcPid, ProcsInfo),
	    Writer ! {Time, Info},
	    simul(Chans, N-1, Info, Time+1, Writer);
	{create, Name, ProcPid, Location} ->
	    Info = dict:store(ProcPid, {Name, Location}, ProcsInfo),
	    Writer ! {Time, Info},
	    simul(Chans, N+1, Info, Time+1, Writer);
	{update, Name, ProcPid, Location} ->
	    Info = dict:store(ProcPid, {Name, Location}, ProcsInfo),
	    Writer ! {Time, Info},
	    simul(Chans, N, Info, Time+1, Writer);
	{get_location, ProcPid, From} ->
	    % `channel' should check for failure in recv'd message
	    From ! dict:find(ProcPid, ProcsInfo),
	    simul(Chans, N, ProcsInfo, Time+1, Writer);
	{inspect_state} ->
	    io:format("Simul: N: ~p, ProcsInfo: ~p~n",
		      [N, dict:to_list(ProcsInfo)]),
	    simul(Chans, N, ProcsInfo, Time, Writer)
    after ?TIMEOUT ->
	    io:format("Simul: exit"),
	    simul(Chans, 0, ProcsInfo, Time, Writer)
    end.

write_state(Step) ->
    receive
	{Time, Info, final} -> % final state of all entities
	    io:format("State: ~p~n", [[V || {_K, V} <- dict:to_list(Info)]]);
	{Time, Info} when Time rem Step =:= 0 ->
	    io:format("State: ~p~n", [[V || {_K, V} <- dict:to_list(Info)]]),
	    write_state(Step);
	{_Time, _Info} ->
	    write_state(Step)
    end.
