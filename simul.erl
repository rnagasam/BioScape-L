-module(simul).
-export([simul/3, write_state/2, geom_to_string/1]).
-define(TIMEOUT, 5000).
-define(STEP_SIZE, 5).

waitfor_entities(0, ProcsInfo) ->
    dict:map(fun (Pid, _) -> Pid ! ok end, ProcsInfo),
    ProcsInfo;
waitfor_entities(N, ProcsInfo) ->
    receive
	{ready, Name, From, Geom} ->
	    waitfor_entities(N-1, dict:store(From, {Name, Geom}, ProcsInfo))
    end.

simul(Chans, N, FilePath) ->
    ProcsInfo = waitfor_entities(N, dict:new()),
    {ok, Handle} = file:open(FilePath, [write]),
    io:format(Handle, "~B~n", [?STEP_SIZE]),
    Writer = spawn(?MODULE, write_state, [Handle, ?STEP_SIZE]),
    simul(Chans, N, ProcsInfo, 0, Writer),
    receive
	{close_file} ->
	    file:close(Handle)
    end.

simul(Chans, 0, ProcsInfo, Time, _Writer) ->
    io:format("Simulation: ran for ~p time steps~n", [Time]),
    dict:map(fun (Pid, _) -> exit(Pid, kill) end, ProcsInfo),
    [exit(C, kill) || {_, C} <- Chans];
simul(Chans, N, ProcsInfo, Time, Writer) ->
    receive
	{done, _Name, ProcPid} ->
	    Info = dict:erase(ProcPid, ProcsInfo),
	    Writer ! {Time, Info},
	    simul(Chans, N-1, Info, Time+1, Writer);
	{ready, Name, ProcPid, Location} ->
	    Info = dict:store(ProcPid, {Name, Location}, ProcsInfo),
	    Writer ! {Time, Info},
	    ProcPid ! ok,
	    simul(Chans, N+1, Info, Time+1, Writer);
	{update, Name, ProcPid, Location} ->
	    Info = dict:store(ProcPid, {Name, Location}, ProcsInfo),
	    Writer ! {Time, Info},
	    simul(Chans, N, Info, Time+1, Writer);
	{get_location, ProcPid, From} ->
	    % `channel' should check for failure in recv'd message
	    From ! dict:find(ProcPid, ProcsInfo),
	    simul(Chans, N, ProcsInfo, Time, Writer);
	{inspect_state} ->
	    io:format("Simul: N: ~p, ProcsInfo: ~p~n",
		      [N, dict:to_list(ProcsInfo)]),
	    simul(Chans, N, ProcsInfo, Time, Writer)
    after ?TIMEOUT ->
	    simul(Chans, 0, ProcsInfo, Time, Writer)
    end.

write_state(File, Step) ->
    receive
	{Time, Info} when Time rem Step =:= 0 ->
	    write_infos(File, Time, Info),
	    write_state(File, Step);
	{_Time, _Info} ->
	    write_state(File, Step)
    after 0 ->
	    whereis(simul) ! {close_file}
    end.

write_infos(File, Time, ProcsInfo) ->
    io:format(File, "~B~n", [Time]),
    dict:map(fun(_K, {Name, Geom}) ->
		     io:format(File, "\t~w\t~s~n", [Name, geom_to_string(Geom)])
	     end, ProcsInfo).

geom_to_string({geom, {pos, X, Y}, Rad}) ->
    io_lib:fwrite("~10.2f\t~10.2f\t~10.1f", [X, Y, Rad]).
