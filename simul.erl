-module(simul).
-export([simul/4, write_state/2, geom_to_string/1, can_moveto/2]).
-define(TIMEOUT, 5000).
-define(MAX_SIMULATION_TIME, 10000).

waitfor_entities(0, ProcsInfo) ->
    dict:map(fun (Pid, _) -> Pid ! ok end, ProcsInfo),
    ProcsInfo;
waitfor_entities(N, ProcsInfo) ->
    receive
	{ready, Name, From, Geom} ->
	    case can_moveto(Geom, ProcsInfo) of
		true ->
		    Info = dict:store(From, {Name, Geom}, ProcsInfo),
		    waitfor_entities(N-1, Info);
		_ ->
		    From ! move_again,
		    waitfor_entities(N, ProcsInfo)
	    end;
	Msg -> io:format("Simul: got unknown message: ~p~n", [Msg])
    end.

can_moveto(ToLoc, ProcsInfo) ->
    Intersects = dict:filter(fun(_ProcPid, {_ProcName, ProcLoc}) ->
				     geom:intersects(ToLoc, ProcLoc)
			     end,
			     ProcsInfo),
    dict:is_empty(Intersects).

simul(Chans, N, FilePath, StepSize) ->
    ProcsInfo = waitfor_entities(N, dict:new()),
    {ok, Handle} = file:open(FilePath, [write]),
    Writer = spawn(?MODULE, write_state, [Handle, StepSize]),
    Writer ! {write_step},
    simul(Chans, N, ProcsInfo, 0, Writer).

simul(Chans, 0, ProcsInfo, Time, Writer) ->
    io:format("Simulation: ran for ~p time steps~n", [Time]),
    Writer ! {close_file},
    dict:map(fun (Pid, _) -> exit(Pid, kill) end, ProcsInfo),
    [exit(C, kill) || {_, C} <- Chans];
simul(Chans, N, ProcsInfo, Time, Writer) when Time < ?MAX_SIMULATION_TIME ->
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
	{moveto, ProcPid, Loc} ->
	    case can_moveto(Loc, dict:erase(ProcPid, ProcsInfo)) of
		true ->
		    ProcPid ! ok;
		_ ->
		    ProcPid ! no
	    end,
	    simul(Chans, N, ProcsInfo, Time, Writer);
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
    end;
simul(Chans, _N, ProcsInfo, Time, Writer) ->
    io:format("Simulation: max time limit reached~n"),
    simul(Chans, 0, ProcsInfo, Time, Writer).

write_state(File, Step) ->
    receive
	{write_step} ->
	    io:format(File, "~B~n", [Step]),
	    write_state(File, Step);
	{close_file} ->
	    file:close(File);
	{Time, Info} when Time rem Step =:= 0 ->
	    write_infos(File, Time, Info),
	    write_state(File, Step);
	{_Time, _Info} ->
	    write_state(File, Step);
	Msg -> io:format("Writer: Unknown message: ~p~n", [Msg])
    end.

write_infos(File, Time, ProcsInfo) ->
    io:format(File, "~B~n", [Time]),
    dict:map(fun(_K, {Name, Geom}) ->
		     io:format(File, "\t~w\t~s~n", [Name, geom_to_string(Geom)])
	     end, ProcsInfo).

geom_to_string({geom, {pos, X, Y}, Rad}) ->
    io_lib:fwrite("~10.2f\t~10.2f\t~10.1f", [X, Y, Rad]).
