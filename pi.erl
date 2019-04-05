-module(pi).
-compile([export_all]).

% TERM ::= { null }
%        | { send, Chan, Msg, TERM }
%        | { recv, Chan, Msg, TERM }
%        | { new, Chan, TERM }
%        | { simul, TERM, TERM }
%        | { repeat, TERM }

freevars({null}) ->
    sets:new();
freevars({send, Chan, Msg, P}) ->
    sets:union(sets:from_list([Chan, Msg]), freevars(P));
freevars({recv, Chan, Msg, P}) ->
    sets:union(sets:from_list([Chan]), sets:del_element(Msg, freevars(P)));
freevars({new, Chan, P}) ->
    sets:del_element(Chan, freevars(P));
freevars({simul, P, Q}) ->
    sets:union(freevars(P), freevars(Q));
freevars({repeat, P}) ->
    freevars(P).

fv(Expr) ->
    sets:to_list(freevars(Expr)).

pick_random([]) ->
    error({pick_random, "No elements to pick from"});
pick_random(Ls) ->
    Index = rand:uniform(length(Ls)),
    lists:nth(Index, Ls).

make_channel(Name) ->
    make_channel(Name, []).

make_channel(Name, Listeners) ->
    receive
	{ send, Msg } ->
	    {_, Pid} = pick_random(Listeners),
	    Pid ! { Msg };
	{ recv, ProcName, ProcPid } ->
	    Proc = { ProcName, ProcPid },
	    make_channel(Name, [Proc | Listeners]);
	{ inspect } ->
	    io:format("Chan ~p: Listeners: ~w~n", [Name, Listeners])
    end,
    make_channel(Name, Listeners).

% build_process should return a function which takes an environment as
% its argument.  This function represents a process in the
% pi-calculus.
build_process(_) ->
    error(not_implemented).

% eval: Program List * Process List * Channel List -> Atom
eval([], _, _) ->
    ok;
eval([{define, P, Actions}|Prog], Procs, Chans) ->
    eval(Prog, [{P, build_process(Actions)} | Procs], Chans);
eval([{simul, P, Q}|Prog], Procs, Chans) ->
    spawn(?MODULE, lists:keyfind(P, 1, Procs), [Chans]),
    spawn(?MODULE, lists:keyfind(Q, 1, Procs), [Chans]),
    eval(Prog, Procs, Chans).
