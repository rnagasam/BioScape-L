-module(simul).
-export([simul/2]).

% For now, `simul' is used to cleanup channels after finishing a run.
simul(Chans, 0) ->
    [exit(C, kill) || { _, C } <- Chans];
simul(Chans, N) ->
    receive
	{ done } -> simul(Chans, N-1);
	{ create } -> simul(Chans, N+1)
    end.
