-module(examples).
-compile(export_all).

prog1() ->
    [prog, [{a, 1}],				% channels

     [{define, p, origin,			% definitions
       {move,
	{send, a, this,
	 {null}}}},

      {define, q, origin,
       {recv, a, x,
	{null}}}],

     [{p, 1}, {q, 1}]].				% run commands

prog2() ->
    [prog, [{a, 1}],

     [{define, procA, origin,
       {send, a, "ack",
	{null}}},

      {define, procB, origin,
       {recv, a, ack,
	{spawn, [procC],
	 {null}}}},

      {define, procC, origin,
       {spawn, [procA, procB],
	{null}}}],

     [{procA, 1}, {procB, 1}]].


prog3() ->
    [prog, [{a, 1}],

     [{define, procA, origin,
       {send, a, "ack",
	{recv, a, ack,
	 {null}}}},

      {define, procB, origin,
       {recv, a, ack, {null}}}],

     [{procA, 1}, {procB, 1}]].

prog4() ->
    [prog, [{a, 1}],

     [{define, procA, origin,
       {send, a, a,
	{recv, a, ack, {null}}}},

      {define, procB, origin,
       {recv, a, x,
	{send, x, "ack", {null}}}}],

     [{procA, 1}, {procB, 1}]].

prog5() ->
    [prog, [{a, 1}],

     [{define, procA, origin,
       {choice, [
		 {send, a, "ack", {null}},
		 {recv, a, "ack", {null}}
	       ]}}],

     [{procA, 100}]].

prog6() ->
    [prog, [{a, 1}, {b, 1}],

     [{define, procA, origin,
       {choice, [
		{send, a, "ack", {null}},
		{send, b, "ack", {null}}
		]}},

      {define, procB, origin,
       {recv, b, ack, {null}}},

      {define, procC, origin,
       {recv, a, ack, {null}}},

      {define, procD, origin,
       {choice, [
		{send, a, "ack", {null}},
		{send, b, "ack", {null}}
		]}}],

     [{procA, 1}, {procB, 1}, {procC, 1}, {procD, 1}]].

prog7() ->
    [prog, [{a, 1}],

     [{define, procA, origin,
       {spawn, [procB], {null}}},

      {define, procB, origin,
       {send, a, "ack",
	{recv, a, ack,
	 {null}}}}],

    [{procA, 1}]].
