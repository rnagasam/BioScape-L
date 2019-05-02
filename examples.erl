-module(examples).
-compile(export_all).

prog1() ->
    [prog, [a],					% channels

     [{define, p, geomP,			% definitions
       {move,
	{send, a, this,
	 {null}}}},

      {define, q, geomQ,
       {recv, a, x,
	{null}}}],

     [{p, 1}, {q, 1}]].				% run commands

prog2() ->
    [prog, [a],

     [{define, procA, geomA,
       {send, a, "ack",
	{null}}},

      {define, procB, geomB,
       {recv, a, ack,
	{spawn, [procC],
	 {null}}}},

      {define, procC, geomC,
       {spawn, [procA, procB],
	{null}}}],

     [{procA, 1}, {procB, 1}]].


prog3() ->
    [prog, [a],

     [{define, procA, geomA,
       {send, a, "ack",
	{recv, a, ack,
	 {null}}}},

      {define, procB, geomB,
       {recv, a, ack, {null}}}],

     [{procA, 1}, {procB, 1}]].

prog4() ->
    [prog, [a],

     [{define, procA, geomA,
       {send, a, a,
	{recv, a, ack, {null}}}},

      {define, procB, geomB,
       {recv, a, x,
	{send, x, "ack", {null}}}}],

     [{procA, 1}, {procB, 1}]].

prog5() ->
    [prog, [a],

     [{define, procA, geomA,
       {choice, [
		 {send, a, "ack", {null}},
		 {recv, a, "ack", {null}}
	       ]}}],

     [{procA, 100}]].

prog6() ->
    [prog, [a, b],

     [{define, procA, geomA,
       {choice, [
		{send, a, "ack", {null}},
		{send, b, "ack", {null}}
		]}},

      {define, procB, geomB,
       {recv, b, ack, {null}}},

      {define, procC, geomC,
       {recv, a, ack, {null}}},

      {define, procD, geomD,
       {choice, [
		{send, a, "ack", {null}},
		{send, b, "ack", {null}}
		]}}],

     [{procA, 1}, {procB, 1}, {procC, 1}, {procD, 1}]].

prog7() ->
    [prog, [a],

     [{define, procA, geomA,
       {spawn, [procB], {null}}},

      {define, procB, geomB,
       {send, a, "ack",
	{recv, a, ack,
	 {null}}}}],

    [{procA, 1}]].
