-module(examples).
-compile(export_all).

prog1() ->
    [prog, [{a, 5}],				% channels

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
	{spawn, [{procC, this}],
	 {null}}}},

      {define, procC, origin,
       {spawn, [{procA, this}, {procB, this}],
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
    [prog, [{a, 25}, {b, 25}],

     [{define, procA, origin,
       {send, a, a,
	{recv, b, ack, {null}}}},

      {define, procB, origin,
       {recv, a, x,
	{send, b, "ack", {null}}}}],

     [{procA, 25}, {procB, 25}]].

prog5() ->
    [prog, [{a, 1}],

     [{define, procA, origin,
       {choice, [
		 {send, a, "ack", {null}},
		 {recv, a, "ack", {null}}
	       ]}}],

     [{procA, 100}]].

prog6() ->
    [prog, [{a, 25}, {b, 25}],

     [{define, procA, {-25.0, 25.0, 1.0},
       {choice, [
		{send, a, "ack", {null}},
		{send, b, "ack", {null}}
		]}},

      {define, procB, {25.0, -25.0, 1.0},
       {recv, b, ack, {null}}},

      {define, procC, {25.0, 25.0, 1.0},
       {recv, a, ack, {null}}},

      {define, procD, origin,
       {choice, [
		{send, a, "ack", {null}},
		{send, b, "ack", {null}}
		]}}],

     [{procA, 100}, {procB, 100}, {procC, 100}, {procD, 100}]].

prog7() ->
    [prog, [{a, 1}],

     [{define, procA, origin,
       {spawn, [{procB, this}], {null}}},

      {define, procB, origin,
       {send, a, "ack",
	{recv, a, ack,
	 {null}}}}],

    [{procA, 1}]].

prog8() ->
    [prog, [{a, 25}],

     [{define, procA, origin,
       {send, a, a,
	{move,
	 {spawn, [{procB, {10.0, 10.0, 1.0}}],
	  {null}}}}},

      {define, procB, origin,
       {choice, [
		 {send, a, a, {spawn, [{procB, this}], {null}}},
		 {recv, a, x, {send, x, a,
			       {spawn, [{procA, {10.0, 10.0, 2.0}}], {null}}}}
		]}}],

    [{procA, 20}, {procB, 20}]].

prog9() ->
    [prog, [{a, 250}, {b, 250}],

     [{define, procA, origin,
       {send, a, "ack",
	{recv, b, ack,
	 {spawn, [{procB, this}],
	  {null}}}}},

     {define, procB, origin,
      {recv, a, ack,
       {send, b, "ack",
	{spawn, [{procA, this}],
	 {null}}}}}],

     [{procA, 2}, {procB, 2}]].

prog10() ->
    [prog, [{a, 25}],

     [{define, procA, {0.0, 0.0, 1.0},
       {send, a, this,
	{spawn, [{procB, this}],
	 {null}}}},

      {define, procB, {0.0, 0.0, 2.0},
       {recv, a, x,
	{spawn, [{procB, x}],
	 {null}}}}],

    [{procA, 25}, {procB, 1}]].
