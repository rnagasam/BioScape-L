-module(geom).
-compile([export_all]).

-record(pos, { x = 0.0, y = 0.0 }).
-record(geom, { pos = #pos{}, radius = 1.0 }).

default() ->
    #geom{}.

make_pos(N, M) ->
    #pos{ x = N, y = M }.

make_geom(Pos, Rad) ->
    #geom{ pos = Pos, radius = Rad }.

translate(Geom, X, Y) ->
    NewPos = #pos{ x = Geom#geom.pos#pos.x + X,
		   y = Geom#geom.pos#pos.y + Y },
    Geom#geom{ pos = NewPos, radius = Geom#geom.radius }.
    
scale(Geom, R) ->
    Geom#geom{ pos = Geom#geom.pos, radius = Geom#geom.radius * R }.

distance(G1, G2) ->
    Dx = G2#geom.pos#pos.x - G1#geom.pos#pos.x,
    Dy = G2#geom.pos#pos.y - G1#geom.pos#pos.y,
    math:sqrt(math:pow(Dx, 2) + math:pow(Dy, 2)).
