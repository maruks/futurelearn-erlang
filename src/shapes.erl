-module(shapes).
-import(math,[pi/0,sqrt/1,pow/2]).

-export([perimeter/1,area/1,enclose/1]).

-include("shapes.hrl").

distance(X1,Y1,X2,Y2) ->
    sqrt(pow(X1 - X2, 2) + pow(Y1 - Y2, 2)).

area(#circle{r=R}) ->
    pi() * R * R;
area(#rectangle{w=W, h=H}) ->
    W * H;
area(#triangle{x1=X1, y1=Y1, x2=X2, y2=Y2, x3=X3, y3=Y3}) ->
    A = distance(X1, Y1, X2, Y2),
    B = distance(X2, Y2, X3, Y3),
    C = distance(X1, Y1, X3, Y3),
    S = (A + B + C) / 2,
    sqrt(S * (S - A) * (S - B) * (S - C)).

perimeter(#circle{r=R}) ->
    pi() * R * 2;
perimeter(#rectangle{w=W, h=H}) ->
    2 * (W + H);
perimeter(#triangle{x1=X1, y1=Y1, x2=X2, y2=Y2, x3=X3, y3=Y3}) ->
    A = distance(X1, Y1, X2, Y2),
    B = distance(X2, Y2, X3, Y3),
    C = distance(X1, Y1, X3, Y3),
    A + B + C.

enclose(#circle{r=R, x=X, y=Y}) ->
    #rectangle{x = X - R, y = Y - R, w = R*2, h = R*2};
enclose(#rectangle{} = R) ->
    R;
enclose(#triangle{x1=X1, y1=Y1, x2=X2, y2=Y2, x3=X3, y3=Y3}) ->
    MinX = lists:min([X1, X2, X3]),
    MinY = lists:min([Y1, Y2, Y3]),
    MaxX = lists:max([X1, X2, X3]),
    MaxY = lists:max([Y1, Y2, Y3]),
    #rectangle{x = MinX, y = MinY, w = MaxX - MinX, h = MaxY - MinY}.
