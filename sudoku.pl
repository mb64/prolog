% vim:ft=prolog

maplist(F, []).
maplist(F, [X | Xs]) :- call(F, X), maplist(F, Xs).

maplist(F, [], []).
maplist(F, [X | Xs], [Y | Ys]) :- call(F, X, Y), maplist(F, Xs, Ys).

same_length([], []).
same_length([_ | X], [_ | Y]) :- same_length(X, Y).

length([], 0).
length([_ | Tail], N) :- length(Tail, N1), N is N1 + 1.

member(X, [X | _]).
member(X, [_ | Tail]) :- member(X, Tail).

head([X | _], X).
tail([_ | X], X).

all_distinct([X | Xs]) :- \+ member(X, Xs), all_distinct(Xs).
all_distinct([]).

transpose([], []).
transpose([[] | Xss], Yss) :- transpose(Xss, Yss).
transpose(Xss, [Ys | Yss]) :- Xss = [_|_],
    maplist(head, Xss, Ys), maplist(tail, Xss, Tails),
    transpose(Tails, Yss).

upto(X, X, [X]).
upto(X, Y, [X | Tail]) :- X \= Y, X1 is X + 1, upto(X1, Y, Tail).

time(Goal) :-
    cpu_time(Start),
    Goal,
    cpu_time(End),
    Duration is End - Start,
    nl, write('Took '), write(Duration), write('ms'), nl.

%%%%%

in_range(1).
in_range(2).
in_range(3).
in_range(4).

good_row(Row) :- maplist(in_range, Row), all_distinct(Row).

mini_sudoku(Rows) :-
    length(Rows,4), maplist(same_length(Rows), Rows),
    transpose(Rows, Cols),
    Rows = [As,Bs,Cs,Ds],
    maplist(good_row, Rows),
    maplist(all_distinct, Cols),
    blocks(As, Bs),
    blocks(Cs, Ds).

blocks([], []).
blocks([N1,N2|Ns1], [N3,N4|Ns2]) :-
    all_distinct([N1,N2,N3,N4]),
    blocks(Ns1, Ns2).

problem(1, [[_,2,_,_],
            [_,_,1,_],
            [_,1,_,_],
            [_,_,4,_]]).

problem(2, [[1,_,2,_],
            [_,2,_,_],
            [_,_,3,_],
            [_,_,_,_]]).
