direct_connection(wroclaw, warszawa).
direct_connection(wroclaw, krakow).
direct_connection(wroclaw, szczecin).
direct_connection(szczecin, lublin).
direct_connection(szczecin, gniezno).
direct_connection(warszawa, katowice).
direct_connection(gniezno, gliwice).
direct_connection(lublin, gliwice).


% ?- direct_connection(Y, Z), direct_connection(Z, gliwice).
% direct_connection(Y, Z), (Z = gliwice; direct_connection(Z, gliwice); direct_connection(Z, V), direct_connection(V, gliwice)).
%podroz(X,gliwice);podroz(X,Y),podroz(Y,gliwice);podroz(X,Y),podroz(Y,Z),podroz(Z,gliwice).

connection(X, Y) :-
    direct_connection(X, Y).
connection(X, Y) :-
    direct_connection(Z, Y),
    connection(X, Z).

% dla cyklu się zapętla
% direct_connection(1, 2).
% direct_connection(2, 3).
% direct_connection(3, 1).
