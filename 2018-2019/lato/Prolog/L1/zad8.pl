sentences(t, t, t, t, t, t).
sentences(_, t, f, f, f, f).
sentences(t, t, t, _, _, _).
sentences(f, f, f, f, _, _).
sentences(f, f, f, f, t, _).
sentences(f, f, f, f, f, t).

% first(t, t, t, t, t, t).
% first(f, _, _, _, _, _).

% second(_, t, f, f, f, f).
% second(_, f, _, _, _, _).

% third(t, t, t, _, _, _).
% third(_, _, f, _, _, _).

% fourth(f, f, f, f, _, _).
% fourth(_, _, _, t, _, _).

% fifth(f, f, f, f, t, _).
% fifth(_, _, _, _, f, _).

% sixth(f, f, f, f, f, t).
% sixth(_, _, _, _, _, f).