plus(s(N), M, s(R)) :- plus(N, M, R).
plus(z, N, N).
times(s(N), M, A) :- times(N, M, R), plus(R, M, A).
times(z, _, z).
fact(s(N), R) :- fact(N, PR), times(s(N), PR, R).
fact(z, s(z)).
