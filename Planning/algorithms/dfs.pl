%edge(Node1,Node2)
edge(a,b).
edge(a,e).
edge(b,c).
edge(b,d).
edge(b,e).
edge(d,e).
edge(e,f).

% ligacoes bidireccionais
connect(X,Y):-
edge(X,Y);edge(Y,X).

dfs(Orig,Dest,Cam):-
    dfs(Orig,Dest,[Orig],Cam).

dfs(Dest,Dest,LA,Cam):-
    reverse(LA,Cam).

dfs(Act,Dest,LA,Cam):-
    write(LA),nl,
    connect(Act,X),
    \+ member(X,LA),
    dfs(X,Dest,[X|LA],Cam).

