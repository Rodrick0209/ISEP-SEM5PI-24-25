% edge(Node1,Node2)
edge(a,b).
edge(a,e).
edge(b,c).
edge(b,d).
edge(b,e).
edge(d,e).
edge(e,f).

% ligacoes bidireccionais
connect(X,Y):- edge(X,Y);edge(Y,X).

bfs(Orig,Dest,Cam):-
    bfs2(Dest,[[Orig]],Cam).

bfs2(Dest,[[Dest|T]|_],Cam):-
    reverse([Dest|T],Cam).
    
bfs2(Dest,[LA|Outros],Cam):-
    write([LA|Outros]),nl,
    LA=[Act|_],
    findall([X|LA], (Dest\==Act,connect(Act,X),\+member(X,LA)), Novos),
    append(Outros,Novos,Todos),
    bfs2(Dest,Todos,Cam).

