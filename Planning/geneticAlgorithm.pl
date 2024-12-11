:-dynamic generations/1.
:-dynamic population/1.
:-dynamic prob_crossover/1.
:-dynamic prob_mutation/1.
:-dynamic percentage_individuals/1.
:-dynamic lowerCostWanted/1.

% task(Id,ProcessTime,DueTime,PenaltyWeight).
task(t1,2,5,1).
task(t2,4,7,6).
task(t3,1,11,2).
task(t4,3,9,3).
task(t5,3,8,2).

% tasks(NTasks).
tasks(5).


% parameters initialization
initialize:-write('Number of new generations: '),read(NG), 			
    (retract(generations(_));true), asserta(generations(NG)),
	write('Population size: '),read(PS),
	(retract(population(_));true), asserta(population(PS)),
	write('Probability of crossover (%):'), read(P1),
	PC is P1/100, 
	(retract(prob_crossover(_));true), 	asserta(prob_crossover(PC)),
	write('Probability of mutation (%):'), read(P2),
	PM is P2/100, 
	(retract(prob_mutation(_));true), asserta(prob_mutation(PM)),
    write('Percentage of best individuals to be kept for the next generation (%): '), read(P3),
    PI is P3/100,
    (retract(percentage_individuals(_));true), asserta(percentage_individuals(PI)),
    write('Lower cost wanted: '), read(LowerCost),
    (retract(lowerCostWanted(_));true), asserta(lowerCostWanted(LowerCost)),
    write('Time limit (in seconds): '), read(TempoLimite),
    (retract(time_limit(_));true), asserta(time_limit(TempoLimite)).

generate:-
    initialize,
    get_time(StartTime),
    generate_population(Pop),
    write('Pop='),write(Pop),nl,
    evaluate_population(Pop,PopValue),
    write('PopValue='),write(PopValue),nl,
    order_population(PopValue,PopOrd),
    lowerCostWanted(LowerCost),
    ( PopOrd = [_*V|_]) ->
    ( V =< LowerCost ->
        write('Lower cost found!'), nl, !
        ;
        get_time(CurrentTime),
        time_limit(TimeLimit),
        ElapsedTime is CurrentTime - StartTime,
        ( ElapsedTime > TimeLimit ->
            write('Tempo limite atingido após '), write(ElapsedTime), write(' segundos.'), nl, !
        ;
            generations(NG),
            generate_generation(0, NG, PopOrd, StartTime)
        )
    ).



generate_population(Pop):-
    population(PopSize),
    tasks(NumT),
    findall(Task,task(Task,_,_,_),TasksList),
    generate_population(PopSize,TasksList,NumT,Pop).

generate_population(0,_,_,[]):-!.
generate_population(PopSize,TasksList,NumT,[Ind|Rest]):-
    PopSize1 is PopSize-1,
    generate_population(PopSize1,TasksList,NumT,Rest),
    generate_individual(TasksList,NumT,Ind),
    not(member(Ind,Rest)).
generate_population(PopSize,TasksList,NumT,L):-
    generate_population(PopSize,TasksList,NumT,L).
    


generate_individual([G],1,[G]):-!.

generate_individual(TasksList,NumT,[G|Rest]):-
    NumTemp is NumT + 1, % to use with random
    random(1,NumTemp,N),
    remove(N,TasksList,G,NewList), %Remove o elemento na posicao N da lista TasksList e coloca em G
    NumT1 is NumT-1,
    generate_individual(NewList,NumT1,Rest).

remove(1,[G|Rest],G,Rest).
remove(N,[G1|Rest],G,[G1|Rest1]):- N1 is N-1,
            remove(N1,Rest,G,Rest1).


evaluate_population([],[]).
evaluate_population([Ind|Rest],[Ind*V|Rest1]):-
    evaluate(Ind,V),
    evaluate_population(Rest,Rest1).

evaluate(Seq,V):- evaluate(Seq,0,V).

evaluate([ ],_,0).
evaluate([T|Rest],Inst,V):-
    task(T,Dur,Due,Pen),
    FinInst is Inst+Dur,
    evaluate(Rest,FinInst,VRest),
    ((FinInst =< Due,!, VT is 0) ; (VT is (FinInst-Due)*Pen)),
    V is VT+VRest.

order_population(PopValue,PopValueOrd):-
    bsort(PopValue,PopValueOrd).

bsort([X],[X]):-!.
bsort([X|Xs],Ys):-
    bsort(Xs,Zs),
    bchange([X|Zs],Ys).


bchange([X],[X]):-!.

bchange([X*VX,Y*VY|L1],[Y*VY|L2]):-
    VX>VY,!,
    bchange([X*VX|L1],L2).

bchange([X|L1],[X|L2]):-bchange(L1,L2).
    
generate_generation(G,G,Pop,_):-!,
	write('Generation '), write(G), write(':'), nl, write(Pop), nl.
generate_generation(N,G,Pop,StartTime):-
	write('Generation '), write(N), write(':'), nl, write(Pop), nl,
	get_time(CurrentTime),
    time_limit(TimeLimit),
    ElapsedTime is CurrentTime - StartTime,
    
    ( ElapsedTime > TimeLimit ->
        write('Tempo limite atingido após '), write(ElapsedTime), write(' segundos.'), nl,!
    ;

    random_permutation(Pop, ShuffledPop), % Shuffle the population to avoid the crossover being always between elements in the same position
    crossover(ShuffledPop,NPop1),
	
    mutation(NPop1,NPop),
	
    evaluate_population(NPop,NPopValue),
    
    append(Pop, NPopValue, PopMergedWithDup), % Merge the old population with the new one
    list_to_set(PopMergedWithDup, PopMerged), % Remove duplicates
    order_population(PopMerged,PopMergedOrd), % Order the merged population
    split_best_individuals(PopMergedOrd, BestIndividuals, RemainingIndividuals), % Split the best individuals from the remaining ones
    length(BestIndividuals, NumBest),
    population(PopSize),
    
    NumIndividuosAirBuscarRestoLista is PopSize - NumBest,
    nl,
    write('BestIndividuals='),write(BestIndividuals),
	nl,
    create_and_sort_remaining_individuals(RemainingIndividuals, NumIndividuosAirBuscarRestoLista, NextGenIndividuals), 
    write('Rest of individuals for the population='),write(NextGenIndividuals),nl,nl,
	append(BestIndividuals, NextGenIndividuals, NewPopulationNotOrded),
    order_population(NewPopulationNotOrded, NewPopulation),
    write('NewPopulation='),write(NewPopulation),nl,nl,
    N1 is N+1,
	generate_generation(N1,G,NewPopulation,StartTime)
    ).

generate_crossover_points(P1,P2):- generate_crossover_points1(P1,P2).

generate_crossover_points1(P1,P2):-
	tasks(N),
	NTemp is N+1,
	random(1,NTemp,P11),
	random(1,NTemp,P21),
	P11\==P21,!,
	((P11<P21,!,P1=P11,P2=P21);P1=P21,P2=P11).
generate_crossover_points1(P1,P2):-
	generate_crossover_points1(P1,P2).


crossover([ ],[ ]).
crossover([Ind*_],[Ind]).
crossover([Ind1*_,Ind2*_|Rest],[NInd1,NInd2|Rest1]):-
	
    generate_crossover_points(P1,P2),
	prob_crossover(Pcruz),random(0.0,1.0,Pc),
	((Pc =< Pcruz,!,
        cross(Ind1,Ind2,P1,P2,NInd1),
	  cross(Ind2,Ind1,P1,P2,NInd2))
	;
	(NInd1=Ind1,NInd2=Ind2)),
	crossover(Rest,Rest1).

fillh([ ],[ ]).

fillh([_|R1],[h|R2]):-
	fillh(R1,R2).

sublist(L1,I1,I2,L):-I1 < I2,!,
    sublist1(L1,I1,I2,L).

sublist(L1,I1,I2,L):-sublist1(L1,I2,I1,L).

sublist1([X|R1],1,1,[X|H]):-!, fillh(R1,H).

sublist1([X|R1],1,N2,[X|R2]):-!,N3 is N2 - 1,
	sublist1(R1,1,N3,R2).

sublist1([_|R1],N1,N2,[h|R2]):-N3 is N1 - 1,
		N4 is N2 - 1,
		sublist1(R1,N3,N4,R2).

rotate_right(L,K,L1):- tasks(N),
	T is N - K,
	rr(T,L,L1).

rr(0,L,L):-!.

rr(N,[X|R],R2):- N1 is N - 1,
	append(R,[X],R1),
	rr(N1,R1,R2).

remove([],_,[]):-!.

remove([X|R1],L,[X|R2]):- not(member(X,L)),!,
        remove(R1,L,R2).

remove([_|R1],L,R2):-
    remove(R1,L,R2).

insert([],L,_,L):-!.
insert([X|R],L,N,L2):-
    tasks(T),
    ((N>T,!,N1 is N mod T);N1 = N),
    insert1(X,N1,L,L1),
    N2 is N + 1,
    insert(R,L1,N2,L2).


insert1(X,1,L,[X|L]):-!.
insert1(X,N,[Y|L],[Y|L1]):-
    N1 is N-1,
    insert1(X,N1,L,L1).

cross(Ind1,Ind2,P1,P2,NInd11):-
    sublist(Ind1,P1,P2,Sub1),
    tasks(NumT),
    R is NumT-P2,
    rotate_right(Ind2,R,Ind21),
    remove(Ind21,Sub1,Sub2),
    P3 is P2 + 1,
    insert(Sub2,Sub1,P3,NInd1),
    removeh(NInd1,NInd11).


removeh([],[]).

removeh([h|R1],R2):-!,
    removeh(R1,R2).

removeh([X|R1],[X|R2]):-
    removeh(R1,R2).

mutation([],[]).
mutation([Ind|Rest],[NInd|Rest1]):-
	prob_mutation(Pmut),
	random(0.0,1.0,Pm),
	((Pm < Pmut,!,mutacao1(Ind,NInd));NInd = Ind),
	mutation(Rest,Rest1).

mutacao1(Ind,NInd):-
	generate_crossover_points(P1,P2),
	mutacao22(Ind,P1,P2,NInd).

mutacao22([G1|Ind],1,P2,[G2|NInd]):-
	!, P21 is P2-1,
	mutacao23(G1,P21,Ind,G2,NInd).
mutacao22([G|Ind],P1,P2,[G|NInd]):-
	P11 is P1-1, P21 is P2-1,
	mutacao22(Ind,P11,P21,NInd).

mutacao23(G1,1,[G2|Ind],G2,[G1|Ind]):-!.
mutacao23(G1,P,[G|Ind],G2,[G|NInd]):-
	P1 is P-1,
	mutacao23(G1,P1,Ind,G2,NInd).



melhores_n(Lista, N, Melhores) :-
    order_population(Lista, Ordenada),
    first_n(Ordenada, N, Melhores).


% Pega os N primeiros elementos de uma lista
first_n(_, 0, []) :- !. % Quando N é 0, a lista resultante é vazia
first_n([X|Xs], N, [X|Ys]) :-
    N > 0,
    N1 is N - 1,
    first_n(Xs, N1, Ys).


split_best_individuals(Population, BestIndividuals, RemainingIndividuals) :-
    percentage_individuals(Percentage),
    length(Population, TotalSize),
    N is max(1, round(TotalSize * Percentage)), % Calculate N accordingly with the percentage (min 1)
    length(BestIndividuals, N),               
    append(BestIndividuals, RemainingIndividuals, Population). % Divide a população




% Gera o produto (IndList, Eval, Product) para cada indivíduo
generate_random_products([], []).  % Caso base: lista vazia.

generate_random_products([IndList * Eval | Rest], [(IndList * Product) | NewRest]) :-
    random(Rand), 
    Product is Eval * Rand,
    generate_random_products(Rest, NewRest).

% Chama o predicado para gerar os produtos aleatórios
create_and_sort_remaining_individuals(RemainingIndividuals, N, NextGenIndividuals) :-
    nl, nl,
    generate_random_products(RemainingIndividuals, RandomizedProducts),
    order_population(RandomizedProducts, SortedProducts),
    write('Resto da lista SortedProducts='), write(SortedProducts), nl,nl,
    write('Lista original='), write(RemainingIndividuals), nl,
    extract_top_individuals(SortedProducts, N, RemainingIndividuals, NextGenIndividuals).

extract_top_individuals([], _, _, []).
extract_top_individuals(_, 0, _, []).
extract_top_individuals([Ind * _ | RestSorted], N, RemainingIndividuals, [Ind * Eval | NewRest]) :-
    N1 is N - 1,
    member(Ind * Eval, RemainingIndividuals),
    extract_top_individuals(RestSorted, N1, RemainingIndividuals, NewRest).


parar_execucao :-
    writeln('Time limit exceeded! Finishing the program...'),
    abort.


