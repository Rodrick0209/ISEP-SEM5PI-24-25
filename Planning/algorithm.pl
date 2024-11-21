
:- dynamic availability/3.
:- dynamic agenda_staff/3.
:- dynamic agenda_staff1/3.
:-dynamic agenda_operation_room/3.
:-dynamic agenda_operation_room1/3.
:-dynamic better_sol/5.


agenda_staff(d001,20241028,[]).
agenda_staff(d002,20241028,[]).
agenda_staff(d003,20241028,[]).
agenda_staff(d004,20241028,[]).
agenda_staff(d005,20241028,[]).
agenda_staff(e001,20241028,[]).
agenda_staff(e002,20241028,[]).
agenda_staff(e003,20241028,[]).

timetable(d001,20241028,(400,1400)).   % Menor
timetable(d002,20241028,(400,1200)).  % Moderada
timetable(d003,20241028,(300,500)).  % Disponível tarde
timetable(d004,20241028,(300,540)).  % Longa
timetable(d005,20241028,(500,1000)).  % Tarde
timetable(e001,20241028,(450,500)).  % Ajustada
timetable(e002,20241028,(450,500)).   % Curta
timetable(e003,20241028,(460,1220)).

    
%surgery(SurgeryType,TAnesthesia,TSurgery,TCleaning).



surgery(so2,5,15,10).   % Curta
surgery(so3,45,90,45).  % Longa
surgery(so4,30,50,20).  % Média
surgery(so5,20,60,30).  % Nova cirurgia com requisitos apertados

surgery_id(so100001,so3).
surgery_id(so100002,so4).
%surgery_id(so100003,so5).
%surgery_id(so100004,so2).

%assignment_surgery(OpCode,Staff,Phase).
%Exemplos->
assignment_surgery(so100001,d002,surgeryPhase).
assignment_surgery(so100001,d001,anesthesyPhase).
assignment_surgery(so100001,e003,cleaningPhase).

assignment_surgery(so100002,d003,surgeryPhase).
assignment_surgery(so100002,d004,anesthesyPhase).
assignment_surgery(so100002,e001,cleaningPhase).
assignment_surgery(so100002,e002,cleaningPhase).

assignment_surgery(so100003,d001,surgeryPhase).
assignment_surgery(so100003,d005,anesthesyPhase).

assignment_surgery(so100004,d004,surgeryPhase).
assignment_surgery(so100004,e003,anesthesyPhase).


agenda_operation_room(or1,20241028,[]).


free_agenda0([],[(0,1440)]).
free_agenda0([(0,Tfin,_)|LT],LT1):-!,free_agenda1([(0,Tfin,_)|LT],LT1).
free_agenda0([(Tin,Tfin,_)|LT],[(0,T1)|LT1]):- T1 is Tin-1,
    free_agenda1([(Tin,Tfin,_)|LT],LT1).

free_agenda1([(_,Tfin,_)],[(T1,1440)]):-Tfin\==1440,!,T1 is Tfin+1.
free_agenda1([(_,_,_)],[]).
free_agenda1([(_,T,_),(T1,Tfin2,_)|LT],LT1):-Tx is T+1,T1==Tx,!,
    free_agenda1([(T1,Tfin2,_)|LT],LT1).
free_agenda1([(_,Tfin1,_),(Tin2,Tfin2,_)|LT],[(T1,T2)|LT1]):-T1 is Tfin1+1,T2 is Tin2-1,
    free_agenda1([(Tin2,Tfin2,_)|LT],LT1).


adapt_timetable(D,Date,LFA,LFA2):-timetable(D,Date,(InTime,FinTime)),treatin(InTime,LFA,LFA1),treatfin(FinTime,LFA1,LFA2).

treatin(InTime,[(In,Fin)|LFA],[(In,Fin)|LFA]):-InTime=<In,!.
treatin(InTime,[(_,Fin)|LFA],LFA1):-InTime>Fin,!,treatin(InTime,LFA,LFA1).
treatin(InTime,[(_,Fin)|LFA],[(InTime,Fin)|LFA]).
treatin(_,[],[]).

treatfin(FinTime,[(In,Fin)|LFA],[(In,Fin)|LFA1]):-FinTime>=Fin,!,treatfin(FinTime,LFA,LFA1).
treatfin(FinTime,[(In,_)|_],[]):-FinTime=<In,!.
treatfin(FinTime,[(In,_)|_],[(In,FinTime)]).
treatfin(_,[],[]).


intersect_all_agendas([Name],Date,LA):-!,availability(Name,Date,LA).
intersect_all_agendas([Name|LNames],Date,LI):-
    availability(Name,Date,LA),
    intersect_all_agendas(LNames,Date,LI1),
    intersect_2_agendas(LA,LI1,LI).

intersect_2_agendas([],_,[]).
intersect_2_agendas([D|LD],LA,LIT):-	intersect_availability(D,LA,LI,LA1),
					intersect_2_agendas(LD,LA1,LID),
					append(LI,LID,LIT).

intersect_availability((_,_),[],[],[]).

intersect_availability((_,Fim),[(Ini1,Fim1)|LD],[],[(Ini1,Fim1)|LD]):-
		Fim<Ini1,!.

intersect_availability((Ini,Fim),[(_,Fim1)|LD],LI,LA):-
		Ini>Fim1,!,
		intersect_availability((Ini,Fim),LD,LI,LA).

intersect_availability((Ini,Fim),[(Ini1,Fim1)|LD],[(Imax,Fmin)],[(Fim,Fim1)|LD]):-
		Fim1>Fim,!,
		min_max(Ini,Ini1,_,Imax),
		min_max(Fim,Fim1,Fmin,_).

intersect_availability((Ini,Fim),[(Ini1,Fim1)|LD],[(Imax,Fmin)|LI],LA):-
		Fim>=Fim1,!,
		min_max(Ini,Ini1,_,Imax),
		min_max(Fim,Fim1,Fmin,_),
		intersect_availability((Fim1,Fim),LD,LI,LA).


min_max(I,I1,I,I1):- I<I1,!.
min_max(I,I1,I1,I).




schedule_all_surgeries(Room,Day):-
    retractall(agenda_staff1(_,_,_)),
    retractall(agenda_operation_room1(_,_,_)),
    retractall(availability(_,_,_)),
    findall(_,(agenda_staff(D,Day,Agenda),assertz(agenda_staff1(D,Day,Agenda))),_),
    agenda_operation_room(Or,Date,Agenda),assert(agenda_operation_room1(Or,Date,Agenda)),
    findall(_,(agenda_staff1(D,Date,L),free_agenda0(L,LFA),adapt_timetable(D,Date,LFA,LFA2),assertz(availability(D,Date,LFA2))),_),
    findall(OpCode,surgery_id(OpCode,_),LOpCode),
    availability_all_surgeries(LOpCode,Room,Day),!.


availability_all_surgeries([],_,_).
availability_all_surgeries([OpCode|LOpCode],Room,Day):-
    surgery_id(OpCode,OpType),surgery(OpType,TAnesthesy,TSurgery,TCleaning),
    availability_operation(OpCode,Room,Day,Interval,LDoctorsSurgery,LStaffAnesthesy,LStaffCleaning),
    write('Cirurgia a ser agendada: '), write(OpCode), write(' - Intervalo: '), write(Interval), nl,
    calculate_intervals(Interval,TAnesthesy,TSurgery,TCleaning,MinuteStartAnesthesia,MinuteStartSurgery,MinuteStartCleaning,MinuteEndProcess),
    retract(agenda_operation_room1(Room,Day,Agenda)),
    insert_agenda((MinuteStartAnesthesia,MinuteEndProcess,OpCode),Agenda,Agenda1),
    assertz(agenda_operation_room1(Room,Day,Agenda1)),
    insert_agenda_staff((MinuteStartSurgery,MinuteStartCleaning,OpCode),Day,LDoctorsSurgery),
    insert_agenda_staff((MinuteStartAnesthesia,MinuteStartCleaning,OpCode),Day,LStaffAnesthesy),
    insert_agenda_staff((MinuteStartCleaning,MinuteEndProcess,OpCode),Day,LStaffCleaning),
    retractall(availability(_,_,_)),
    findall(_,(agenda_staff1(D,Day,L),free_agenda0(L,LFA),adapt_timetable(D,Day,LFA,LFA2),assertz(availability(D,Day,LFA2))),_),
    availability_all_surgeries(LOpCode,Room,Day).

        

availability_operation(OpCode, Room, Day, Interval, LStaffSurgeryPhase,LStaffAnesthesyPhase,LStaffCleaningPhase) :-
    surgery_id(OpCode, OpType),
    surgery(OpType, TAnesthesia, TSurgery, TCleaning),
    findall(Staff, assignment_surgery(OpCode, Staff, surgeryPhase), LStaffSurgeryPhase),
    findall(Staff, assignment_surgery(OpCode, Staff, anesthesyPhase), LStaffAnesthesyPhase),
    findall(Staff, assignment_surgery(OpCode, Staff, cleaningPhase), LStaffCleaningPhase),

    intersect_all_agendas(LStaffSurgeryPhase, Day, LASurgery),
    intersect_all_agendas(LStaffAnesthesyPhase, Day, LAAnesthesy),
    intersect_all_agendas(LStaffCleaningPhase, Day, LACleaning),

    agenda_operation_room1(Room,Day,LAgenda),
    free_agenda0(LAgenda,LFAgRoom),
    find_valid_interval(LAAnesthesy, LASurgery, LACleaning,LFAgRoom, TAnesthesia, TSurgery, TCleaning, Interval),!,
    write('Cirurgia'),write(OpCode),write(' tem Intervalo disponivel : '), write(Interval), nl.




find_valid_interval(StaffAnesthesiaAvailable, StaffSurgeryAvailable, StaffCleaningAvailable,RoomAvailable, TAnesthesia, TSurgery, TCleaning, Interval) :-
    % Para cada intervalo de disponibilidade da sala
    member((RoomStart, RoomEnd), RoomAvailable),  
    TotalTime is TAnesthesia+TSurgery+TCleaning,  % Soma do tempo de anestesia, cirurgia e limpeza
    MaxStart is RoomEnd - TotalTime,  % Calcula o último minuto em que o processo pode começar
    between(RoomStart, MaxStart, StartAnesthesia), % Para cada minuto entre o início e o último minuto de cada bloco de tempo disponível da sala
    % Assegurar que o intervalo total (anestesia + cirurgia + limpeza) caiba dentro do dia
    StartAnesthesia + TAnesthesia + TSurgery =< 1440,
    StartSurgery is StartAnesthesia + TAnesthesia,
    StartCleaning is StartSurgery + TSurgery,
    % Verific a disponibilidade da sala de operação
    RoomStart =< StartAnesthesia,
    RoomEnd >= (StartAnesthesia + TAnesthesia + TSurgery + TCleaning),
    % Verifica a disponibilidade do staff de anestesia
    member((AnesthesiaStart, AnesthesiaEnd), StaffAnesthesiaAvailable),
    AnesthesiaStart =< StartAnesthesia,  
    AnesthesiaEnd >= (StartAnesthesia + TAnesthesia + TSurgery), 
    % Verifica a disponibilidade do staff de cirurgia
    member((SurgeryStart, SurgeryEnd), StaffSurgeryAvailable),
    SurgeryStart =< StartSurgery,
    SurgeryEnd >= (StartSurgery + TSurgery),
    % Verifica a disponibilidade do staff de limpeza
    member((CleaningStart, CleaningEnd), StaffCleaningAvailable),
    CleaningStart =< StartCleaning,
    CleaningEnd >= (StartSurgery + TSurgery + TCleaning),

    % Calcular o fim do processo
    EndProcess is StartAnesthesia + TAnesthesia + TSurgery + TCleaning,
    % Retornar o intervalo
    Interval = (StartAnesthesia, EndProcess),!.
% Caso não encontre nenhum intervalo válido, retorna false
find_valid_interval(_, _, _, _, _, _, false) :-
    write('Nenhum intervalo disponível!'), nl.







remove_unf_intervals(_,[],[]).
remove_unf_intervals(TSurgery,[(Tin,Tfin)|LA],[(Tin,Tfin)|LA1]):-DT is Tfin-Tin+1,TSurgery=<DT,!,
    remove_unf_intervals(TSurgery,LA,LA1).
remove_unf_intervals(TSurgery,[_|LA],LA1):- remove_unf_intervals(TSurgery,LA,LA1).


schedule_first_interval(TSurgery,[(Tin,_)|_],(Tin,TfinS)):-
    TfinS is Tin + TSurgery - 1.

insert_agenda((TinS,TfinS,OpCode),[],[(TinS,TfinS,OpCode)]).
insert_agenda((TinS,TfinS,OpCode),[(Tin,Tfin,OpCode1)|LA],[(TinS,TfinS,OpCode),(Tin,Tfin,OpCode1)|LA]):-TfinS<Tin,!.
insert_agenda((TinS,TfinS,OpCode),[(Tin,Tfin,OpCode1)|LA],[(Tin,Tfin,OpCode1)|LA1]):-insert_agenda((TinS,TfinS,OpCode),LA,LA1).

insert_agenda_staff(_,_,[]).
insert_agenda_staff((TinS,TfinS,OpCode),Day,[Doctor|LDoctors]):-
    retract(agenda_staff1(Doctor,Day,Agenda)),
    insert_agenda((TinS,TfinS,OpCode),Agenda,Agenda1),
    assert(agenda_staff1(Doctor,Day,Agenda1)),
    insert_agenda_staff((TinS,TfinS,OpCode),Day,LDoctors).



obtain_better_sol(Room,Day,AgOpRoomBetter,LAgDoctorsBetter,TFinOp):-
		get_time(Ti),
		(obtain_better_sol1(Room,Day);true),
		retract(better_sol(Day,Room,AgOpRoomBetter,LAgDoctorsBetter,TFinOp)),
            write('Final Result: AgOpRoomBetter='),write(AgOpRoomBetter),nl,
            write('LAgDoctorsBetter='),write(LAgDoctorsBetter),nl,
            write('Best TimeFinOp='),write(TFinOp),nl,
		get_time(Tf),
		T is Tf-Ti,
		write('Tempo de geracao da solucao:'),write(T),nl.


obtain_better_sol1(Room,Day):-
    asserta(better_sol(Day,Room,_,_,1441)),
    findall(OpCode,surgery_id(OpCode,_),LOC),!,
    permutation(LOC,LOpCode),
    write('A analisar as cirurgias: '), write(LOpCode), nl,
    retractall(agenda_staff1(_,_,_)),
    retractall(agenda_operation_room1(_,_,_)),
    retractall(availability(_,_,_)),
    findall(_,(agenda_staff(D,Day,Agenda),assertz(agenda_staff1(D,Day,Agenda))),_),
    agenda_operation_room(Room,Day,Agenda),assert(agenda_operation_room1(Room,Day,Agenda)),
    findall(_,(agenda_staff1(D,Day,L),free_agenda0(L,LFA),adapt_timetable(D,Day,LFA,LFA2),assertz(availability(D,Day,LFA2))),_),
    write('Antes de availability_all_surgeries'), nl,
    availability_all_surgeries(LOpCode,Room,Day),
    write('Depois de availability_all_surgeries'), nl,
    agenda_operation_room1(Room,Day,AgendaR),
		update_better_sol(Day,Room,AgendaR,LOpCode),
		fail.

update_better_sol(Day,Room,Agenda,LOpCode):-
                better_sol(Day,Room,_,_,FinTime),
                reverse(Agenda,AgendaR),
                write('update_better_sol foi chamado'), nl,
                evaluate_final_time(AgendaR,LOpCode,FinTime1),
             write('Analysing for LOpCode='),write(LOpCode),nl,
             write('now: FinTime1='),write(FinTime1),write(' Agenda='),write(Agenda),nl,
		FinTime1<FinTime,
             write('best solution updated'),nl,
                retract(better_sol(_,_,_,_,_)),
                findall(Doctor,assignment_surgery(_,Doctor,_),LDoctors1),
                remove_equals(LDoctors1,LDoctors),
                list_doctors_agenda(Day,LDoctors,LDAgendas),
                write('Chegou aqui'), nl,
		asserta(better_sol(Day,Room,Agenda,LDAgendas,FinTime1)).

evaluate_final_time([],_,1441).
evaluate_final_time([(_,Tfin,OpCode)|_],LOpCode,Tfin):-member(OpCode,LOpCode),!.
evaluate_final_time([_|AgR],LOpCode,Tfin):-evaluate_final_time(AgR,LOpCode,Tfin).

list_doctors_agenda(_,[],[]).
list_doctors_agenda(Day,[D|LD],[(D,AgD)|LAgD]):-agenda_staff1(D,Day,AgD),list_doctors_agenda(Day,LD,LAgD).

remove_equals([],[]).
remove_equals([X|L],L1):-member(X,L),!,remove_equals(L,L1).
remove_equals([X|L],[X|L1]):-remove_equals(L,L1).

% O predicado calcula os intervalos de tempo para anestesia, cirurgia e limpeza
calculate_intervals((Start, End), TAnesthesia, TSurgery, TCleaning, MinuteStartAnesthesia, MinuteStartSurgery, MinuteStartCleaning, MinuteEndProcess) :-
    % O início da anestesia é o início do intervalo (Start)
    MinuteStartAnesthesia = Start,
    % O início da cirurgia é o final da anestesia, ou seja, após a duração de anestesia
    MinuteStartSurgery is MinuteStartAnesthesia + TAnesthesia,
    % O início da limpeza é o final da cirurgia, ou seja, após a duração da cirurgia
    MinuteStartCleaning is MinuteStartSurgery + TSurgery,
    % O fim do processo é o final da limpeza, ou seja, após a duração da limpeza
    MinuteEndProcess is MinuteStartCleaning + TCleaning.



% Predicado principal para agendar todas as cirurgias
schedule_all_surgeriesHeuristic(Room, Day) :-
    get_time(Ti),
    retractall(agenda_staff1(_, _, _)),
    retractall(agenda_operation_room1(_, _, _)),
    retractall(availability(_, _, _)),
    
    % Copiar as agendas de staff e sala para as novas tabelas
    findall(_, (agenda_staff(D, Day, Agenda), assertz(agenda_staff1(D, Day, Agenda))), _),
    agenda_operation_room(Or, Date, Agenda),
    assertz(agenda_operation_room1(Or, Date, Agenda)),

    % Atualizar a disponibilidade de staff
    findall(_, 
        (
            agenda_staff1(D, Date, L),
            free_agenda0(L, LFA),
            adapt_timetable(D, Date, LFA, LFA2),
            assertz(availability(D, Date, LFA2))
        ), _),

    % Obter lista de operações (cirurgias)
    findall(OpCode, surgery_id(OpCode, _), LOpCode),

    % Chamada recursiva para agendar todas as cirurgias
    availability_all_surgeries2(LOpCode, Room, Day, 0),  % Passando 0 como tempo final inicial
    !,
        % Registrar o tempo de fim
    get_time(EndTime),
    
    % Calcular e mostrar o tempo total de execução
    TotalTime is EndTime - Ti,
    write('Tempo total de execução: '), write(TotalTime), nl.



% Caso base: Nenhuma cirurgia a ser agendada
availability_all_surgeries2([], _, _, MaxEndTime) :-
    write('Tempo final de todas as cirurgias: '), write(MaxEndTime), nl.

% Caso recursivo: Selecionar e agendar cirurgias
availability_all_surgeries2(LOpCode, Room, Day, CurrentMaxEndTime) :-
    % Selecionar a próxima cirurgia com base no critério
    (   select_next_surgeryCriteria2(LOpCode, OpCode, Interval, LDoctorsSurgery, LStaffAnesthesy,LStaffCleaning)
    ->  % Se uma cirurgia for encontrada, continuar o agendamento
        surgery_id(OpCode, OpType),
        surgery(OpType, TAnesthesy, TSurgery, TCleaning),
        calculate_intervals(Interval, TAnesthesy, TSurgery, TCleaning, 
                            MinuteStartAnesthesia, MinuteStartSurgery, 
                            MinuteStartCleaning, MinuteEndProcess),
        retract(agenda_operation_room1(Room, Day, Agenda)),
        insert_agenda((MinuteStartAnesthesia, MinuteEndProcess, OpCode), Agenda, Agenda1),
        assertz(agenda_operation_room1(Room, Day, Agenda1)),
        insert_agenda_staff((MinuteStartSurgery, MinuteStartCleaning, OpCode), Day, LDoctorsSurgery),
        insert_agenda_staff((MinuteStartAnesthesia, MinuteStartCleaning, OpCode), Day, LStaffAnesthesy),
        insert_agenda_staff((MinuteStartCleaning, MinuteEndProcess, OpCode), Day, LStaffCleaning),
        retractall(availability(_, _, _)),
        findall(_, 
            (
                agenda_staff1(D, Day, L),
                free_agenda0(L, LFA),
                adapt_timetable(D, Day, LFA, LFA2),
                assertz(availability(D, Day, LFA2))
            ), _),
        MaxEndTime is max(CurrentMaxEndTime, MinuteEndProcess),
        delete(LOpCode, OpCode, RemainingLOpCode),
        availability_all_surgeries2(RemainingLOpCode, Room, Day, MaxEndTime)
    ;   % Se nenhuma cirurgia puder ser selecionada, falhar explicitamente
        write('Nenhuma cirurgia pode ser agendada com os critérios atuais.'), nl,
        fail
    ).
    
% Predicado que escolhe a próxima cirurgia com base no primeiro tempo disponível
select_next_surgeryCriteria1(LOpCode, BestOpCode, Interval, LDoctorsSurgery, LStaffAnesthesy,LStaffCleaning) :-
    write('Lista de operações a ser analisada: '), write(LOpCode), nl,
    findall(
        (OpCode, Interval, LDoctorsSurgery, LStaffAnesthesy,LStaffCleaning),
        (   
            member(OpCode, LOpCode),
            surgery_id(OpCode, OpType),
            %write('A analisar a cirurgia: '), write(OpCode), nl,
            surgery(OpType, _, _, _),
            availability_operation(OpCode, _, _, Interval, LDoctorsSurgery, LStaffAnesthesy,LStaffCleaning)
        ),
        Candidates
    ),
    % Selecionar a cirurgia com o menor intervalo de início
    sort(2, @=<, Candidates, SortedCandidates),
    SortedCandidates = [(BestOpCode, Interval, LDoctorsSurgery, LStaffAnesthesy,LStaffCleaning) | _],
    write('BestOpCode='), write(BestOpCode), nl.


% Predicado que escolhe a próxima cirurgia com base no menor número de staff envolvido
select_next_surgeryCriteria2(LOpCode, BestOpCode, Interval, LDoctorsSurgery, LStaffAnesthesy,LStaffCleaning) :-
    write('Lista de operações a ser analisada: '), write(LOpCode), nl,
    
    findall(
        (OpCode, Interval, LDoctorsSurgery, LStaffAnesthesy,LStaffCleaning),
        (   
            member(OpCode, LOpCode),
            surgery_id(OpCode, OpType),
            surgery(OpType, _, _, _),
            availability_operation(OpCode, _, _, Interval, LDoctorsSurgery, LStaffAnesthesy,LStaffCleaning)
        ),
        Candidates
    ),

    write('Doctor: '), write(LDoctorsSurgery), nl,
    
    

    % Ordenar pela menor quantidade de staff (índice 5)
    sort(2, @=<, Candidates, SortedCandidates),
    write('Candidatos ordenados pelo número de staff: '), write(SortedCandidates), nl,
    SortedCandidates = [(BestOpCode, Interval, LDoctorsSurgery, LStaffAnesthesy, LStaffCleaning) | _],
    write('BestOpCode='), write(BestOpCode), nl.

