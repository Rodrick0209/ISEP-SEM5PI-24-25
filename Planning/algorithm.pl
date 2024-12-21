
:- dynamic availability/3.
:- dynamic agenda_staff/3.
:- dynamic agenda_staff1/3.
:-dynamic agenda_operation_room/3.
:-dynamic agenda_operation_room1/3.
:-dynamic better_sol/5.
:-dynamic lastSurgeryTime/1.



% Agenda staff
agenda_staff(d001,20251130,[]).
agenda_staff(d002,20251130,[]).
agenda_staff(d003,20251130,[]).
agenda_staff(d004,20251130,[]).
agenda_staff(d005,20251130,[]).
agenda_staff(e001,20251130,[]).
agenda_staff(e002,20251130,[]).
agenda_staff(e003,20251130,[]).

% Timetable
timetable(d001,20251130,(300,1400)).
timetable(d002,20251130,(300,1400)).
timetable(d003,20251130,(300,1400)).
timetable(d004,20251130,(300,1400)).
timetable(d005,20251130,(300,1400)).
timetable(e001,20251130,(300,1400)).
timetable(e002,20251130,(300,1400)).
timetable(e003,20251130,(300,1400)).

% Surgery
surgery(so2,5,15,10).
surgery(so3,10,15,25).
surgery(so4,30,50,20).
surgery(so5,20,60,30).

% Surgery IDs
surgery_id(so100001,so2).
surgery_id(so100002,so3).
surgery_id(so100003,so4).
surgery_id(so100004,so5).
surgery_id(so100005,so2).

surgeries_number(5).


% Assignment surgery
assignment_surgery(so100001,d001,surgeryPhase).
assignment_surgery(so100001,d002,anesthesyPhase).
assignment_surgery(so100001,e001,cleaningPhase).

assignment_surgery(so100002,d003,surgeryPhase).
assignment_surgery(so100002,d004,anesthesyPhase).
assignment_surgery(so100002,e002,cleaningPhase).

assignment_surgery(so100003,d001,surgeryPhase).
assignment_surgery(so100003,d005,anesthesyPhase).
assignment_surgery(so100003,e001,cleaningPhase).

assignment_surgery(so100004,d004,surgeryPhase).
assignment_surgery(so100004,e003,anesthesyPhase).
assignment_surgery(so100004,e002,cleaningPhase).

assignment_surgery(so100005,d002,surgeryPhase).
assignment_surgery(so100005,d003,anesthesyPhase).
assignment_surgery(so100005,e003,cleaningPhase).

assignment_surgery(so100006,d001,surgeryPhase).
assignment_surgery(so100006,d004,anesthesyPhase).
assignment_surgery(so100006,e001,cleaningPhase).

assignment_surgery(so100007,d002,surgeryPhase).
assignment_surgery(so100007,d005,anesthesyPhase).
assignment_surgery(so100007,e003,cleaningPhase).



% Agenda operation room
agenda_operation_room(or1,20251130,[]).
agenda_operation_room(or2,20251130,[]).
agenda_operation_room(or3,20251130,[]).






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


schedule_surgeries_fromListForMultipleRooms(Day,LOpCode):-
    retractall(lastSurgeryTime(_)),
    retractall(agenda_staff1(_,_,_)),
    retractall(agenda_operation_room1(_,_,_)),
    retractall(availability(_,_,_)),
    findall(_,(agenda_staff(D,Day,Agenda),assertz(agenda_staff1(D,Day,Agenda))),_),
    findall((Or), (agenda_operation_room(Or, Date, Agenda), assert(agenda_operation_room1(Or, Date, Agenda))), Lrooms),
    write('Lista de rooms = '), write(Lrooms), nl,
    findall(_,(agenda_staff1(D,Date,L),free_agenda0(L,LFA),adapt_timetable(D,Date,LFA,LFA2),assertz(availability(D,Date,LFA2))),_),
    asserta(lastSurgeryTime(0)),
    availability_all_surgeries4(LOpCode,Lrooms,Day),!.

availability_all_surgeries4([], _, _) :-!.
availability_all_surgeries4([OpCode|LOpCode], [], Day):- 
    % Reinicia a lista de salas caso esteja vazia
    findall(Or, agenda_operation_room(Or, Date, Agenda), Lrooms),
    availability_all_surgeries4([OpCode|LOpCode], Lrooms, Day),!.

availability_all_surgeries4([OpCode|LOpCode], [Room|LRooms], Day):-
    surgery_id(OpCode, OpType),
    write('A atribuir a cirurgia'),write(OpCode),write(' à sala '),write(Room),nl,
    surgery(OpType, TAnesthesy, TSurgery, TCleaning),
    availability_operation(OpCode, Room, Day, Interval, LDoctorsSurgery, LStaffAnesthesy, LStaffCleaning),
    calculate_intervals(Interval, TAnesthesy, TSurgery, TCleaning, MinuteStartAnesthesia, MinuteStartSurgery, MinuteStartCleaning, MinuteEndProcess),
    write('Intervalo calculado: '), write((MinuteStartAnesthesia, MinuteEndProcess)), nl,
    retract(agenda_operation_room1(Room, Day, Agenda)),
    insert_agenda((MinuteStartAnesthesia, MinuteEndProcess, OpCode), Agenda, Agenda1),
    assertz(agenda_operation_room1(Room, Day, Agenda1)),
    write('Agenda atualizada da sala'),write(Room),write(' atualizada para -->') ,write(Agenda1), nl,
    insert_agenda_staff((MinuteStartSurgery, MinuteStartCleaning, OpCode), Day, LDoctorsSurgery),
    insert_agenda_staff((MinuteStartAnesthesia, MinuteStartCleaning, OpCode), Day, LStaffAnesthesy),
    insert_agenda_staff((MinuteStartCleaning, MinuteEndProcess, OpCode), Day, LStaffCleaning),
    lastSurgeryTime(LastSurgeryTime),
    write('Último tempo de cirurgia: '), write(LastSurgeryTime), nl,
    % Atualizando LastSurgeryTime apenas se for maior que o valor atual
    write('Minuto de fim do processo: '), write(MinuteEndProcess), nl,
    

    (LastSurgeryTime < MinuteEndProcess ->
        write('Atualizando lastSurgeryTime'), nl,
        retract(lastSurgeryTime(_)),
        assertz(lastSurgeryTime(MinuteEndProcess)),
        !
    ; 
        true
    ),
    availability_all_surgeries4(LOpCode, LRooms, Day).





schedule_surgeries_fromList(Room,Day,LOpCode):-
    retractall(lastSurgeryTime(_)),
    retractall(agenda_staff1(_,_,_)),
    retractall(agenda_operation_room1(_,_,_)),
    retractall(availability(_,_,_)),
    findall(_,(agenda_staff(D,Day,Agenda),assertz(agenda_staff1(D,Day,Agenda))),_),
    agenda_operation_room(Or,Date,Agenda),assert(agenda_operation_room1(Or,Date,Agenda)),
    findall(_,(agenda_staff1(D,Date,L),free_agenda0(L,LFA),adapt_timetable(D,Date,LFA,LFA2),assertz(availability(D,Date,LFA2))),_),
    asserta(lastSurgeryTime(0)),
    availability_all_surgeries3(LOpCode,Room,Day),!.


availability_all_surgeries3([],_,_).
availability_all_surgeries3([OpCode|LOpCode],Room,Day):-
    surgery_id(OpCode,OpType),surgery(OpType,TAnesthesy,TSurgery,TCleaning),
    availability_operation(OpCode,Room,Day,Interval,LDoctorsSurgery,LStaffAnesthesy,LStaffCleaning),

    calculate_intervals(Interval,TAnesthesy,TSurgery,TCleaning,MinuteStartAnesthesia,MinuteStartSurgery,MinuteStartCleaning,MinuteEndProcess),
    retract(agenda_operation_room1(Room,Day,Agenda)),
    insert_agenda((MinuteStartAnesthesia,MinuteEndProcess,OpCode),Agenda,Agenda1),
    assertz(agenda_operation_room1(Room,Day,Agenda1)),
    insert_agenda_staff((MinuteStartSurgery,MinuteStartCleaning,OpCode),Day,LDoctorsSurgery),
    insert_agenda_staff((MinuteStartAnesthesia,MinuteStartCleaning,OpCode),Day,LStaffAnesthesy),
    insert_agenda_staff((MinuteStartCleaning,MinuteEndProcess,OpCode),Day,LStaffCleaning),
    lastSurgeryTime(LastSurgeryTime),
    % Atualizando LastSurgeryTime apenas se for maior que o valor atual
    (LastSurgeryTime < MinuteEndProcess ->
        retract(lastSurgeryTime(_)),
        assert(lastSurgeryTime(MinuteEndProcess))
    ),
    % Agora LastSurgeryTime1 possui o valor atualizado
    availability_all_surgeries3(LOpCode, Room, Day).











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
    find_valid_interval(LAAnesthesy, LASurgery, LACleaning,LFAgRoom, TAnesthesia, TSurgery, TCleaning, Interval),!.




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
find_valid_interval(_, _, _, _, _, _, false) :-!.






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
		get_time(Tf),
		T is Tf-Ti.

obtain_better_sol1(Room,Day):-
    asserta(better_sol(Day,Room,_,_,1441)),
    findall(OpCode,surgery_id(OpCode,_),LOC),!,
    permutation(LOC,LOpCode),
    retractall(agenda_staff1(_,_,_)),
    retractall(agenda_operation_room1(_,_,_)),
    retractall(availability(_,_,_)),
    findall(_,(agenda_staff(D,Day,Agenda),assertz(agenda_staff1(D,Day,Agenda))),_),
    agenda_operation_room(Room,Day,Agenda),assert(agenda_operation_room1(Room,Day,Agenda)),
    findall(_,(agenda_staff1(D,Day,L),free_agenda0(L,LFA),adapt_timetable(D,Day,LFA,LFA2),assertz(availability(D,Day,LFA2))),_),
    availability_all_surgeries(LOpCode,Room,Day),
    agenda_operation_room1(Room,Day,AgendaR),
		update_better_sol(Day,Room,AgendaR,LOpCode),
        fail.

        

update_better_sol(Day,Room,Agenda,LOpCode):-
                better_sol(Day,Room,_,_,FinTime),
                reverse(Agenda,AgendaR),
                evaluate_final_time(AgendaR,LOpCode,FinTime1),
		FinTime1<FinTime,
                retract(better_sol(_,_,_,_,_)),
                findall(Doctor,assignment_surgery(_,Doctor,_),LDoctors1),
                remove_equals(LDoctors1,LDoctors),
                list_doctors_agenda(Day,LDoctors,LDAgendas),
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
    MinuteEndProcess is End.



% Predicado principal para agendar todas as cirurgias
schedule_all_surgeriesHeuristic(Room, Day) :-
    get_time(Ti),
    retractall(agenda_staff1(_, _, _)),
    retractall(agenda_operation_room1(_, _, _)),
    retractall(availability(_, _, _)),
    
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

    findall(OpCode, surgery_id(OpCode, _), LOpCode),


    availability_all_surgeries2(LOpCode, Room, Day, 0),  
    !,
        % Registrar o tempo de fim
    get_time(EndTime),
    
    % Calcular e mostrar o tempo total de execução
    TotalTime is EndTime - Ti.


% Caso base: Nenhuma cirurgia a ser agendada
availability_all_surgeries2([], _, _, MaxEndTime) :-!.
% Caso recursivo: Selecionar e agendar cirurgias
availability_all_surgeries2(LOpCode, Room, Day, CurrentMaxEndTime) :-
    % Selecionar a próxima cirurgia com base no critério
    (   select_next_surgeryCriteria1(LOpCode, OpCode, Interval, LDoctorsSurgery, LStaffAnesthesy,LStaffCleaning)
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
        fail
    ).
    
% Predicado que escolhe a próxima cirurgia com base no primeiro tempo disponível
select_next_surgeryCriteria1(LOpCode, BestOpCode, Interval, LDoctorsSurgery, LStaffAnesthesy,LStaffCleaning) :-
   %write('Lista de operações a ser analisada: '), write(LOpCode), nl,
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
    SortedCandidates = [(BestOpCode, Interval, LDoctorsSurgery, LStaffAnesthesy,LStaffCleaning) | _].


% Predicado que escolhe a próxima cirurgia com base no menor número de staff envolvido
select_next_surgeryCriteria2(LOpCode, BestOpCode, Interval, LDoctorsSurgery, LStaffAnesthesy,LStaffCleaning) :-
    write('Lista de operações a ser analisada: '), write(LOpCode), nl,
    findall(
        (OpCode, Interval, LDoctorsSurgery, LStaffAnesthesy, LStaffCleaning, StaffCount),
        (   
            member(OpCode, LOpCode),
            surgery_id(OpCode, OpType),
            surgery(OpType, _, _, _),
            availability_operation(OpCode, _, _, Interval, LDoctorsSurgery, LStaffAnesthesy, LStaffCleaning),
            length(LDoctorsSurgery, NumDoctors),
            length(LStaffAnesthesy, NumAnesthesy),
            length(LStaffCleaning, NumCleaning),
            StaffCount is NumDoctors + NumAnesthesy + NumCleaning
        ),
        Candidates
    ),
    maplist(
        % Extract the 5th element for sorting
        [(_, _, _, _,_, StaffCount), StaffCount]>>true,
        Candidates, StaffCounts
    ),
    % Pair candidates with their StaffCounts for sorting
    pairs_keys_values(Pairs, StaffCounts, Candidates),
    keysort(Pairs, SortedPairs),
    pairs_values(SortedPairs, SortedCandidates),

    write('Candidatos ordenados: '), write(SortedCandidates), nl,

    % Extract the best candidate
    SortedCandidates = [(BestOpCode, Interval, LDoctorsSurgery, LStaffAnesthesy, LStaffCleaning,_) | _],
    write('Melhor OpCode: '), write(BestOpCode), nl.


