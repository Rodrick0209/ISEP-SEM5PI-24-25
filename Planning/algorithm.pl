
:- dynamic availability/3.
:- dynamic agenda_staff/3.
:- dynamic agenda_staff1/3.
:-dynamic agenda_operation_room/3.
:-dynamic agenda_operation_room1/3.
:-dynamic better_sol/5.


%Factos  agenda staff
agenda_staff(d001,20241028,[(720,790,m01)]). 
agenda_staff(d002,20241028,[(850,900,m02),(901,960,m02),(1380,1440,c02)]). 
agenda_staff(d003,20241028,[(720,790,m01),(910,980,m02)]).
agenda_staff(d004,20241028,[]).
agenda_staff(d005,20241028,[]).


%Horas de saida e entrada de cada staff
timetable(d001,20241028,(480,1400)).
timetable(d002,20241028,(500,1440)).
timetable(d003,20241028,(520,1320)).
timetable(d004,20241028,(480,1440)).
timetable(d005,20241028,(480,1440)).

%Registar o staff na base de conhecimento
staff(d001,doctor,orthopaedist,[so2,so3,so4]).
staff(d002,doctor,orthopaedist,[so2,so3,so4]).
staff(d003,doctor,orthopaedist,[so2,so3,so4]).
staff(d004,doctor,anesthesist,[so2,so3,so4]).
staff(d005,doctor,anesthesist,[so2,so3,so4]).

%surgery(SurgeryType,TAnesthesia,TSurgery,TCleaning). 
%Registar tipos de cirurgia na base de conhecimento 
surgery(so2,45,60,45).
surgery(so3,45,90,45).
surgery(so4,45,75,45).

surgery_specializationsRequired(so2,[[1,anesthesist]]).



%Criar cirurgias na base de conhecimento em formato (idCirurgia,idTipoCirurgia)
surgery_id(so100001,so2).
%surgery_id(so100002,so3).
%surgery_id(so100003,so4).
%surgery_id(so100004,so2).
%surgery_id(so100005,so4).


surgery_specializations(so2,[anesthesist]).


%Atribuicao da cirurgia a cada staff em formato (idCirurgia, idStaff)
assignment_surgery(so100001,d001).
assignment_surgery(so100002,d002).
assignment_surgery(so100003,d003).
assignment_surgery(so100004,d001).
assignment_surgery(so100004,d002).
assignment_surgery(so100005,d002).
assignment_surgery(so100005,d003).



%Facto que registar a agenda da sala em formato (idSala, data, Lista(tInicial,tFinal,idCirurgia))
agenda_operation_room(or1,20241028,[(520,579,so100000),(1000,1059,so099999)]).


%Predicado para retornar a agenda livre
free_agenda0([],[(0,1440)]). %Se a agenda tiver vazia retorna 0,1440 ou seja disponivel o dia todo
free_agenda0([(0,Tfin,_)|LT],LT1):-!, free_agenda1([(0,Tfin,_)|LT],LT1). %Caso para o intervalo começado em 0
free_agenda0([(Tin,Tfin,_)|LT],[(0,T1)|LT1]):- T1 is Tin-1, free_agenda1([(Tin,Tfin,_)|LT],LT1). %Caso para quando começa num intervalo diferente de 0

free_agenda1([(_,Tfin,_)],[(T1,1440)]):-Tfin\==1440,!,T1 is Tfin+1.  %Caso base para adicionar o intervalo antes Do minuto 1440
free_agenda1([(_,_,_)],[]). %Caso base para quando nao existem mais elementos a adicionar a LT1
free_agenda1([(_,T,_),(T1,Tfin2,_)|LT],LT1):-Tx is T+1,T1==Tx,!, free_agenda1([(T1,Tfin2,_)|LT],LT1). %Se o tempo final de um horario + 1 é igual ao tempo inicial seguinte ignora pois nao existe tempo livre entre esses 2 
free_agenda1([(_,Tfin1,_),(Tin2,Tfin2,_)|LT],[(T1,T2)|LT1]):-T1 is Tfin1+1,T2 is Tin2-1, free_agenda1([(Tin2,Tfin2,_)|LT],LT1). %Horario disponivel é igual a tempo final de um bloco +1 e o tempo inicial Do seguinte -1,



%Predicado que vai adaptar na hora de de entrada e saida Do medico(InTime,FinTime) e colocar a agenda de acordo com esses 2 condicoes
%Treatin recebe o tempo de entrada e a lista de horario Do medico e retornar a lista adaptada
%Treatfin recebe o tempo de saida e a lista adaptada Do Treatin e retorna o horario adaptado à hora de saida 
adapt_timetable(D,Date,LFA,LFA2):-timetable(D,Date,(InTime,FinTime)),treatin(InTime,LFA,LFA1),treatfin(FinTime,LFA1,LFA2). 



treatin(InTime,[(In,Fin)|LFA],[(In,Fin)|LFA]):-InTime=<In,!. %Condicao paragem - Verifica se o tempo In é menor que o tempo de entrada
treatin(InTime,[(_,Fin)|LFA],LFA1):-InTime>Fin,!,treatin(InTime,LFA,LFA1). %Se o tempo de entrada por depois da tempoFinal Do primeiro bloco Do horario avança para o proximo bloco 
treatin(InTime,[(_,Fin)|LFA],[(InTime,Fin)|LFA]). %Condicao paragem - Se o tempo de entrada não For depois Do tempoFinal define como tempo de inicio Do primeiro bloco a hora de entrada 
treatin(_,[],[]). %Caso nao tenha blocos de horario retorna vazio.

%Faz igual ao de cima mas vai buscar o ultimo bloco de horario e compara com o tempo de saida
treatfin(FinTime,[(In,Fin)|LFA],[(In,Fin)|LFA1]):-FinTime>=Fin,!,treatfin(FinTime,LFA,LFA1).
treatfin(FinTime,[(In,_)|_],[]):-FinTime=<In,!.
treatfin(FinTime,[(In,_)|_],[(In,FinTime)]).
treatfin(_,[],[]).



%Predicado que vai intersetar As agendas em comum 
intersect_all_agendas([Name],Date,LA):-!,availability(Name,Date,LA). %Caso em que só existe um nome na lista, retornar a disponibilidade desse staff com esse name 
intersect_all_agendas([Name|LNames],Date,LI):-  availability(Name,Date,LA),
                                                intersect_all_agendas(LNames,Date,LI1),
                                                intersect_2_agendas(LA,LI1,LI).
                                                %Caso em que existem varios nomes na lista, vai ver a disponibilidade  De cada Staff e vai percorrendo todos os Staff
                                                %No backTracking vai intersetar a agenda LA vinda Do availability com o resultados das agendas processadas anteriormente(LI1) e adiciona o resultado à lista LI  



intersect_2_agendas([],_,[]). %Caso base: Intersecao de uma lista vazia com outra lista qualquer é uma lista vazia
intersect_2_agendas([D|LD],LA,LIT):-	intersect_availability(D,LA,LI,LA1),    
					                    intersect_2_agendas(LD,LA1,LID),
					                    append(LI,LID,LIT).
                                        %Vai percorrendo cada bloco de horario e intersetando com a lista resultante  



intersect_availability((_,_),[],[],[]). %Condicao Paragem : Intersecao com vazio retorna vazio
intersect_availability((_,Fim),[(Ini1,Fim1)|LD],[],[(Ini1,Fim1)|LD]):-
		Fim<Ini1,!.   %Condicao Paragem : Se o valor Fim For menor que o inicial  

intersect_availability((Ini,Fim),[(_,Fim1)|LD],LI,LA):-
		Ini>Fim1,!,intersect_availability((Ini,Fim),LD,LI,LA).     %Caso o valor Inicial For maior que o valor Final Do que o bloco da agenda avança para o proximo

intersect_availability((Ini,Fim),[(Ini1,Fim1)|LD],[(Imax,Fmin)],[(Fim,Fim1)|LD]):-
                                                                                    Fim1>Fim,!,
                                                                                    min_max(Ini,Ini1,_,Imax),
                                                                                    min_max(Fim,Fim1,Fmin,_).

intersect_availability((Ini,Fim),[(Ini1,Fim1)|LD],[(Imax,Fmin)|LI],LA):-
                                                                            Fim>=Fim1,!,
                                                                            min_max(Ini,Ini1,_,Imax),
                                                                            min_max(Fim,Fim1,Fmin,_),
                                                                            intersect_availability((Fim1,Fim),LD,LI,LA).

%Retorna o maximo No 4 argumento e o minimo No 3
min_max(I,I1,I,I1):- I<I1,!.
min_max(I,I1,I1,I).



%Metodo que marcar todas As cirurgias criando os devidos factos
schedule_all_surgeries(Room,Day):-
    retractall(agenda_staff1(_,_,_)), %Apaga todas os factos agenda_staff1
    retractall(agenda_operation_room1(_,_,_)), %Apaga todos os factos agenda_operation_room1
    retractall(availability(_,_,_)), %Apaga todos os factos availability
    findall(_,(agenda_staff(D,Day,Agenda),assertz(agenda_staff1(D,Day,Agenda))),_), %Procura todos os factos agenda_staff e cria factos agenda_staff1 a partir dos mesmos
    agenda_operation_room(Or,Date,Agenda),assert(agenda_operation_room1(Or,Date,Agenda)),   %cria o facto agenda_operation_room1 a partir Do facots agenda_operation_room
    findall(_,(agenda_staff1(D,Date,L), %encontra todas As agenda_staff1
    free_agenda0(L,LFA),adapt_timetable(D,Date,LFA,LFA2), %Procura a agenda livre e adapta a agenda de acordo com horarios de entrada e saida
    assertz(availability(D,Date,LFA2))),_), %cria availability de acordo com a agenda resultante processos anteriores 
    findall(OpCode,surgery_id(OpCode,_),LOpCode), % Vai buscar todas As cirurgias por marcar
    availability_all_surgeries(LOpCode,Room,Day),!. 

availability_all_surgeries([],_,_).
availability_all_surgeries([OpCode|LOpCode],Room,Day):- surgery_id(OpCode,OpType),  %Vai buscar o facto surgery_id retornando OpCode e OpType
                                                        surgery(OpType,_,TSurgery,_), %Vai buscar o facto surgery retornando o TSurgery
                                                        availability_operation(OpCode,Room,Day,LPossibilities,LDoctors), 
                                                        schedule_first_interval(TSurgery,LPossibilities,(TinS,TfinS)),
                                                        retract(agenda_operation_room1(Room,Day,Agenda)),
                                                        insert_agenda((TinS,TfinS,OpCode),Agenda,Agenda1),
                                                        assertz(agenda_operation_room1(Room,Day,Agenda1)),
                                                        insert_agenda_doctors((TinS,TfinS,OpCode),Day,LDoctors),
                                                        availability_all_surgeries(LOpCode,Room,Day).



availability_operation(OpCode,Room,Day,LPossibilities,LDoctors):-
    surgery_id(OpCode,OpType),
    surgery(OpType,TAnesthesia,TSurgery,_),
    TAnesthesiaAndSurgery is TAnesthesia + TSurgery, %Calcular o tempo total da cirurgia
    write('TAnesthesiaAndSurgery='),write(TAnesthesiaAndSurgery),nl,
    findall(Doctor,assignment_surgery(OpCode,Doctor),LDoctors), %Vai buscar todos os medicos que tem a cirurgia atribuida
    intersect_all_agendas(LDoctors,Day,LA), %Vai intersetar todas As agendas dos medicos
    write('Agenda livre medicos atribuidas a cirugia='),write(LA),nl,
    find_available_staff(OpType, Day, LStaffDynamic), %Vai buscar o staff necessario para a cirurgia
    write('LStaffDynamic='),write(LStaffDynamic),nl,
    intersect_all_agendas(LStaffDynamic,Day,LAgendaResultAnestesia),
    write('LAgendaResultAnestesia livre='),write(LAgendaResultAnestesia),nl,

    agenda_operation_room1(Room,Day,LAgenda), %Vai buscar a agenda da sala
    free_agenda0(LAgenda,LFAgRoom), %Vai buscar a agenda livre da sala
    write('Agenda livre da sala='),write(LFAgRoom),nl,
    %intersect_2_agendas(LA,LFAgRoom,LIntAgDoctorsRoom), %Vai intersetar a agenda dos medicos com a agenda livre da sala
    intersect_2_agendas(LAgendaResultAnestesia,LFAgRoom,LIntAgDoctorsRoom), %Vai intersetar a agenda dos medicos com a agenda livre da sala
    write('Lista intersecao salas e anestesia='),write(LIntAgDoctorsRoom),nl,
    remove_unf_intervals(TAnesthesiaAndSurgery,LIntAgDoctorsRoom,LPossibilities), %Vai remover os intervalos que nao sao suficientes para a cirurgia
    write('Lista possibilidades='),write(LPossibilities),nl,
    write('Lista agenda doctors= '),write(LA),nl,
    available_schedule_doctor_anesthesy(LPossibilities,LA,TAnesthesia,TSurgery,LPossibilities1, LPossibilitiesDoctor),
    write('Lista possibilidades final='),write(LPossibilities1),nl,
    write('Lista possibilidades doctors= '),write(LPossibilitiesDoctor),nl.


available_schedule_doctor_anesthesy([], _, _, _, [],[]).
available_schedule_doctor_anesthesy([(Start, End)|Rest], LAgendaDoctors, TAnesthesia, TSurgery, [(Start, End)|LPossibilities1], LPossibilitiesDoctor) :-
    SurgeryStart is Start + TAnesthesia,
    SurgeryEnd is End,
    
    write('TEmpo cirurgia= '), write(TSurgery), nl,
    write('SurgeryStart='), write(SurgeryStart), write(', SurgeryEnd='), write(SurgeryEnd), nl,
    write('LAgendaDoctors='), write(LAgendaDoctors), nl,
    intersect_2_agendas([(SurgeryStart, SurgeryEnd)], LAgendaDoctors, AvailableIntervals),
    write('AvailableIntervals='), write(AvailableIntervals), nl,
    AvailableIntervals \= [],
    findall((IntStart, IntEnd), 
            (member((IntStart, IntEnd), AvailableIntervals), 
             IntervalDuration is IntEnd - IntStart,
             IntervalDuration >= TSurgery),
            ValidIntervals),!,
    write('ValidIntervals='), write(ValidIntervals), nl,
    ValidIntervals \= [],
    append(ValidIntervals, LPossibilitiesDoctorRest, LPossibilitiesDoctor),
    available_schedule_doctor_anesthesy(Rest, LAgendaDoctors, TAnesthesia, TSurgery, LPossibilities1, LPossibilitiesDoctorRest).
available_schedule_doctor_anesthesy([_|Rest], LAgendaDoctors, TAnesthesia, TSurgery, LPossibilities1, LPossibilitiesDoctor) :-
    available_schedule_doctor_anesthesy(Rest, LAgendaDoctors, TAnesthesia, TSurgery, LPossibilities1, LPossibilitiesDoctor).


remove_unf_intervals(_,[],[]).
remove_unf_intervals(TSurgery,[(Tin,Tfin)|LA],[(Tin,Tfin)|LA1]):-DT is Tfin-Tin+1,TSurgery=<DT,!,
    remove_unf_intervals(TSurgery,LA,LA1).
remove_unf_intervals(TSurgery,[_|LA],LA1):- remove_unf_intervals(TSurgery,LA,LA1).


schedule_first_interval(TSurgery,[(Tin,_)|_],(Tin,TfinS)):-
    TfinS is Tin + TSurgery - 1.

insert_agenda((TinS,TfinS,OpCode),[],[(TinS,TfinS,OpCode)]).
insert_agenda((TinS,TfinS,OpCode),[(Tin,Tfin,OpCode1)|LA],[(TinS,TfinS,OpCode),(Tin,Tfin,OpCode1)|LA]):-TfinS<Tin,!.
insert_agenda((TinS,TfinS,OpCode),[(Tin,Tfin,OpCode1)|LA],[(Tin,Tfin,OpCode1)|LA1]):-insert_agenda((TinS,TfinS,OpCode),LA,LA1).

insert_agenda_doctors(_,_,[]).
insert_agenda_doctors((TinS,TfinS,OpCode),Day,[Doctor|LDoctors]):-
    retract(agenda_staff1(Doctor,Day,Agenda)),
    insert_agenda((TinS,TfinS,OpCode),Agenda,Agenda1),
    assert(agenda_staff1(Doctor,Day,Agenda1)),
    insert_agenda_doctors((TinS,TfinS,OpCode),Day,LDoctors).


obtain_better_sol(Room,Day,AgOpRoomBetter,LAgDoctorsBetter,TFinOp):-
		get_time(Ti),
		(obtain_better_sol1(Room,Day);true),
		retract(better_sol(Day,Room,AgOpRoomBetter,LAgDoctorsBetter,TFinOp)),
            write('Final Result: AgOpRoomBetter='),write(AgOpRoomBetter),nl,
            write('LAgDoctorsBetter='),write(LAgDoctorsBetter),nl,
            write('TFinOp='),write(TFinOp),nl,
		get_time(Tf),
		T is Tf-Ti,
		write('Tempo de geracao da solucao:'),write(T),nl.

obtain_better_sol1(Room,Day):-
    asserta(better_sol(Day,Room,_,_,1441)),
    findall(OpCode,surgery_id(OpCode,_),LOC),!,
    permutation(LOC,LOpCode),
    retractall(agenda_staff1(_,_,_)),
    retractall(agenda_operation_room1(_,_,_)),
    retractall(availability(_,_,_)),
    findall(_,(agenda_staff(D,Day,Agenda),assertz(agenda_staff1(D,Day,Agenda))),_),
    agenda_operation_room(Room,Day,Agenda),assert(agenda_operation_room1(Room,Day,Agenda)),
    findall(_,(agenda_staff1(D,Day,L),
    free_agenda0(L,LFA),
    adapt_timetable(D,Day,LFA,LFA2),
    assertz(availability(D,Day,LFA2))),_),
    availability_all_surgeries(LOpCode,Room,Day),
    agenda_operation_room1(Room,Day,AgendaR),
		update_better_sol(Day,Room,AgendaR,LOpCode),
		fail.

update_better_sol(Day,Room,Agenda,LOpCode):-
                better_sol(Day,Room,_,_,FinTime),
                reverse(Agenda,AgendaR),
                evaluate_final_time(AgendaR,LOpCode,FinTime1),
             write('Analysing for LOpCode='),write(LOpCode),nl,
             write('now: FinTime1='),write(FinTime1),write(' Agenda='),write(Agenda),nl,
		FinTime1<FinTime,
             write('best solution updated'),nl,
                retract(better_sol(_,_,_,_,_)),
                findall(Doctor,assignment_surgery(_,Doctor),LDoctors1),
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








%Estes 2 predicados servem para irmos buscar o staff necessario para uma cirurgia.
find_available_staff(OpType, Day, LStaff) :-
    % Obter As especializações e quantidades necessárias para a cirurgia
    surgery_specializationsRequired(OpType, RequiredStaff),
    write('Passo 1: Especializações necessárias: '), write(RequiredStaff), nl,
    % Encontrar staff disponível para cada requisito
    findall(StaffForSpec, (
        member([Count, Specialization], RequiredStaff),
        find_staff_for_specialization(Specialization, Count, Day, StaffForSpec)
    ), StaffLists),
    % Concatenar listas para obter todos os staffs necessários
    flatten(StaffLists, LStaff).



find_staff_for_specialization(Specialization, Count, Day, LStaff) :-
    write('Passo 1: Especialização: '), write(Specialization), nl,
    write('Passo 2: Quantidade necessária: '), write(Count), nl,
    % Encontrar todos os staffs disponíveis com a especialização no dia especificado
    findall(StaffID, (staff(StaffID, _, Specialization, _)), AvailableStaff),
    write('Passo 3: Staff disponível: '), write(AvailableStaff), nl,
    % Selecionar apenas a quantidade necessária de staff
    length(LStaff, Count),
    append(LStaff, _, AvailableStaff).
