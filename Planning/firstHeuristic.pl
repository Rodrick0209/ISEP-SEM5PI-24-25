:- consult(algorithm).

schedule_all_surgeries_heuristic(Room, Day) :-
    retractall(agenda_staff1(_, _, _)),
    retractall(agenda_operation_room1(_, _, _)),
    retractall(availability(_, _, _)),
    findall(_, (agenda_staff(D, Day, Agenda), assertz(agenda_staff1(D, Day, Agenda))), _),
    findall(_, (agenda_operation_room(Room, Day, Agenda), assertz(agenda_operation_room1(Room, Day, Agenda))), _),
    findall(_, 
            (agenda_staff1(D, Day, L), 
             free_agenda0(L, LFA), 
             adapt_timetable(D, Day, LFA, LFA2), 
             assertz(availability(D, Day, LFA2))
            ), _),
    findall(OpCode, surgery_id(OpCode, _), LOpCode),
    availability_all_surgeries_heuristic(LOpCode, Room, Day), 
    !.

availability_all_surgeries_heuristic([], _, _).
availability_all_surgeries_heuristic([OpCode | LOpCode], Room, Day) :-
    surgery_id(OpCode, OpType),                
    surgery(OpType, _, TSurgery, _),           
    availability_operation(OpCode, Room, Day, LPossibilities, LDoctors), 
    earliest_doctor_availability(LDoctors, TSurgery, LPossibilities, (TinS, TfinS)), 
    retract(agenda_operation_room1(Room, Day, Agenda)),
    insert_agenda((TinS, TfinS, OpCode), Agenda, Agenda1),
    assertz(agenda_operation_room1(Room, Day, Agenda1)),
    insert_agenda_doctors((TinS, TfinS, OpCode), Day, LDoctors),
    availability_all_surgeries_heuristic(LOpCode, Room, Day).

earliest_doctor_availability(LDoctors, TSurgery, LPossibilities, (TinS, TfinS)) :-
    findall((Doctor, Start, End), 
            (member(Doctor, LDoctors), 
             member((Start, End), LPossibilities), 
             End - Start >= TSurgery), 
            AvailableSlots),
    sort(2, @=<, AvailableSlots, [(Doctor, TinS, End) | _]),
    TfinS is TinS + TSurgery.

obtain_heuristic_sol(Room, Day, AgOpRoom, LAgDoctors, TFinOp) :-
    get_time(Ti),
    (obtain_heuristic_sol1_heuristic(Room, Day); true),
    retract(better_sol(Day, Room, AgOpRoom, LAgDoctors, TFinOp)),
    write('Final Result: AgOpRoom='), write(AgOpRoom), nl,
    write('LAgDoctors='), write(LAgDoctors), nl,
    write('TFinOp='), write(TFinOp), nl,
    get_time(Tf),
    T is Tf - Ti,
    write('Tempo de geracao da solucao:'), write(T), nl.

obtain_heuristic_sol1_heuristic(Room, Day) :-
    asserta(better_sol(Day, Room, _, _, 1441)),
    findall(OpCode, surgery_id(OpCode, _), LOC), !,
    permutation(LOC, LOpCode),
    retractall(agenda_staff1(_, _, _)),
    retractall(agenda_operation_room1(_, _, _)),
    retractall(availability(_, _, _)),
    findall(_, (agenda_staff(D, Day, Agenda), assertz(agenda_staff1(D, Day, Agenda))), _),
    agenda_operation_room(Room, Day, Agenda), assert(agenda_operation_room1(Room, Day, Agenda)),
    findall(_, (agenda_staff1(D, Day, L), free_agenda0(L, LFA), adapt_timetable(D, Day, LFA, LFA2), assertz(availability(D, Day, LFA2))), _),
    availability_all_surgeries_heuristic(LOpCode, Room, Day),
    agenda_operation_room1(Room, Day, AgendaR),
    update_better_sol(Day, Room, AgendaR, LOpCode),
    fail.
