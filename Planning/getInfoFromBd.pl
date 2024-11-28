
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/json)).

:- dynamic agenda_staff/3.  % Define o predicado dinâmico
:- dynamic surgery/4.        % Declare the surgery/4 predicate as dynamic
:- dynamic surgery_id/2.     % Declare the surgery_id/2 predicate as dynamic
:- dynamic assignment_surgery/2.  % Declare the assignment_surgery/2 predicate as dynamic


%% Função principal para salvar a agenda dos medicos num ficheiro
saveDoctorSchedules(Data) :-
    % Percorre a lista de dados e adiciona como factos à base de dados Prolog
    forall(member(Staff, Data),
        (
            Staff = _{staffId: StaffIdStr, dailyAvailabilities: DailyAvailabilities},
            atom_string(StaffId, StaffIdStr),  % Converte StaffId de string para átomo
            forall(member(Daily, DailyAvailabilities),
                (
                    Daily = _{date: DateStr, timeSlots: TimeSlots},
                    parse_date(DateStr, DateInt),
                    convert_time_slots(TimeSlots, TimeSlotsFormatted),
                    assertz(agenda_staff(StaffId, DateInt, TimeSlotsFormatted))
                )
            )
        )
    ).

saveSurgeryTypes([]).
saveSurgeryTypes([Surgery | Rest]) :-
    Surgery = _{
        id: _,
        name: SurgeryName,
        status: _,
        preparationPhase: PreparationPhase,
        surgeryPhase: SurgeryPhase,
        cleaningPhase: CleaningPhase,
        specialization: _
    },
    
    PreparationPhase = _{
        id: _,
        duration: PreparationDuration,
        requiredStaff: _
    },
    SurgeryPhase = _{
        id: _,
        duration: SurgeryDuration,
        requiredStaff: _
    },
    CleaningPhase = _{
        id: _,
        duration: CleaningDuration,
        requiredStaff: _
    },
    atom_string(SurgeryName, SurgeryNameStr),
    assertz(surgery(SurgeryNameStr, PreparationDuration, SurgeryDuration, CleaningDuration)),

    saveSurgeryTypes(Rest).

getSurgeryById(SurgeryId, SurgeryName) :-
    format(atom(URL), 'https://10.9.10.55:5001/api/OperationType/~w', [SurgeryId]),
    http_open(URL, Reply, [cert_verify_hook(cert_accept_any)]),
    json_read_dict(Reply, Surgery),
    Surgery = _{
        id: SurgeryId,
        name: SurgeryName,
        status: _,
        preparationPhase: _,
        surgeryPhase: _,
        cleaningPhase: _,
        specialization: _
    }.


saveSurgeries([]).
saveSurgeries([Data | Rest]) :-

    Data = _{
        id: SurgeryId,
        deadLineDate: _, 
        priority: _, 
        patientId: _, 
        operationTypeId: Surgery, 
        doctorThatWillPerformId: Doctor, 
        doctorThatRequestedId: _
    },

    getSurgeryById(Surgery, SurgeryName),
    atom_string(SurgeryIdAtom, SurgeryId),
    atom_string(SurgeryNameAtom, SurgeryName),
    atom_string(DoctorAtom, Doctor),

    assertz(surgery(SurgeryNameAtom, _, _, _)),
    assertz(surgery_id(SurgeryIdAtom, SurgeryNameAtom)), 
    assertz(assignment_surgery(SurgeryIdAtom, DoctorAtom))  

    saveSurgeries(Rest).

% Converte os slots de tempo para o formato desejado
convert_time_slots(TimeSlots, TimeSlotsFormatted) :-
    findall((Start, End), 
        (member(TempSlot, TimeSlots),
        TempSlot = _{startMinute: Start, endMinute: End}), 
    TimeSlotsFormatted).

% Converte a string da data para o formato YYYYMMDD
parse_date(DateStr, DateInt) :-
    sub_string(DateStr, 0, 4, _, Year),
    sub_string(DateStr, 5, 2, _, Month),
    sub_string(DateStr, 8, 2, _, Day),
    atom_number(Year, Y),
    atom_number(Month, M),
    atom_number(Day, D),
    DateInt is Y * 10000 + M * 100 + D.

% Predicado auxiliar para formatar os slots de tempo
format_time_slots(Stream, []) :- write(Stream, ''), !.  % Se não houver slots, não faz nada.
format_time_slots(Stream, [(Start, End) | Rest]) :-
    format(Stream, '(~w, ~w)', [Start, End]),  % Formata o slot de tempo
    (   Rest \= [] -> write(Stream, ', '), format_time_slots(Stream, Rest)  % Se houver mais slots, adiciona vírgula
    ;   true  % Se for o último, não faz nada
    ).

doctorSchedules() :-
    clear_agenda_staff,
    http_open('https://10.9.10.55:5001/api/AvailabilitySlots/GetAll', Reply,
               [cert_verify_hook(cert_accept_any)]),
    json_read_dict(Reply, Data),
    writeln(Data),
    saveDoctorSchedules(Data).  % Salva os dados

allSurgeryTypes() :-
    clear_surgery,
    http_open('https://10.9.10.55:5001/api/OperationType/GetAll', Reply,
               [cert_verify_hook(cert_accept_any)]),
    json_read_dict(Reply, Data),
    writeln(Data),
    saveSurgeryTypes(Data).  % Salva os dados

allSurgeries() :-
    clear_surgery_id,
    http_open('https://10.9.10.55:5001/api/OperationRequest/GetAll', Reply,
               [cert_verify_hook(cert_accept_any)]),
    json_read_dict(Reply, Data),
    writeln(Data),
    saveSurgeries(Data).  % Salva os dados



% Remove todos os fatos de agenda_staff/3
clear_agenda_staff :-
    retractall(agenda_staff(_, _, _)).

% Remove todos os fatos de surgery/4
clear_surgery :-
    retractall(surgery(_, _, _, _)).

clear_surgery_id :-
    retractall(surgery_id(_, _)).