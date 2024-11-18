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
:- dynamic surgery_counter/1.% Declare the global counter for surgery IDs

% Initialize the counter if it doesn't exist
:- (retract(surgery_counter(_)) -> true ; assertz(surgery_counter(1))).


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

% Generate the next surgery ID
next_surgery_id(SurgeryId) :-
    retract(surgery_counter(N)),                
    format(atom(SurgeryId), 's10000~w', [N]),        
    NewN is N + 1,                              % Increment the counter
    assertz(surgery_counter(NewN)).             % Update the counter

% Salva os doutores
saveSurgeries(Data) :-
    % Percorre a lista de dados e adiciona como factos à base de dados Prolog
    forall(member(Surgery, Data),
        (
            forall(member(Surgery, Data),
                (
                    next_surgery_id(SurgeryId),
                    Surgery = _{fullName: SurgeryName, preparationPhase: PreparationPhase, surgeryPhase: SurgeryPhase, cleaningPhase: CleaningPhase},
                    PreparationPhase = _{duration: PreparationDuration},
                    SurgeryPhase = _{duration: SurgeryDuration},
                    CleaningPhase = _{duration: CleaningDuration},
                    atom_string(SurgeryName, SurgeryNameStr),  % Converte SurgeryName de string para átomo
                    atom_string(SurgeryId, SurgeryIdStr),  % Converte SurgeryId de string para átomo
                    assertz(surgery(SurgeryNameStr, PreparationDuration, SurgeryDuration, CleaningDuration)),
                    assertz(surgery_id(SurgeryIdStr, SurgeryName))
                )
            )
        )
    ).

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
    http_open('https://10.9.10.55:5001/api/AvailabilitySlots/GetAll', Reply,
               [cert_verify_hook(cert_accept_any)]),
    json_read_dict(Reply, Data),
    writeln(Data),
    saveDoctorSchedules(Data).  % Salva os dados

allSurgeries() :-
    http_open('https://10.9.10.55:5001/api/OperationType/GetAll', Reply,
               [cert_verify_hook(cert_accept_any)]),
    json_read_dict(Reply, Data),
    writeln(Data),
    saveSurgeries(Data).  % Salva os dados

% Remove todos os fatos de agenda_staff/3
clear_agenda_staff :-
    retractall(agenda_staff(_, _, _)).

% Remove todos os fatos de surgery/4
clear_surgery :-
    retractall(surgery(_, _, _, _)),
    retractall(surgery_id(_, _)).