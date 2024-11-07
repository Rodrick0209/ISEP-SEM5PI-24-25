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
    http_open('https://localhost:5001/api/AvailabilitySlots/GetAll', Reply,
               [cert_verify_hook(cert_accept_any)]),
    json_read_dict(Reply, Data),
    writeln(Data),
    saveDoctorSchedules(Data).  % Salva os dados



% Remove todos os fatos de agenda_staff/3
clear_agenda_staff :-
    retractall(agenda_staff(_, _, _)).