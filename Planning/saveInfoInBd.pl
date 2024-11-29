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
:- dynamic agenda_operation_room/3.  % Declare the agenda_operation_room/3 predicate as dynamic

saveAppointment(_, _, []).
saveAppointment(Room, DayRaw, [(StartMinutes, EndMinutes, OperationRequestId) | Rest]) :-
    atom_number(DayRaw, DayNum),
    Year is DayNum // 10000,
    Month is (DayNum // 100) mod 100,
    Day is DayNum mod 100,
    format(atom(Date), '~d-~|~`0t~d~2+-~|~`0t~d~2+', [Year, Month, Day]),
    
    StartHour is StartMinutes // 60,
    StartMinute is StartMinutes mod 60,
    format(atom(Start), '~|~`0t~d~2+:~|~`0t~d~2+', [StartHour, StartMinute]),
    
    EndHour is EndMinutes // 60,
    EndMinute is EndMinutes mod 60,
    format(atom(End), '~|~`0t~d~2+:~|~`0t~d~2+', [EndHour, EndMinute]),
    
    URL = 'https://10.9.10.55:5001/api/Appointment',
    
    Body = _{
        appointmentTimeSlotDto: _{
            date: Date,
            timeSlot: _{
                startTime: Start,
                endTime: End
            }
        },
        operationRoomId: Room,
        operationRequestId: OperationRequestId
    },
    
    http_post(URL, json(Body), _, [json_object(dict)]),
    
    saveAppointment(Room, DayRaw, Rest).


    