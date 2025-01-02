% Bibliotecas 
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_cors)).
:- use_module(library(date)).
:- use_module(library(random)).

% Bibliotecas JSON
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).

% Configuração CORS
:- set_setting(http:cors, [*]).

:- consult('getInfoFromBd').
:- consult('saveInfoInBd').
:- consult('algorithm').
:- consult('geneticAlgorithmForSurgeries').

% Definição dos handlers
:- http_handler('/greet', greet_handler, []).
:- http_handler('/planning/getSchedule', getSchedule, []).
:- http_handler('/planning/getHeuristicSchedule', getHeuristicSchedule, []).
:- http_handler('/planning/getGeneticAlgorithmSchedule', getGeneticAlgorithmSchedule, []).

% Tratamento para a rota /greet
greet_handler(Request) :-
    % Define os cabeçalhos CORS
    set_cors_headers,
    (   member(method(options), Request) ->
        % Para requisições OPTIONS, retorne apenas uma resposta vazia
        format('~n')
    ;   % Para outras requisições, processe normalmente
        http_parameters(Request, [name(Name, [default('World')])]),  % Extrai o parâmetro "name"
        format('{"message": "Hello, ~w!"}', [Name])
    ).

% Converte uma lista de tuplas (Start, End, OperationId) para uma lista de objetos JSON
% Define cabeçalhos CORS para todas as respostas
set_cors_headers :-
    format('Access-Control-Allow-Origin: *~n'),
    format('Access-Control-Allow-Methods: GET, POST, OPTIONS~n'),
    format('Access-Control-Allow-Headers: Content-Type, Authorization~n'),
    format('Access-Control-Allow-Credentials: true~n').



% Converte lista de tuplas (Start, End, OperationId) para JSON
convert_tuples_to_json([], []).
convert_tuples_to_json([(Start, End, OperationId) | Tail], [json{start: Start, end: End, operationId: OperationIdStr} | JsonTail]) :-
    atom_string(OperationId, OperationIdStr),  % Converte OperationId para string
    convert_tuples_to_json(Tail, JsonTail).

% Converte lista de pares (DoctorId, Assignments) para JSON
convert_assignments_to_json([], []).
convert_assignments_to_json([(DoctorId, Assignments) | Tail], [json{doctorId: DoctorIdStr, assignments: AssignmentsJson} | JsonTail]) :-
    atom_string(DoctorId, DoctorIdStr),  % Converte DoctorId para string
    convert_tuples_to_json(Assignments, AssignmentsJson),
    convert_assignments_to_json(Tail, JsonTail).

getSchedule(Request) :-
    set_cors_headers,
    (   member(method(options), Request) ->
        format('~n')  % Responds to OPTIONS with CORS headers
    ;   % Process normal requests
        http_parameters(Request, 
            [ 
                day(Day, [number]),
                room(Room, [atom])
            ]
        ),

        % Call predicates to retrieve data
        doctorSchedules,
        surgeryTypes,
        surgeries,
        operationRooms,

        (   obtain_better_sol(Room, Day, Appointments, _, Z) ->
            % Check if the value of Z is 1441
            (   Z = 1441 ->
                reply_json(json([
                    error="Invalid scheduling value (Z = 1441)",
                    day=Day,
                    room=Room
                ]), [status(400)])
            ;   % If scheduling is successful, save to the database and respond
                (   saveAppointment(Room, Day, Appointments) ->
                    reply_json(json([
                        room=Room,
                        day=Day,
                        agOpRoomBetter=Appointments,
                        tFinOp=Z
                    ]))
                ;   % Handle failure during saving
                    reply_json(json([
                        error="Failed to save appointments",
                        day=Day,
                        room=Room
                    ]), [status(500)])
                )
            )
        ;   reply_json(json([
                error="No better solution found",
                day=Day,
                room=Room
            ]), [status(404)])
        )
    ).

% Handler for the route /planning/getHeuristicSchedule
getHeuristicSchedule(Request) :-
    set_cors_headers,
    (   member(method(options), Request) ->
        format('~n')  % Responds to OPTIONS with CORS headers
    ;   % Process normal requests
        http_parameters(Request, 
            [ 
                day(Day, [number]),
                room(Room, [atom])
            ]
        ),

        % Call predicates to retrieve data
        doctorSchedules,
        surgeryTypes,
        surgeries,
        operationRooms,

        % Call heuristic scheduling logic
        (   schedule_all_surgeriesHeuristic(Room, Day) ->
            % If heuristic succeeds, attempt to assign surgeries to the room
            (   agenda_operation_room1(Room, Day, Appointments) ->
                % Save appointments to the database
                (   saveAppointment(Room, Day, Appointments) ->
                    % Respond with the scheduled appointments
                    reply_json(json([
                        room=Room,
                        day=Day,
                        agOpRoomBetter=Appointments
                    ]))
                ;   % Handle failure during saving
                    reply_json(json([
                        error="Failed to save appointments",
                        day=Day,
                        room=Room
                    ]), [status(500)])
                )
            ;   reply_json(json([
                    error="Solution not found, try scheduling to another day",
                    day=Day,
                    room=Room
                ]), [status(404)])
            )
        ;   % If heuristic scheduling fails
            reply_json(json([
                error="Heuristic scheduling failed, try another room or day",
                day=Day,
                room=Room
            ]), [status(404)])
        )
    ).



getGeneticAlgorithmSchedule(Request) :-
    set_cors_headers,
    (   member(method(options), Request) ->
        format('~n')
    ;   http_parameters(Request, 
            [ 
                populationSize(PopulationSize, [number]),
                generations(Generations, [number]),
                crossoverRate(CrossoverRate, [number]),
                date(Date, [number]),
                bestIndividualsToBeKeptRate(BestIndividualsToBeKeptRate, [number]),
                timeLimit(TimeLimit, [number]),
                lowerCostWanted(LowerCostWanted, [number]),
                mutationRate(MutationRate, [number])
            ]
        ),
        			
    (retract(generations(_));true), asserta(generations(Generations)),
	(retract(population(_));true), asserta(population(PopulationSize)),
	PC is CrossoverRate/100, 
	(retract(prob_crossover(_));true), 	asserta(prob_crossover(PC)),
	PM is MutationRate/100, 
	(retract(prob_mutation(_));true), asserta(prob_mutation(PM)),
    PI is BestIndividualsToBeKeptRate/100,
    (retract(percentage_individuals(_));true), asserta(percentage_individuals(PI)),
    (retract(lowerCostWanted(_));true), asserta(lowerCostWanted(LowerCostWanted)),
    (retract(time_limit(_));true), asserta(time_limit(TimeLimit)),
    (retract(dateToSchedule(_));true), asserta(dateToSchedule(Date)),

        (   generate ->
            agenda_operation_room1(or1, Date, XRaw),
            agenda_operation_room1(or2, Date, YRaw),
            agenda_operation_room1(or3, Date, ZRaw),
            convert_tuples_to_json(XRaw, X),
            convert_tuples_to_json(YRaw, Y),
            convert_tuples_to_json(ZRaw, Z),

            reply_json(json([
                        schedules=[
                            json([room="or1", schedule=X]),
                            json([room="or2", schedule=Y]),
                            json([room="or3", schedule=Z])
                        ],
                        date=Date                        
                            ]), [status(200)])
        ;   reply_json(json([message="Genetic Algorithm failed to execute"]), [status(400)])
        )
    
    ).



% Servidor HTTP
server(Port) :-
    http_server(http_dispatch, [port(Port), ip('0.0.0.0')]).