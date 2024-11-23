% Bibliotecas 
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_cors)).
:- use_module(library(date)).
:- use_module(library(random)).
:- consult('algorithm').

% Bibliotecas JSON
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).

% Configuração CORS
:- set_setting(http:cors, [*]).

:- consult('getInfoFromBd').

% Definição dos handlers
:- http_handler('/greet', greet_handler, []).
:- http_handler('/planning/getSchedule', getSchedule, []).

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
    format('Access-Control-Allow-Headers: Content-Type~n'),
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

% Handler para a rota /planning/getSchedule
getSchedule(Request) :-
    set_cors_headers,
    (   member(method(options), Request) ->
        format('~n')  % Responde OPTIONS com cabeçalhos CORS
    ;   
        % Processa requisições normais
        http_parameters(Request, [
            room(Room, []),
            day(Day, [])
        ]),

        %%Se o better sol eu meter os parametros manuais Do genero better_sol(20241130, or1, X, _, Z). funciona mas Do frontEnd nao 

        (   better_sol(20241130, or1, X, _, Z) ->
            % Converte X e Y para JSON
            convert_tuples_to_json(X, XJson),
            reply_json(json([
                room=Room,
                day=Day,
                agOpRoomBetter=XJson,
                tFinOp=Z
            ]))
        ;   reply_json(json([
                error="No better solution found",
                day=Day,
                room=Room
            ]), [status(404)])
        )
    ).


% Predicado de teste para depuração
testPredicate :-
    better_sol(20241130, or1, X, _, Z),
    convert_tuples_to_json(X, XJson),
    convert_assignments_to_json(Y, YJson),
    write(XJson), nl,
    write(YJson), nl.

% Servidor HTTP
server(Port) :-						
    http_server(http_dispatch, [port(Port)]).
