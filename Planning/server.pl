:- use_module(library(http/http_server)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).  % Necessário para manipular parâmetros da solicitação

:- http_handler('/greet', greet_handler, []).

greet_handler(Request) :-
    % Define os cabeçalhos CORS
    format('Content-type: application/json~n'),
    format('Access-Control-Allow-Origin: *~n'),
    format('Access-Control-Allow-Methods: GET, POST, OPTIONS~n'),
    format('Access-Control-Allow-Headers: Content-Type~n'),
    format('~n'),  % Linha em branco para separar cabeçalhos do corpo

    (   member(method(options), Request) ->
        % Para requisições OPTIONS, retorne apenas uma resposta vazia
        true
    ;   % Para outras requisições, processe normalmente
        http_parameters(Request, [name(Name, [default('World')])]),  % Extrai o parâmetro "name"
        format('{"message": "Hello, ~w!"}', [Name])
    ).

:- http_server(http_dispatch, [port(8080)]).