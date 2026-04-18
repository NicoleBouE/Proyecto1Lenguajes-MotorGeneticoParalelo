-module(parte_erlang).
-export([start/1]).

% Dominio: Puerto (entero positivo)
% Codominio: void (efecto: inicia servidor TCP en el puerto especificado)
start(Puerto) ->
    {ok, L} = gen_tcp:listen(Puerto, [binary, {packet, 4}, {reuseaddr, true}, {active, false}]),
    io:format("Erlang listo en puerto ~p~n", [Puerto]),
    accept_loop(L).

% Dominio: L (socket de escucha)
% Codominio: never (bucle infinito aceptando conexiones)
accept_loop(L) ->
    {ok, S} = gen_tcp:accept(L),
    spawn(fun() -> sesion_scheme(S) end),
    accept_loop(L).

% Dominio: Socket (socket TCP conectado con cliente Scheme)
% Codominio: void (efecto: maneja una sesión completa con Scheme)
sesion_scheme(Socket) ->
    Puente = self(),
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            {ok, {puntos, Puntos, arboles, Arboles}} = parsear(Data),
            Resultados = [begin 
                F = calcular_fitness(Arbol, Puntos, Socket),
                {F, Arbol}
            end || Arbol <- Arboles],
            enviar_resultados(Socket, Resultados),
            sesion_scheme(Socket); % RECURSIÓN: Espera la siguiente gen de Scheme
        {error, closed} -> ok
    end.

% Dominio: Arbol (string con representación de árbol), Puntos (lista de tuplas {X,Y,Z}), Socket (socket TCP)
% Codominio: float (fitness calculado entre 0 y 1)
calcular_fitness(Arbol, Puntos, Socket) ->
    Errores = [begin 
        Msg = io_lib:format("(compute (~p ~p) ~s)", [X, Y, Arbol]),
        gen_tcp:send(Socket, list_to_binary(Msg)),
        case gen_tcp:recv(Socket, 0) of
            {ok, D} -> {ok, {result, V}} = parsear(D), abs(Z - V);
            _ -> 100.0
        end
    end || {X, Y, Z} <- Puntos],
    1.0 / (1.0 + lists:sum(Errores)).

% Dominio: Socket (socket TCP), Resultados (lista de tuplas {Fitness, Arbol})
% Codominio: void (efecto: envía resultados a Scheme)
enviar_resultados(Socket, Resultados) ->
    ResStr = [io_lib:format("(~p . ~s)", [F, A]) || {F, A} <- Resultados],
    Msg = io_lib:format("(results (~s))", [string:join(ResStr, " ")]),
    gen_tcp:send(Socket, list_to_binary(Msg)).

% Dominio: Bin (binary con datos recibidos del socket)
% Codominio: {ok, Term} donde Term es el término Erlang parseado, o error
parsear(Bin) ->
    {ok, T, _} = erl_scan:string(binary_to_list(Bin)),
    erl_parse:parse_term(T).