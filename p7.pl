:- dynamic paciente/4.
%Ejercicio 1
pensar_numero :-
    write('Piensa en un numero entre 1 y 1000 y escribe cualquier cosa cuando estes listo.'), nl,
    read(_),
    LimiteInferior = 1,
    LimiteSuperior = 1000,
    adivinar_numero(LimiteInferior, LimiteSuperior).

adivinar_numero(LimiteInferior, LimiteSuperior) :-
    NumeroAdivinado is (LimiteInferior + LimiteSuperior) // 2,
    write('Es '), write(NumeroAdivinado), write(' el numero que pensaste? (si/no)'), nl,
    read(Respuesta),
    proceso_respuesta(Respuesta, NumeroAdivinado, LimiteInferior, LimiteSuperior).

proceso_respuesta(si, Numero, _, _) :-
    write('El numero es '), write(Numero), write('.'), nl.
proceso_respuesta(no, Numero, LimiteInferior, LimiteSuperior) :-
    write('Es el numero mayor o menor que '), write(Numero), write('? (mayor/menor)'), nl,
    read(Respuesta),
    actualiza_limites(Respuesta, Numero, LimiteInferior, LimiteSuperior, NuevoLimiteInferior, NuevoLimiteSuperior),
    adivinar_numero(NuevoLimiteInferior, NuevoLimiteSuperior).

actualiza_limites(mayor, Numero, _, LimiteSuperior, Numero, LimiteSuperior).
actualiza_limites(menor, Numero, LimiteInferior, _, LimiteInferior, Numero).


% Ejercicio 2
revertir_lista([], []).
revertir_lista([H|T], Revertida) :-
    revertir_lista(T, ColaRevertida),
    append(ColaRevertida, [H], Revertida).

es_palindromo(Lista) :-
    revertir_lista(Lista, Revertida),
    Lista = Revertida.

es_capicua :-
    write('Ingresa un numero: '),
    read(NumeroUsuario),
    number_chars(NumeroUsuario, ListaCaracteres),
    es_palindromo(ListaCaracteres),
    write('El numero '), write(NumeroUsuario), write(' es capicua.').
es_capicua :-
    write('El numero no es capicua.').


%Ejercicio 3
% Ejemplos de pacientes (puedes añadir más)
paciente(juan, 20, 50, 172).
paciente(pedro, 19, 70, 168).
paciente(ana, 19, 54, 162).
paciente(maria, 20, 60, 170).
paciente(luis, 20, 70, 168).
paciente(gerardo, 19, 80, 170).
paciente(paco, 20, 90, 180).
paciente(pancho, 20, 100, 190).
paciente(panchito, 20, 50, 100).
paciente(panchote, 20, 120, 210).

% Agregar un nuevo paciente
agrega :- write('Proporciona nombre: '),
          read(Nombre),
          write('Proporciona edad: '),
          read(Edad),
          write('Proporciona peso: '),
          read(Peso),
          write('Proporciona estatura: '),
          read(Estatura),
          assertz(paciente(Nombre, Edad, Peso, Estatura)).

% Consultar todos los pacientes
buscartodos(L) :- findall(P, paciente(P, _, _, _), L).

% Imprimir nombres de pacientes
imprimirNombres([]).
imprimirNombres([P | R]) :- write(P), nl, imprimirNombres(R).

% Buscar pacientes por edad
buscarPorEdad(L, E) :- findall(P, (paciente(P, Ed, _, _), Ed = E), L).

% Buscar pacientes por nombre
buscarPorNombre(N, L) :- findall(P, (paciente(P, _, _, _), P = N), L).

% Buscar pacientes por peso
buscarPorPeso(L, P) :- findall(N, (paciente(N, _, Pes, _), Pes = P), L).

% Buscar pacientes por estatura
buscarPorEstatura(L, E) :- findall(N, (paciente(N, _, _, Est), Est = E), L).

% Eliminar paciente por nombre y edad
eliminar(N, E) :- 
    retract(paciente(N, E, _, _)), 
    write("Paciente eliminado.").

% Menú principal
menu :- write("Menu principal"), nl,
        write("1.- Agregar paciente"), nl,
        write("2.- Imprimir todos los pacientes"), nl,
        write("3.- Buscar pacientes por edad"), nl,
        write("4.- Buscar pacientes por nombre"), nl,
        write("5.- Buscar pacientes por peso"), nl,
        write("6.- Buscar pacientes por estatura"), nl,
        write("7.- Eliminar paciente"), nl,
        write("8.- Salir"), nl,
        write("Proporciona opción [1..8]: ").

% Ejecutar el menú
run :- menu,
       read(Opc),
       opcion(Opc).

% Opciones del menú
opcion(1) :- 
    agrega, 
    run.
opcion(2) :- 
    buscartodos(T), 
    imprimirNombres(T), 
    run.
opcion(3) :- 
    write("Proporciona edad a buscar: "), 
    read(E), 
    buscarPorEdad(T, E), 
    imprimirNombres(T), 
    run.
opcion(4) :- 
    write("Proporciona nombre a buscar: "), 
    read(N), 
    buscarPorNombre(N, T), 
    imprimirNombres(T), 
    run.
opcion(5) :- 
    write("Proporciona peso a buscar: "), 
    read(P), 
    buscarPorPeso(T, P), 
    imprimirNombres(T), 
    run.
opcion(6) :- 
    write("Proporciona estatura a buscar: "), 
    read(Es), 
    buscarPorEstatura(T, Es), 
    imprimirNombres(T), 
    run.
opcion(7) :- 
    write("Proporciona nombre: "), 
    read(N), 
    write("Proporciona edad: "), 
    read(E), 
    eliminar(N, E), 
    run.
opcion(8) :- 
    write("El programa ha terminado.").
