%Ejercicio 1
%a)
calcula_promedio(Calificaciones, Promedio) :-
    sumlist(Calificaciones, Suma),
    length(Calificaciones, Longitud),
    Longitud > 0,
    Longitud < 9,
    Promedio is Suma / Longitud.

empleados_no_aprueban(_, [], _, []). 
empleados_no_aprueban(Tasa, [Empleado|Restantes], Indice, [Indice|NoApruebanRest]) :-
    calcula_promedio(Empleado, Promedio),
    Promedio < Tasa, 
    NuevoIndice is Indice + 1,
    empleados_no_aprueban(Tasa, Restantes, NuevoIndice, NoApruebanRest).
empleados_no_aprueban(Tasa, [_|Restantes], Indice, NoApruebanRest) :-
    NuevoIndice is Indice + 1,
    empleados_no_aprueban(Tasa, Restantes, NuevoIndice, NoApruebanRest).

lista_empleados_no_aprueban(Tasa, Calificaciones, NoAprueban) :-
    empleados_no_aprueban(Tasa, Calificaciones, 0, NoAprueban).
    
%b)
lista_empleados_no_aprueban_por_tasas([], _, []).
lista_empleados_no_aprueban_por_tasas([Tasa|RestTasas], Calificaciones, [NoAprueban|RestNoAprueban]) :-
    lista_empleados_no_aprueban(Tasa, Calificaciones, NoAprueban),
    lista_empleados_no_aprueban_por_tasas(RestTasas, Calificaciones, RestNoAprueban).

%Ejericio 2
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


% Ejercicio 3
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