:- use_module(library(random)).
:- use_module(library(http/json)).
:- use_module(library(csv)).

% Permettre des clauses discontinues pour csp_to_json/2
:- discontiguous csp_to_json/2.

% Convertir une structure CSP en un dictionnaire JSON compatible
csp_to_json(csp(Variables, _, Matrix), JSON) :-
    JSON = _{variables: Variables, matrix: Matrix}.

% Générer une liste de variables de 1 à N
generate_variables(N, Variables) :-
    findall(Var, between(1, N, Var), Variables).

% Générer un domaine de taille fixe de 1 à D
generate_domain(D) :-
    findall(Val, between(1, D, Val), _).

% Calculer le nombre de contraintes en fonction de N et Density
calculate_constraints(N, Density, M) :-
    M is Density * N * (N + 1) // 200.

% Générer une matrice initiale de contraintes (identité sur la diagonale, universelle ailleurs)
generate_initial_constraint_matrix(N, Matrix) :-
    numlist(1, N, Indices),
    maplist(generate_row(N), Indices, Matrix).

generate_row(N, RowIndex, Row) :-
    length(Row, N),
    numlist(1, N, ColIndices),
    maplist(universal_or_identity(RowIndex), Row, ColIndices).

universal_or_identity(Index, 1, Index).  % Diagonale identité
universal_or_identity(_, 1, _).          % Universel ailleurs

% Insérer des contraintes aléatoires dans la matrice
insert_random_constraints(Matrix, M, RandomMatrix) :-
    length(Matrix, Size),
    findall((R, C), (between(1, Size, R), between(1, Size, C), R < C), Pairs),
    random_permutation(Pairs, ShuffledPairs),
    length(Constraints, M),
    append(Constraints, _, ShuffledPairs),
    foldl(apply_constraint, Constraints, Matrix, RandomMatrix).

apply_constraint((R, C), Matrix, UpdatedMatrix) :-
    nth1(R, Matrix, Row),
    nth1(C, Row, _, TempRow),
    nth1(C, NewRow, 0, TempRow),
    replace(Matrix, R, NewRow, UpdatedMatrix).

replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 1,
    NI is I - 1,
    replace(T, NI, X, R).

% Générer un seul CSP
generate_single_csp(N, D, Density, CSP) :-
    generate_variables(N, Variables),
    generate_domain(D),
    calculate_constraints(N, Density, M),
    generate_initial_constraint_matrix(N, InitialMatrix),
    insert_random_constraints(InitialMatrix, M, FinalMatrix),
    CSP = csp(Variables, FinalMatrix).

% Générer plusieurs CSPs et les sauvegarder dans un fichier JSON
generate_multiple_csp(Size, Density, Filename) :-
    calculate_constraints(Size, Density, NumConstraints),
    generate_csp_list(10, Size, Density, CSPList),
    maplist(csp_to_json, CSPList, JSONCSPList),
    CSPData = _{size: Size, density: Density, constraints: NumConstraints, instances: JSONCSPList},
    open(Filename, write, Stream),
    json_write(Stream, CSPData, [width(128)]),
    close(Stream).

% Générer tous les CSPs et les sauvegarder
generate_all_csp_and_save(FilenameJSON, FilenameCSV) :-
    % Enregistrer l'heure de début
    get_time(StartTime),

    % Combinaisons de tailles et densités
    findall(Size-Density, (
        member(Size, [20, 40, 60, 80, 100]),
        member(Density, [10, 20, 30, 40, 50, 60, 70, 80, 90])
    ), Combinations),

    % Générer les données CSP
    findall(CSPData, (
        member(Size-Density, Combinations),
        calculate_constraints(Size, Density, NumConstraints),
        % Utilisation d'une boucle explicite pour générer exactement 10 CSPs
        generate_csp_list(10, Size, Density, CSPList),
        CSPData = _{size: Size, density: Density, constraints: NumConstraints, instances: CSPList}
    ), AllCSPData),

    % Sauvegarder en JSON
    open(FilenameJSON, write, JSONStream),
    json_write(JSONStream, json(AllCSPData), [width(128)]),
    close(JSONStream),

    % Enregistrer l'heure de fin
    get_time(EndTime),

    % Calculer et afficher le temps d'exécution
    ExecutionTime is EndTime - StartTime,
    format('Le temps d\'exécution est de ~2f secondes.~n', [ExecutionTime]),

    % Sauvegarder le temps d'exécution dans un fichier CSV
    open(FilenameCSV, append, CSVStream),
    % Ajouter un en-tête si le fichier est vide
    (   current_input(CSVStream),
        at_end_of_stream(CSVStream)
    ->  write(CSVStream, 'Size,Density,ExecutionTime\n')
    ;   true
    ),
    % Ajouter les données d'exécution dans le fichier CSV
    format(CSVStream, '~w,~w\n', ["All CSPs", ExecutionTime]),
    close(CSVStream).

% Générer exactement N CSPs
generate_csp_list(0, _, _, []) :- !.
generate_csp_list(N, Size, Density, [CSP | Rest]) :-
    generate_single_csp(Size, 20, Density, CSP),
    N1 is N - 1,
    generate_csp_list(N1, Size, Density, Rest).

% Exemple d'utilisation :
% generate_all_csp_and_save('csp_instances.json', 'execution_times.csv').
