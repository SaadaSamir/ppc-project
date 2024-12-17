:- use_module(library(random)).
:- use_module(library(csv)).

% Permettre des clauses discontinues pour csp_to_json/2
:- discontiguous csp_to_json/2.

% Convertir une structure CSP en une représentation compatible avec Prolog
csp_to_pl(csp(Variables, _, Matrix), PlRepresentation) :-
    PlRepresentation = csp(Variables, Matrix).

% Générer une liste de variables de 1 à N
generate_variables(N, Variables) :-
    findall(Var, between(1, N, Var), Variables).

% Générer un domaine de taille fixe de 1 à D
generate_domain(D) :-
    findall(Val, between(1, D, Val), _).

% Calculer le nombre de contraintes en fonction de N et Density
calculate_constraints(N, Density, M) :-
    M is Density * N * (N + 1) // 200.

% Afficher le nombre de contraintes pour chaque taille et densité
print_number_of_constraints(Size, Density) :-
    calculate_constraints(Size, Density, M),
    format('Pour la taille ~w et la densité ~w, le nombre de contraintes est ~w.~n', [Size, Density, M]).

% Afficher la matrice initiale Mp
print_initial_matrix(Size) :-
    generate_initial_constraint_matrix(Size, Matrix),
    format('La matrice initiale Mp pour la taille ~w est :~n', [Size]),
    print_matrix(Matrix).

% Afficher une matrice
print_matrix([]).
print_matrix([Row|Rest]) :-
    writeln(Row),
    print_matrix(Rest).

% Générer une matrice booléenne initiale de contraintes (NxN)
generate_initial_constraint_matrix(Size, Matrix) :-
    length(Matrix, Size),
    maplist(generate_row(Size), Matrix).

% Générer une ligne avec des valeurs universelles
generate_row(Size, Row) :-
    length(Row, Size),
    generate_row_elements(Size, Row, 1).

% Fonction qui génère les éléments de la ligne, avec 1 sur la diagonale et des valeurs universelles (1) ailleurs
generate_row_elements(0, [], _). % Cas de base : une liste vide
generate_row_elements(Index, [1|Rest], Index) :-  % Si c'est la diagonale, mettre 1
    NextIndex is Index - 1,
    generate_row_elements(NextIndex, Rest, NextIndex).
generate_row_elements(Index, [UniversalValue|Rest], NonZeroIndex) :- % Sinon, mettre UniversalValue (ici 1)
    Index \= NonZeroIndex,
    UniversalValue = 1,  % Valeur universelle choisie
    NextIndex is Index - 1,
    generate_row_elements(NextIndex, Rest, NonZeroIndex).

% Générer une liste de positions libres dans la partie supérieure (R < C)
generate_free_positions(Size, Pairs) :-
    findall((R, C), (between(1, Size, R), between(R+1, Size, C)), Pairs).

% Insérer des contraintes aléatoires dans la matrice avec gestion des positions libres
insert_random_constraints(Matrix, M, RandomMatrix) :-
    length(Matrix, Size),
    findall((R, C), (between(1, Size, R), between(R, Size, C), R < C), Pairs),
    random_permutation(Pairs, ShuffledPairs),
    length(Constraints, M),
    append(Constraints, _, ShuffledPairs),
    foldl(apply_constraint, Constraints, Matrix, RandomMatrix).
    

% Appliquer une contrainte (mettre un 0 dans la matrice à la position (R, C))
apply_constraint((R, C), Matrix, UpdatedMatrix) :-
    nth1(R, Matrix, Row),
    replace_element(Row, C, 0, UpdatedRow),
    replace(Matrix, R, UpdatedRow, TempMatrix),
    UpdatedMatrix = TempMatrix.

% Remplacer un élément spécifique dans une liste
replace_element([_|T], 1, X, [X|T]).
replace_element([H|T], I, X, [H|R]) :-
    I > 1,
    NI is I - 1,
    replace_element(T, NI, X, R).

% Remplacer une ligne spécifique dans une matrice
replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 1,
    NI is I - 1,
    replace(T, NI, X, R).

% Vérifier qu'il n'y a pas de contraintes en double
check_no_duplicates(Constraints) :-
    sort(Constraints, SortedConstraints),
    length(Constraints, Length),
    length(SortedConstraints, Length).

% Générer un seul CSP
generate_single_csp(N, D, Density, CSP) :-
    format('Génération du CSP pour N = ~w, D = ~w, Density = ~w~n', [N, D, Density]),
    generate_variables(N, Variables),
    generate_domain(D),
    calculate_constraints(N, Density, M),
    format('Nombre de contraintes calculées : ~w~n', [M]),
    generate_initial_constraint_matrix(N, InitialMatrix),
    insert_random_constraints(InitialMatrix, M, FinalMatrix),
    CSP = csp(Variables, FinalMatrix).


% Générer plusieurs CSPs pour une combinaison donnée de taille et densité
generate_csp_list(0, _, _, []) :- !.
generate_csp_list(N, Size, Density, [CSP | Rest]) :-
    generate_single_csp(Size, 20, Density, CSP),
    N1 is N - 1,
    generate_csp_list(N1, Size, Density, Rest).

% Afficher la matrice Mb après l'insertion des contraintes aléatoires
print_matrice_mb(Size, Density) :-
    generate_single_csp(Size, 20, Density, csp(_, Matrix)),
    format('Matrice Mb générée pour la taille ~w et la densité ~w :~n', [Size, Density]),
    print_matrix(Matrix).

% Afficher toutes les matrices Mb générées pour une combinaison donnée de taille et densité
print_matrices_for_combination(Size, Density) :-
    generate_csp_list(10, Size, Density, CSPList),
    format('Matrices Mb générées pour la taille ~w et la densité ~w :~n', [Size, Density]),
    forall(
        member(csp(_, Matrix), CSPList),
        print_matrix(Matrix)
    ).

% Afficher un seul CSP, avec ses variables et la matrice de contraintes
print_csp(csp(Variables, Matrix)) :-
    format('CSP avec Variables: ~w~n', [Variables]),
    format('Matrice de contraintes:~n'),
    print_matrix(Matrix).

% Afficher les 10 CSPs générés pour une combinaison donnée de taille et densité
print_csp_list_for_combination(Size, Density) :-
    generate_csp_list(10, Size, Density, CSPList),
    format('Affichage des 10 CSPs générés pour la taille ~w et la densité ~w :~n', [Size, Density]),
    forall(
        member(CSP, CSPList),
        print_csp(CSP)
    ).

% Générer et sauvegarder tous les CSPs dans un fichier .pl
generate_all_csp_and_save(FilenamePL) :-
    % Combinaisons de tailles et densités
    findall(Size-Density, (
        member(Size, [20, 40, 60, 80, 100]),
        member(Density, [10, 20, 30, 40, 50, 60, 70, 80, 90])
    ), Combinations),

    % Ouvrir le fichier .pl en écriture
    open(FilenamePL, write, Stream),

    % Générer et sauvegarder les CSPs pour chaque combinaison de taille et densité
    forall(
        member(Size-Density, Combinations),
        (
            % Générer 10 CSPs pour cette taille et densité
            generate_csp_list(10, Size, Density, CSPList),

            % Sauvegarder chaque CSP dans le fichier .pl
            forall(
                member(CSP, CSPList),
                (
                    csp_to_pl(CSP, PlRepresentation),
                    write(Stream, PlRepresentation),
                    write(Stream, '.\n')
                )
            )
        )
    ),

    % Fermer le fichier
    close(Stream).

% Boucle itérative pour insérer des matrices
insert_matrices_loop(Mp, M, Compteur, n, MFinal) :-
    insert_matrices_loop_iter(Mp, M, Compteur, n, MFinal).

% Version itérative de l'insertion des matrices
insert_matrices_loop_iter(Mp, 0, _, _, Mp) :- !.  % Terminer quand M = 0
insert_matrices_loop_iter(Mp, M, Compteur, n, MFinal) :-
    % Générer une matrice booléenne aléatoire Mb de taille 20x20
    generate_random_matrix(Mb),
    % Générer une position aléatoire p dans l'ensemble {1, ..., Compteur}
    random_between(1, Compteur, P),
    % Placer la matrice Mb dans la moitié supérieure de Mp à la position p
    place_matrix_in_mp(Mp, Mb, P, MpUpdated),
    % Décrémenter le compteur et M
    M1 is M - 1,
    Compteur1 is Compteur - 1,
    % Appel de la fonction avec les nouvelles valeurs, boucle itérative
    insert_matrices_loop_iter(MpUpdated, M1, Compteur1, n, MFinal).


% Générer une matrice booléenne aléatoire 20x20
generate_random_matrix(Mb) :-
    findall(Row, (length(Row, 20), maplist(random_bool, Row)), Mb).

% Générer une valeur booléenne aléatoire (0 ou 1)
random_bool(0) :- random_member(0, [0, 1]).
random_bool(1) :- random_member(1, [0, 1]).

% Placer la matrice Mb dans la position P de la matrice Mp
place_matrix_in_mp(Mp, Mb, P, MFinal) :-
    nth1(P, Mp, Row),
    replace_element(Row, P, Mb, MFinal).

% Exemple d utilisation :
% generate_all_csp_and_save('csp_instances.pl').
% print_number_of_constraints(40, 20).
% print_initial_matrix(20).
% Afficher une seule matrice Mb pour la taille 40 et la densite 20
% print_matrice_mb(40, 20).

% Afficher toutes les matrices Mb generees pour la taille 40 et la densite 20
% print_matrices_for_combination(40, 20).

% Exemple d utilisation pour afficher les CSPs de taille 40 et densite 20 : print_csp_list_for_combination(40, 20).
