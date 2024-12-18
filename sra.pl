% Define possible sizes for the outer matrix
tailles([20, 40, 60, 80, 100]).

% Define possible densities (percentage of constraints)
densites([0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]).

% Domaine de cardinalité 20
domaine([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]).

% Calculate the number of constraints based on density and size
nombre_contraintes(Taille, Densite, Contraintes) :-
    Contraintes is round((Densite * Taille * (Taille + 1)) / 2),
    writeln(['Number of constraints calculated:', Contraintes]).

% Generate a 20x20 identity matrix
generer_matrice_identite(N, Matrice) :-
    findall(Row, (between(1, N, Index), generer_ligne_identite(Index, N, Row)), Matrice).

generer_ligne_identite(Index, N, Row) :-
    findall(Value, (between(1, N, Col), (Col =:= Index -> Value = 1 ; Value = 0)), Row).

% Generate a 20x20 matrix of ones
generer_matrice_universelle(N, Matrice) :-
    findall(Row, (between(1, N, _), generer_ligne_universelle(N, Row)), Matrice).

generer_ligne_universelle(N, Row) :-
    findall(1, between(1, N, _), Row).

% Initialize an NxN outer matrix with 20x20 identity blocks on the diagonal and 20x20 universal blocks elsewhere
initialiser_matrice(N, Matrice) :-
    findall(Row, (between(1, N, RowIndex), generer_ligne_matrice(N, RowIndex, Row)), Matrice).

generer_ligne_matrice(N, RowIndex, Row) :-
    findall(Block,
        (between(1, N, ColIndex),
         (RowIndex =:= ColIndex -> generer_matrice_identite(20, Block)
         ; generer_matrice_universelle(20, Block))),
        Row).

% Add random constraints to upper triangle and diagonal blocks only
ajouter_contraintes(N, Densite, MatriceIn, MatriceOut) :-
    nombre_contraintes(N, Densite, Contraintes),
    findall((I, J), (between(1, N, I), between(I, N, J)), Positions), % Upper triangle and diagonal only
    random_permutation(Positions, RandomPositions),
    ajouter_contraintes_positions(RandomPositions, Contraintes, MatriceIn, MatriceOut).

% Handle the addition of constraints to the outer matrix
ajouter_contraintes_positions(_, 0, Matrice, Matrice).
ajouter_contraintes_positions([], _, Matrice, Matrice).
ajouter_contraintes_positions([(I, J) | Rest], Contraintes, MatriceIn, MatriceOut) :-
    Contraintes > 0,
    ( I =:= J -> % Diagonal position
        generer_matrice_contrainte_unary(20, Constraint) % Unary constraints for diagonal
    ; generer_matrice_contrainte(20, Constraint) % Regular constraints for off-diagonal
    ),
    placer_bloc(I, J, Constraint, MatriceIn, NouvelleMatrice),
    NewContraintes is Contraintes - 1,
    ajouter_contraintes_positions(Rest, NewContraintes, NouvelleMatrice, MatriceOut).

% Generate a random 20x20 constraint matrix for off-diagonal blocks
generer_matrice_contrainte(N, Matrice) :-
    findall(Row, (between(1, N, _), generer_ligne_contrainte(N, Row)), Matrice).

generer_ligne_contrainte(N, Row) :-
    findall(Value, (between(1, N, _), random_between(0, 1, Value)), Row).

% Generate a 20x20 unary constraint matrix for diagonal blocks
% Ensure non-diagonal elements are zero and only diagonal values are randomized
generer_matrice_contrainte_unary(N, Matrice) :-
    findall(Row, (between(1, N, Index), generer_ligne_contrainte_unary(Index, N, Row)), Matrice).

generer_ligne_contrainte_unary(Index, N, Row) :-
    findall(Value,
        (between(1, N, Col),
         (Col =:= Index -> random_between(0, 1, Value) % Randomize diagonal values
         ; Value = 0)), % Ensure off-diagonal elements remain 0
        Row).

% Place a block at position (I, J) in the outer matrix
placer_bloc(I, J, Block, MatriceIn, MatriceOut) :-
    nth1(I, MatriceIn, Row, RestRows),
    remplacer_element(J, Block, Row, NewRow),
    nth1(I, MatriceOut, NewRow, RestRows).

remplacer_element(1, Val, [_ | Rest], [Val | Rest]).
remplacer_element(N, Val, [X | Rest], [X | NewRest]) :-
    N > 1,
    N1 is N - 1,
    remplacer_element(N1, Val, Rest, NewRest).

% Generate a CSP with a given size and density
generer_csp(N, D, Matrice) :-
    initialiser_matrice(N, MatriceInitiale),
    writeln('Initialized matrix with identity and universal blocks.'),
    ajouter_contraintes(N, D, MatriceInitiale, Matrice),
    save_matrices_to_file('cst_test.pl', Matrice).

save_matrices_to_file(FileName, Matrices) :-
    open(FileName, write, Stream),
    write(Stream, 'csp_matrices('), % Write the term 'csp_matrices(' to the file
    writeq(Stream, Matrices),       % Write the matrices in a format readable by Prolog
    write(Stream, ').'),            % Close the term with ')'
    nl(Stream),                     % Newline
    close(Stream).                  % Close the file

% Helper to print the matrix (outer structure only for readability)
print_matrix([]).
print_matrix([Row | Rest]) :-
    writeln(Row),
    print_matrix(Rest).

% SRA implementation for solving the CSP
% Improved SRA implementation: propagate constraints by narrowing possible domain for each variable

sra_solve(Matrice, Sol) :-
    flatten(Matrice, MatriceFlat), % Flatten the matrix for easy access
    resolve(MatriceFlat, Sol).

resolve([], []).
resolve([Var | Rest], [Value | SolRest]) :-
    domaine(Domain), % Get the domain of values
    select(Value, Domain, _), % Select a value from the domain
    apply_constraints(Var, Value, Matrice), % Apply constraints on the matrix
    resolve(Rest, SolRest).

% Apply constraints on a specific variable
apply_constraints(_, _, []).

% Applying constraints systematically and narrowing the domains
apply_constraints(Var, Value, [Row | Rest]) :-
    (   memberchk(Var, Row) % Check if the variable is part of the row
    ->  % Apply constraint logic here (e.g., updating domains)
        true
    ;   apply_constraints(Var, Value, Rest)
    ).

% Helper to handle the constraints between rows and variables
narrow_domain(Var, Value, Row, UpdatedRow) :-
    % Logic to narrow down the possible values of the variable
    % based on the current assignment and constraints
    % Update the domain for the variable and propagate through the rows.
    UpdatedRow = Row. % Dummy operation (to be replaced with domain reduction logic).

% Générer et sauvegarder les matrices CSP
generer_et_sauvegarder_matrices :-
    generer_450_matrices(Matrices),
    save_matrices_to_file('csp_matrices.pl', Matrices).

% Générer un test de matrice CSP
generer_test_matrix :-
    generer_csp(20, 0.1, Matrice),              % Generate a CSP matrix with size 5 and density 0.3
    writeln('Generated test matrix:'),
    print_matrix(Matrice),                     % Print the matrix to the console
    save_matrices_to_file('test_matrix.pl', Matrice). % Save it to 'test_matrix.pl'

% Générer 10 CSP et les enregistrer dans un fichier
generer_10_csp :-
    findall(Matrice, (between(1, 10, _), generer_csp(20, 0.1, Matrice)), Matrices),
    save_10_csp_to_file('generer_10csp.pl', Matrices).

% Sauvegarder les 10 CSP dans un fichier sous forme de csp()
save_10_csp_to_file(FileName, Matrices) :-
    open(FileName, write, Stream),
    write(Stream, 'csp('), % Commencer le terme csp(
    writeq(Stream, Matrices), % Ecrire les matrices dans un format lisible par Prolog
    write(Stream, ').'),     % Fermer le terme avec )
    nl(Stream),              % Nouvelle ligne
    close(Stream).           % Fermer le fichier


% exemple d'usage : generer_csp(20, 0.1, Matrice), sra_solve(Matrice, Solution).
