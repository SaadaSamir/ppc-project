% Define possible sizes for the outer matrix
tailles([20, 40, 60, 80, 100]).

% Define possible densities (percentage of constraints)
densites([0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]).

% Domaine de cardinalité 20
domaine([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]).

% Helper predicate to calculate the average of a list of numbers
average(List, Avg) :-
    sumlist(List, Sum),
    length(List, Len),
    (Len > 0 -> Avg is Sum / Len; Avg is 0).

% Run forward checking 10 times and print results
run_fc_10_times(Size, Density) :-
    run_fc_10_times(Size, Density, 10, [], AvgTime),
    format('Average time for 10 CSPs: ~2f ms\n', [AvgTime]).

run_fc_10_times(_, _, 0, Times, AvgTime) :-
    average(Times, AvgTime).

run_fc_10_times(Size, Density, N, Times, AvgTime) :-
    N > 0,
    statistics(runtime, [Start|_]),
    ( run_fc(Size, Density) ->
        statistics(runtime, [End|_]),
        Time is End - Start,
        format('Size: ~d, Density: ~2f, Time: ~d ms\n', [Size, Density, Time])
    ;
        statistics(runtime, [End|_]),
        Time is End - Start,
        format('Size: ~d, Density: ~2f, Time: ~d ms (no solution found)\n', [Size, Density, Time])
    ),
    N1 is N - 1,
    run_fc_10_times(Size, Density, N1, [Time|Times], AvgTime).

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

% Initialize an NxN outer matrix with 20x20 identity blocks on the diagonal and 20x20 universal blocks elsewhere
initialiser_matrice(N, Matrice) :-
    findall(Row, (between(1, N, RowIndex), generer_ligne_matrice(N, RowIndex, Row)), Matrice).

generer_ligne_matrice(N, RowIndex, Row) :-
    findall(Block,
        (between(1, N, ColIndex),
         (RowIndex =:= ColIndex -> generer_matrice_identite(20, Block)
         ; generer_matrice_universelle(20, Block))),
        Row).

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

% Add random constraints to upper triangle and diagonal blocks only
ajouter_contraintes(N, Densite, MatriceIn, MatriceOut) :-
    nombre_contraintes(N, Densite, Contraintes),
    findall((I, J), (between(1, N, I), between(I, N, J)), Positions), % Upper triangle and diagonal only
    random_permutation(Positions, RandomPositions),
    ajouter_contraintes_positions(RandomPositions, Contraintes, MatriceIn, MatriceOut).

% Calculate the number of constraints based on density and size
nombre_contraintes(Taille, Densite, Contraintes) :-
    Contraintes is round((Densite * Taille * (Taille + 1)) / 2),
    writeln(['Number of constraints calculated:', Contraintes]).

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

% Forward checking algorithm to solve the CSP
solve_fc(CSP, Dom, A) :-
    % Check if all variables are assigned
    all_assigned(Dom),
    % Collect assignments
    dom_to_assignment(Dom, A),
    !.

solve_fc(CSP, Dom, A) :-
    % Not all assigned: choose a variable not assigned yet
    choose_unassigned_var(Dom, X),
    % Get its domain
    get_domain(X, Dom, DomainX),
    % Try each value in DomainX
    member(V, DomainX),
    unary_consistent(X, V, CSP),
    % Prepare new domains after choosing V for X
    assign_value(X, V, Dom, DomAssigned),
    forward_check_domains(X, V, DomAssigned, CSP, DomPruned, DomainEmpty),
    DomainEmpty == false,
    solve_fc(CSP, DomPruned, A).

% forward_check_domains/6
forward_check_domains(X, V, DomIn, CSP, DomOut, DomainEmpty) :-
    unassigned_vars(DomIn, Unassigned),
    prune_domains(Unassigned, X, V, DomIn, CSP, DomOut, DomainEmpty).

% prune_domains/8
prune_domains([], _X, _V, Dom, _CSP, Dom, false).
prune_domains([Y|Ys], X, V, DomIn, CSP, DomOut, DomainEmpty) :-
    Y \= X,
    get_domain(Y, DomIn, D),
    filter_domain(Y, D, X, V, CSP, DFiltered),
    ( DFiltered = [] ->
        % Domain empty, fail early
        DomainEmpty = true,
        DomOut = DomIn
    ;
        % Update domain of Y and continue
        update_domain(Y, DFiltered, DomIn, DomInt),
        prune_domains(Ys, X, V, DomInt, CSP, DomOut, DomainEmpty)
    ).

% filter_domain/6
filter_domain(_Y, [], _X, _V, _CSP, []).
filter_domain(Y, [Val|Vals], X, V, CSP, [Val|Filtered]) :-
    binary_consistent_pair(X, V, Y, Val, CSP),
    filter_domain(Y, Vals, X, V, CSP, Filtered).
filter_domain(Y, [Val|Vals], X, V, CSP, Filtered) :-
    \+ binary_consistent_pair(X, V, Y, Val, CSP),
    filter_domain(Y, Vals, X, V, CSP, Filtered).

% binary_consistent_pair(X, VX, Y, VY, CSP)
binary_consistent_pair(X,VX,Y,VY,CSP) :-
    (X < Y -> I=X, J=Y; I=Y, J=X),
    nth0(I, CSP, RowI),
    nth0(J, RowI, BinMatrix),
    VxIdx is VX - 1,
    VyIdx is VY - 1,
    nth0(VxIdx, BinMatrix, VxRow),
    nth0(VyIdx, VxRow, Allowed),
    Allowed =:= 1.

% unary_consistent(X, V, CSP)
unary_consistent(X, V, CSP) :-
    nth0(X, CSP, RowX),
    nth0(X, RowX, UnaryMatrix),
    VIdx is V - 1,
    nth0(VIdx, UnaryMatrix, ValRow),
    nth0(VIdx, ValRow, Allowed),
    Allowed =:= 1.

% Helper predicates

% all_assigned(Dom)
all_assigned([]).
all_assigned([(_Var, [ _Val ])|T]) :- all_assigned(T).

% dom_to_assignment(Dom, A)
dom_to_assignment([], []).
dom_to_assignment([(X,[V])|T], [(X,V)|A]) :-
    dom_to_assignment(T, A).

% choose_unassigned_var(Dom, X)
choose_unassigned_var(Dom, X) :-
    member((X,D), Dom),
    length(D, Len),
    Len > 1,
    !.

% get_domain(Var, Dom, Domain)
get_domain(Var, [(Var,D)|_], D) :- !.
get_domain(Var, [_|T], D) :- get_domain(Var, T, D).

% update_domain(Var, NewDomain, DomIn, DomOut)
update_domain(_Var, _NewD, [], []) :- fail.  % Should not happen if Var is always found
update_domain(Var, NewD, [(Var,_)|T], [(Var,NewD)|T]) :- !.
update_domain(Var, NewD, [H|T], [H|T2]) :-
    update_domain(Var, NewD, T, T2).

% assign_value(Var, Val, DomIn, DomOut)
assign_value(Var, Val, [], []) :- fail.
assign_value(Var, Val, [(Var,_)|T], [(Var,[Val])|T]) :- !.
assign_value(Var, Val, [H|T], [H|T2]) :-
    assign_value(Var, Val, T, T2).

% unassigned_vars(Dom, Unassigned)
unassigned_vars(Dom, Unassigned) :-
    findall(X, (member((X,D),Dom), length(D,L), L>1), Unassigned).

% Domain definition: [1..20]
initial_domain([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]).

% Example entry point:
% run_fc(Size, Density):
%  1. Generate CSP with generer_csp(Size,Density,Matrix).
%  2. Create initial domains for all variables: N = Size, Vars = 0..N-1
%  3. Call solve_fc(Matrix, Dom, Solution).

run_fc(Size, Density) :-
    generer_csp(Size, Density, Matrix),
    format('Generated CSP Matrix for Size ~d and Density ~f\n', [Size, Density]),
    length(Matrix, N),
    initial_domain(D),
    create_initial_domains(N, D, Dom),
    statistics(runtime, [Start|_]),
    ( solve_fc(Matrix, Dom, Solution) ->
        statistics(runtime, [End|_]),
        Time is End - Start,
        format('Solution found in ~d ms: ~w\n', [Time, Solution])
    ; 
        statistics(runtime, [End|_]),
        Time is End - Start,
        format('No solution found. Time taken: ~d ms\n', [Time])
    ).

% create_initial_domains(N, D, Dom)
create_initial_domains(N, D, Dom) :-
    N1 is N - 1,                 % Evaluate arithmetic before numlist/3
    numlist(0, N1, Vars),
    maplist({D}/[X,(X,D)]>>true, Vars, Dom).

% Example call to solve a CSP of size 20 with density 0.1
% run_fc_10_times(40, 0.3).
