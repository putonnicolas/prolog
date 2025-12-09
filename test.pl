:- begin_tests(puissance4_tests).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%       TESTS UNITAIRES    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(plateau_vide) :-
    plateau_initial(Board),
    Board == [[], [], [], [], [], [], []].



:- end_tests(puissance4_tests).
