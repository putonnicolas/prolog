:- begin_tests(puissance4_tests).

:- use_module(utils).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%       TESTS UNITAIRES    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(plateau_vide) :-
    plateau_initial(Board),
    Board == [[], [], [], [], [], [], []].



:- end_tests(puissance4_tests).
