# Manuel d'utilisation (Puissance 4 Prolog)

## 1) Lancer le jeu
```bash
cd <dossier_du_projet>

?- [load].      % charge tous les fichiers
?- init.        % démarre une partie
```

Recharger après modification de code :
```prolog
?- make.
```

Quitter Prolog :
```prolog
?- halt.
```

## 2) Structure du projet
- main.pl : boucle de jeu, lancement de la partie.
- utils.pl : gestion du plateau (affichage, coups, victoire, plateau plein), changement de joueur.
- ia_random.pl : IA aléatoire.
- ia_naive.pl : IA naïve (empiler jusqu'à se faire bloquer).
- ia_niveau1.pl : IA opportuniste (gagner ou bloquer si possible).
- ia_minimax.pl : IA Minimax avec heuristique et profondeur paramétrable.
- benchmark.pl : simulation automatique de parties entre IA et collecte de statistiques.
- test.pl : tests unitaires et tests d’intégration.
- load.pl : point d’entrée pour charger tous les fichiers du projet.

## 3) Fonctions clés
- `init/0` : initialise un plateau vide et lance `play/1` avec le joueur `x`.
- `play(Player)` : boucle principale. Affiche le plateau, lit le coup, met à jour le plateau, teste la victoire ou l’égalité, passe au joueur suivant.
- `choisir_coup/3` : saisie humaine pour `x`, IA pour `o`.
- `jouer_coup/4` : empile un pion dans une colonne si elle n’est pas pleine.
- `win/3` : teste la victoire (ligne, colonne, deux diagonales).

## 4) Comment jouer
1. Lancer SWI-Prolog dans le dossier du projet.
2. Charger avec `[load].` puis lancer `init.`
3. Joueur `x` est humain : entre un numéro de colonne 1-7 quand demandé.
4. Joueur `o` est l’IA : son coup est joué automatiquement.
5. Le jeu s’arrête sur une victoire ou si le plateau est plein.


## 5) Benchmark des IA
Le fichier benchmark.pl permet de faire s’affronter deux IA automatiquement sur un grand nombre de parties afin de comparer leurs performances.

Lancer un benchmark : 
```prolog
?- bench_games(random, naive, 0, 0, 1000, S), print_stats(S).
```

Paramètres :
```prolog
bench_games(AI_X, AI_O, DepthX, DepthO, N, Stats).
```
- AI_X, AI_O : type d’IA (random, naive, niveau1, minimax)
- DepthX, DepthO : profondeur (uniquement utilisée pour minimax)
- N : nombre de parties simulées
- Stats : structure contenant les résultats globaux

## 6) Tests
Le fichier test.pl contient des tests unitaires et d’intégration utilisant le framework plunit.

Lancer les tests: 
```prolog
?- run_tests.
```

Ces tests garantissent la correction logique du jeu ainsi que le bon comportement des IA.

## 7) Dépannage rapide
- Erreur de prédicat manquant : assurez-vous de charger via `[load].` pour inclure utils/IA.
- Rien ne se passe après modification : faites `make.` pour recharger.
- Problème de saisie : entrez un entier 1-7 et n'oubliez pas le point après l'entier.