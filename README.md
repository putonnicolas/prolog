# 📘 README — Utiliser Prolog avec VSCode & SWI-Prolog


## ▶️ 1. Exécuter du Prolog

### ✔ Ouvrir un terminal dans VSCode

```
Ctrl + Shift + `
```

Tapez :

```bash
swipl
```

Vous arrivez dans l’interpréteur Prolog :

```
?-
```

--- 

## 📂 2. Charger un fichier Prolog

Assurez-vous d’être dans le dossier contenant votre fichier (`test.pl` par exemple).

Dans SWI-Prolog :

```prolog
[test].
```

→ Le `.pl` est optionnel.
→ Si le fichier est dans un autre dossier :

```prolog
['C:/chemin/vers/votre_fichier.pl'].
```

Si tout va bien, Prolog répond :

```
true.
```

---

## 🔄 4. Recharger après modification

Chaque fois que vous sauvegardez le fichier et voulez le recharger :

```
make.
```

ou :

```
[All].
```

ou encore :

```
reload_file.
```

---

## 🛑 6. Quitter Prolog

Dans l’interpréteur :

```prolog
halt.
```

---

## 🧰 7. Résumé rapide (TL;DR)

**À faire pour exécuter n’importe quel code Prolog :**

```bash
cd dossier_du_projet
swipl
?- [nom_fichier].
?- requete.
```


# 🛠️ Installation des outils

### ✔ Installer SWI-Prolog

Télécharger ici :
[https://www.swi-prolog.org/download/stable](https://www.swi-prolog.org/download/stable)

**Pendant l’installation, cocher :**
➡️ *“Add swipl.exe to PATH”*

Pour vérifier l’installation, ouvrir un terminal et taper :

```bash
swipl
```

Vous devez voir :

```
Welcome to SWI-Prolog ...
?-
```

---

### ✔ Installer l’extension VSCode Prolog

Dans VSCode :

1. `Ctrl+Shift+P`
2. Tapez : `ext install prolog`
3. Ouvrez un fichier `.pl`
4. En bas à droite, sélectionner **Prolog** comme mode de langage
   (car `.pl` peut être pris pour du Perl)

⚠️ Cette extension gère la **coloration syntaxique**, pas l'exécution.

---
