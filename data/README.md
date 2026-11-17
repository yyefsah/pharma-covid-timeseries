# Données (non incluses)

Séries mensuelles de l'INSEE (Banque de données macroéconomiques, BDM), exportées au format CSV (séparateur `;`) et placées dans le dossier de travail de R :

| Fichier attendu | Contenu | Série(s) utilisée(s) |
|---|---|---|
| `valeurs_mensuelles.csv` | Indices de production industrielle, une colonne par mois (1990-01 → 2026-04) | idBank 010767835 (21.10 brute), 010767837 (21.10 CVS-CJO), 010767842 (21.20), 010767832 (21) |
| `caractéristiques.csv` | Métadonnées des séries de production | |
| `valeurs_mensuellesca.csv` | Indice de chiffre d'affaires, industrie pharmaceutique (A88, poste 21), CVS-CJO, base 2021, idBank 010774278 (1999-01 → 2026-03) | |

(Le fichier de métadonnées du chiffre d'affaires est `caractéristiquesca.csv`, non utilisé par les scripts.) Page de référence : https://www.insee.fr/fr/information/2860802. Les données INSEE sont publiques ; vérifier les conditions de réutilisation avant de les republier.
