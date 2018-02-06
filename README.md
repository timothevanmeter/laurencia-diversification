# Description rapide des scripts R


### Codes pour le formatage de données

- Script écrit par Line. Permet de passer du format csv au format fasta: [code](csv_to_fasta.R)
- Extrait uniquement les données ayant un géoréférencement dans un data.frame: [code](georef.R)
- Permet la comparaison entre deux jeu de données, en particulier isoler les données uniques: [code](trouver_intrus.R)
- Regroupe plusieurs jeux de données en un seul: [code](merge_all.R)


### Codes spécifiques à un format particulier
Chacun des scripts suivants ont été écris pour un format particulier

- Format Genbank. Extrait les noms d'espèces et de genre: [code](format_genbank.R)
- Format Line. Extrait les noms d'espèces et de genre: [code](format_line.R)
- Format Saunders. Extrait les noms d'espèces et de genre: [code](format_saunders.R)


### Codes pour les données géographiques

- Script écrit par Elizabeth J. Sbrocco pour extraire d'un raster MARSPEC les données d'une zone géographique précise: [code](Sbrocco_script.R)
- Extrait les données climatiques pour chaque spécimen ayant une localisation (lat, long) à partir d'un raster MARSPEC. Le code recherche pour les données manquantes, la cellule la plus proche possédant une valeur: [code](extraction_points_marspec.R)
- Produit une carte (fond de carte nécessaire!) avec les localisations de spécimens (données lat, long) : [code](plotmap.R)


### Codes pour les séquences nucléiques

- Identifie les doublons dans un alignement de séquences nucléiques: [code](RmDuplicates.R)

### Aide

Je peux répondre aux questions concernant l'utilisation des scripts. Dans certains cas, préciser dans les descritpions, je n'en suis pas l'auteur et il vaut s'adresser directement à l'auteur.
- Pour me contacter utiliser l'adresse sur mon [profil github](https://github.com/timothevanmeter).
