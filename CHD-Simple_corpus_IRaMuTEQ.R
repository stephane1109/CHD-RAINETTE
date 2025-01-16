################################
# # Rainette ## développé par Julien Barnier
# https://cran.r-project.org/web/packages/rainette/vignettes/introduction_usage.html
# https://juba.r-universe.dev/builds
################################

# 1. Installer et charger les librairies nécessaires
# install.packages(c("rainette", "quanteda", "factoextra"))

library(rainette)
library(quanteda)
library(factoextra)


# 2. Importer le corpus formaté pour Iramuteq
# Les variables étoilées seront supprimées
chemin_fichier <- "/Users/stephanemeurisse/Documents/Recherche/ia-1an/rainette/psychiatrie-darmanin-clean.txt"
corpus <- import_corpus_iramuteq(chemin_fichier)

# 3. Vérifier l'importation
print ('information sur le fichier importé')
print(ndoc(corpus)) # nombre de doc
print(corpus) # affiche un extrait
summary(corpus) # affiche les metadonnées (stats de l'article)

# 4. Définir la taille des segments de texte
# On definit des segments de texte dans le afin de diviser un même document considéré comme (trop) "long"
# Attention : le segment de texte est construit sur la base du texte brut avant la tokenisation/stopword/lemmatisation
corpus <- split_segments(corpus, segment_size = 40)

print('aperçu après splitting')
print(head(docvars(corpus)))
print(as.character(corpus))[1:2]


# 3. Prétraitement du texte -> on utilise la fonction token de Quanteda
tok <- tokens(corpus, remove_punct = TRUE, remove_numbers = TRUE)
tok <- tokens_remove(tok, stopwords("fr"))
tok <- tokens_tolower(tok) # convertir les tokens en minuscules
print('Apercu après tokenisation')
print(tok)

# 4. Créer une matrice termes/documents (dfm)
dfm <- dfm(tok)
print(dfm)

# Limiter le vocabulaire aux termes apparaissant dans au moins 5 segments
dfm <- dfm_trim(dfm, min_docfreq = 5)
print(dfm)

# 5. Effectuer la classification hiérarchique descendante (CHD)
res <- rainette(dfm, k = 8, min_segment_size = 10, min_split_members = 10)
 
# 6. Explorer les résultats
# On passe par la fonction "rainette_explor" une interface graphique
rainette_explor(res, dfm, corpus)


