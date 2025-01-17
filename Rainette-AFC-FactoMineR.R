################################
# # Rainette ## développé par Julien Barnier
# https://cran.r-project.org/web/packages/rainette/vignettes/introduction_usage.html
# https://juba.r-universe.dev/builds
################################

###############################################################################
#               Script test - AFC à partir de Rainette                        #
#      A partir d'un corpus texte formaté aux exigences IRAMUTEQ              #
###############################################################################

# install.packages(c("scatterD3", "ggplot2", "rainette", "quanteda", "FactoMineR"))

library(quanteda)
library(rainette)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(plotly)
library(scatterD3)
library(htmlwidgets)

# Paramétrages et définition des répertoires d'export
base_dir <- "/Users/stephanemeurisse/Documents/Recherche/rainette"  # Le chemin du repertoire de w
export_dir <- file.path(base_dir, "exports")
dir.create(export_dir, showWarnings = FALSE, recursive = TRUE)

# Chargement du corpus
chemin_fichier <- file.path(base_dir, "psychiatrie-darmanin-clean.txt") # le nom de votre fichier texte
corpus <- import_corpus_iramuteq(chemin_fichier)
cat("Nombre de documents importés :", ndoc(corpus), "\n")

## Classification sur segments
corpus <- split_segments(corpus)

tok <- tokens(corpus, remove_punct = TRUE, remove_numbers = TRUE)
tok <- tokens_split(tok, "'") # on split l'apostrophe
tok <- tokens_remove(tok, stopwords("fr"))
tok <- tokens_tolower(tok)

dtm <- dfm(tok)
dtm <- dfm_trim(dtm, min_termfreq = 5)

# Vérification et suppression des lignes avec des sommes nulles
row_sums <- rowSums(as.matrix(dtm))
dtm <- dtm[row_sums > 0, ]
print(dtm)

res <- rainette(dtm, k = 8, min_segment_size = 15, min_split_members = 10)

# AFC avec FactoMineR et factoextra
df <- convert(dtm, to = "matrix")

# Calcul de l'AFC
afc <- CA(df, graph = FALSE)

# Espace des segments (documents) - statique
fviz_ca_row(afc, col.row = factor(res$group), addEllipses = TRUE, labelsize = 3)

# Espace des segments (documents) - interactif
tmp <- afc$row$coord  # Coordonnées des lignes (documents)
interactive_afc_row <- scatterD3(
  x = tmp[, 1], 
  y = tmp[, 2], 
  col_var = res$group,  # Grouper par les classes de 'res'
  ellipses = TRUE, 
  lab = rownames(tmp)
)

# Sauvegarde de l'AFC interactive des segments (documents) en HTML
saveWidget(interactive_afc_row, file.path(export_dir, "afc_row_interactive.html"))

# Espace des termes - statique
fviz_ca_col(afc, labelsize = 3)

# Espace des termes - interactif
tmp <- afc$col$coord  # Coordonnées des colonnes (termes)
interactive_afc_col <- scatterD3(
  x = tmp[, 1], 
  y = tmp[, 2],
  lab = rownames(tmp), # Afficher les noms des termes
  colors = TRUE
)

# Sauvegarde de l'AFC interactive des termes en HTML
saveWidget(interactive_afc_col, file.path(export_dir, "afc_col_interactive.html"))

# Sauvegarde des AFC en PNG (statique)
output_afc_row <- file.path(export_dir, "afc_row.png")
ggsave(output_afc_row, plot = fviz_ca_row(afc, col.row = factor(res$group), addEllipses = TRUE, labelsize = 3))

output_afc_col <- file.path(export_dir, "afc_col.png")
ggsave(output_afc_col, plot = fviz_ca_col(afc, labelsize = 3))

cat("AFC sauvegardée dans :\n")
cat("Segments (documents) : ", output_afc_row, "\n")
cat("Termes : ", output_afc_col, "\n")
cat("AFC interactive sauvegardée dans :\n")
cat("Segments (documents) : /exports/afc_row_interactive.html\n")
cat("Termes : /exports/afc_col_interactive.html\n")
