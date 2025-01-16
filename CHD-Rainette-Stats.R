################################
# # Rainette ## développé par Julien Barnier
# https://cran.r-project.org/web/packages/rainette/vignettes/introduction_usage.html
# https://juba.r-universe.dev/builds
################################

# install.packages(c("rainette", "quanteda", "wordcloud", "RColorBrewer", "igraph", "dplyr", "shiny"))

###############################################################################
#               Script CHD + Extraction STATS et Affichage CHD                #
#      A partir d'un corpus texte formaté aux exigences IRAMUTEQ              #
#                                                                             #
#      1.Réalise la CHD sur le corpus, sans rainette_explor                   #
#      2.Extrait chi2, lr, freq, docprop dans un CSV                          #
#      3.Génère nuages de mots et graphes de cooccurrences par classe         #
#      4.Exporte les segments de texte par classe                             #
#      5.Affichage de la CHD                                                  #
###############################################################################

# 1) Chargement des bibliothèques
library(rainette)         # CHD + rainette_stats
library(quanteda)         # Construction dfm, tokens,...
library(wordcloud)        # Génération nuages de mots
library(RColorBrewer)     # Palette de couleurs
library(igraph)           # Graphes de cooccurrences
library(dplyr)            # Pour bind_rows() si besoin
library(shiny)


#########################################################
# 2) Paramétrages et définition des répertoires d'export
#########################################################
base_dir <- "/Users/stephanemeurisse/Documents/Recherche/ia-1an/rainette"  # Le chemin du repertoire de w
export_dir <- file.path(base_dir, "exports")
dir.create(export_dir, showWarnings = FALSE, recursive = TRUE)


#########################################################
# 3) Chargement du corpus
#########################################################
chemin_fichier <- file.path(base_dir, "psychiatrie-darmanin-clean.txt") # le nom de votre fichier texte
corpus <- import_corpus_iramuteq(chemin_fichier)
cat("Nombre de documents importés :", ndoc(corpus), "\n")


#########################################################
# 4) Découpage du corpus en segments
#########################################################
corpus <- split_segments(corpus, segment_size = 40) # definir le nombre de caractère par segment de texte
cat("Nombre de segments après découpage :", ndoc(corpus), "\n")


#########################################################
# 5) Prétraitement du texte (quanteda)
#########################################################
tok <- tokens(corpus, remove_punct = TRUE, remove_numbers = TRUE)
tok <- tokens_split(tok, "'") # on split l'apostrophe
tok <- tokens_remove(tok, stopwords("fr"))
tok <- tokens_tolower(tok)

dfm <- dfm(toks)
dfm <- dfm_trim(dfm, min_docfreq = 3) # à définir
cat("Dimensions du DFM après trim :", dim(dfm), "\n")


#########################################################
# 6) CHD avec rainette (sans affichage)
#########################################################
res <- rainette(dfm, k = 8, min_segment_size = 10, min_split_members = 10) # à définir

# Affecter les classes au corpus
docvars(corpus)$Classes <- res$group

cat("DEBUG 1 : Table de répartition des classes :\n")
print(table(docvars(corpus)$Classes))


#########################################################
# 7) Extraction des stats par classe (rainette_stats)
#########################################################
res_stats_list <- rainette_stats(
  dtm          = dfm,
  groups       = docvars(corpus)$Classes,
  measure      = c("chi2", "lr", "frequency", "docprop"),
  n_terms      = 9999,        # pas de limite
  show_negative = TRUE,
  max_p = 0.05                # filtrage sur la p-value
)

cat("\nDEBUG 2 : Structure de 'res_stats_list'\n")
str(res_stats_list)


#########################################################
# 8) Harmonisation des colonnes avant fusion (si nécessaire)
#########################################################
target_cols <- c("feature","chi2","p","n_target","n_reference",
                 "sign","frequency","docprop","lr")

for (i in seq_along(res_stats_list)) {
  if (!is.null(res_stats_list[[i]])) {
    current_names <- names(res_stats_list[[i]])
    for (col in target_cols) {
      if (! (col %in% current_names)) {
        cat("DEBUG 4 : Ajout de la colonne", col, "en NA pour l'élément", i, "\n")
        res_stats_list[[i]][[col]] <- NA_real_
      }
    }
  }
}


#########################################################
# 9) Fusion en un data frame unique via dplyr::bind_rows
#########################################################
cat("\nDEBUG 5 : Fusion en un data frame unique (bind_rows) .id = 'ClusterID'\n")
res_stats_df <- bind_rows(res_stats_list, .id = "ClusterID")

cat("DEBUG 6 : Dimensions de 'res_stats_df' :", dim(res_stats_df), "\n")
cat("DEBUG 7 : Noms colonnes :", names(res_stats_df), "\n")


#########################################################
# 10) Renommer `feature` -> `Terme`
#########################################################
if ("feature" %in% names(res_stats_df)) {
  names(res_stats_df)[names(res_stats_df) == "feature"] <- "Terme"
}


#########################################################
# 11) Forcer l'utilisation de `ClusterID` comme "Classe"
# (renomme ClusterID en Classe => col identifiant la classe)
#########################################################
names(res_stats_df)[names(res_stats_df) == "ClusterID"] <- "Classe"
# Convertir en numérique pour que la boucle puisse faire (Classe == cl)
# Actuellement, docvars(corpus)$Classes = 1..8
res_stats_df$Classe <- as.numeric(res_stats_df$Classe)



#########################################################
# 12) Tri par (Classe, -chi2) ou seulement -chi2 si ça vous arrange
#########################################################
if ("chi2" %in% names(res_stats_df)) {
  res_stats_df <- res_stats_df[order(res_stats_df$Classe, -res_stats_df$chi2), ]
}


#########################################################
# 13) Ordonner les colonnes finales
#########################################################
col_order <- c("Classe","Terme","chi2","p","n_target","n_reference",
               "sign","frequency","docprop","lr")
col_order <- intersect(col_order, names(res_stats_df))
res_stats_df <- res_stats_df[, col_order, drop = FALSE]

cat("DEBUG : Aperçu res_stats_df :\n")
print(head(res_stats_df, 15))


#########################################################
# 14) Finaliser le data frame => `res_stats`
#########################################################
res_stats <- res_stats_df  # on adopte un seul nom pour la suite


#########################################################
# 14) p-value -> critères en 3 colonnes
#########################################################
res_stats$p_value_1pct <- ifelse(res_stats$p < 0.01, "<0.01", "≥0.01")
res_stats$p_value_5pct <- ifelse(res_stats$p < 0.05, "<0.05", "≥0.05")


#########################################################
# 15) Export CSV des stats
#########################################################
chi2_file <- file.path(export_dir, "chi2_dataframe.csv")
write.csv(res_stats, chi2_file, row.names = FALSE)
cat("Fichier CSV des stats exporté dans :", chi2_file, "\n")


#########################################################
# 16) Génération des nuages de mots par classe (basés sur docvars)
#########################################################
wordcloud_dir <- file.path(export_dir, "wordclouds")
dir.create(wordcloud_dir, showWarnings = FALSE, recursive = TRUE)

# Récupération de la liste des classes (0..7 ou 1..8, selon rainette)
clusters <- sort(unique(docvars(corpus)$Classes))

for (cl in clusters) {
  # Sous-table pour la classe cl => on compare docvars(corpus)$Classes == cl
  subset_stats <- subset(res_stats, Classe == cl)
  
  # Tri par chi2 décroissant
  subset_stats <- subset_stats[order(-subset_stats$chi2), ]
  
  # Limite top 50
  top_n <- 20
  if (nrow(subset_stats) > top_n) {
    subset_stats <- subset_stats[1:top_n, ]
  }
  
  if (nrow(subset_stats) == 0) {
    cat("Aucun mot valable pour la classe", cl, "\n")
    next
  }
  
  png_filename <- file.path(wordcloud_dir, paste0("cluster_", cl, "_wordcloud.png"))
  png(png_filename, width = 800, height = 600)
  
  wordcloud(
    words = subset_stats$Terme,
    freq  = subset_stats$chi2,
    scale = c(10, 0.5),
    max.words = top_n,
    colors    = brewer.pal(8, "Dark2")
  )
  dev.off()
  
  cat("Nuage de mots pour la classe", cl, "enregistré dans :", png_filename, "\n")
}


#########################################################
# 17) Graphes de cooccurrences (top 20 termes) par classe
#########################################################
cooc_dir <- file.path(export_dir, "cooccurrences")
dir.create(cooc_dir, showWarnings = FALSE, recursive = TRUE)

for (cl in clusters) {
  tokens_cluster <- tokens_subset(toks, docvars(corpus)$Classes == cl)
  if (length(tokens_cluster) == 0) {
    cat("Classe", cl, ": aucun token.\n")
    next
  }
  
  fcm_cluster <- fcm(tokens_cluster, context = "window", window = 5, tri = FALSE)
  term_freq   <- sort(colSums(fcm_cluster), decreasing = TRUE)
  
  top_feat <- 20
  feat_cluster <- names(term_freq)[seq_len(min(top_feat, length(term_freq)))]
  fcm_cluster_sel <- fcm_select(fcm_cluster, feat_cluster, selection = "keep")
  
  adj <- as.matrix(fcm_cluster_sel)
  g   <- graph_from_adjacency_matrix(adj, mode = "undirected", weighted = TRUE, diag = FALSE)
  
  png_filename <- file.path(cooc_dir, paste0("cluster_", cl, "_fcm_network.png"))
  png(png_filename, width = 1600, height = 1200)
  
  plot(
    g,
    layout         = layout_with_fr(g),
    main           = paste("Cooccurrences - Classe", cl),
    vertex.size    = 12,
    vertex.label   = V(g)$name,
    vertex.label.cex = 1,
    edge.width     = E(g)$weight / 2,
    edge.color     = "gray80"
  )
  dev.off()
  
  cat("Graph de cooccurrences pour la classe", cl, ":", png_filename, "\n")
}


#########################################################
# 18) Export des segments de texte par classe
#########################################################
segments_by_class <- split(as.character(corpus), docvars(corpus)$Classes)
segments_file <- file.path(export_dir, "segments_par_classe.txt")

writeLines(
  unlist(lapply(
    names(segments_by_class),
    function(cl) {
      c(
        paste0("Classe ", cl, ":"),
        segments_by_class[[cl]],
        ""
      )
    }
  )),
  segments_file
)
cat("Segments par classe exportés dans :", segments_file, "\n")


#########################################################
# 19) Maintenant qu'on a les données... On affiche la CHD
#########################################################
rainette_explor(res, dfm, corpus)


