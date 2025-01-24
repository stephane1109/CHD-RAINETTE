################################
# # Rainette ## développé par Julien Barnier
# https://cran.r-project.org/web/packages/rainette/vignettes/introduction_usage.html
# https://juba.r-universe.dev/builds
################################


# install.packages(c("rainette", "quanteda", "wordcloud", "RColorBrewer", "igraph", "dplyr", "shiny"))
# install.packages("htmltools")

###############################################################################
#               Script CHD + Extraction STATS et Affichage CHD                #
#      A partir d'un corpus texte formaté aux exigences IRAMUTEQ              #
#                                                                             #
#      1.Réalise la CHD sur le corpus, sans rainette_explor                   #
#      2.Extrait chi2, lr, freq, docprop dans un CSV                          #
#      3.Génère nuages de mots et graphes de cooccurrences par classe         #
#      4.Exporte les segments de texte par classe au format text              #
#      5.Creation d'un concordancier au format html                           #
#      6.Affichage de la CHD avec rainette_explor (navigateur)                #
###############################################################################


# Chargement des bibliothèques nécessaires
library(rainette)
library(quanteda)
library(wordcloud)
library(RColorBrewer)
library(igraph)
library(dplyr)
library(htmltools)

#########################################################
# 1) Paramétrages et définition des répertoires d'export
#########################################################
base_dir <- "/Users/stephanemeurisse/Documents/Recherche/ia-1an/rainette"
export_dir <- file.path(base_dir, "exports")
dir.create(export_dir, showWarnings = FALSE, recursive = TRUE)

#########################################################
# 2) Chargement et découpage du corpus
#########################################################
chemin_fichier <- file.path(base_dir, "psychiatrie-darmanin-clean.txt")
corpus <- import_corpus_iramuteq(chemin_fichier)
cat("Nombre de documents importés :", ndoc(corpus), "\n")

# Découpage du corpus en segments
segment_size <- 40
corpus <- split_segments(corpus, segment_size = segment_size)
cat("Nombre de segments après découpage :", ndoc(corpus), "\n")

#########################################################
# 3) Prétraitement du texte et création du DFM
#########################################################
tok <- tokens(corpus, remove_punct = TRUE, remove_numbers = TRUE)
tok <- tokens_split(tok, "'")
tok <- tokens_remove(tok, stopwords("fr"))
tok <- tokens_tolower(tok)

dfm <- dfm(tok)
dfm <- dfm_trim(dfm, min_docfreq = 5)
cat("Dimensions du DFM après trim :", dim(dfm), "\n")

# Filtrer le corpus pour correspondre au DFM
included_segments <- docnames(dfm)
filtered_corpus <- corpus[included_segments]

#########################################################
# 4) Classification hiérarchique descendante (CHD)
#########################################################
res <- rainette(dfm, k = 8, min_segment_size = 10, min_split_members = 10)
docvars(filtered_corpus)$Classes <- res$group  # Affecter les classes

# Vérifier les segments classifiés
cat("DEBUG : Table de répartition des classes :\n")
print(table(docvars(filtered_corpus)$Classes))

#########################################################
# 5) Extraction et export des segments par classe
#########################################################
# Préparer les segments par classe
segments_by_class <- split(as.character(filtered_corpus), docvars(filtered_corpus)$Classes)

# Exporter les segments dans un fichier texte
segments_file <- file.path(export_dir, "segments_par_classe.txt")
writeLines(
  unlist(lapply(
    names(segments_by_class),
    function(cl) {
      c(
        paste0("Classe ", cl, ":"),
        segments_by_class[[cl]],
        ""  # Ligne vide entre les classes
      )
    }
  )),
  segments_file
)
cat("Segments par classe exportés dans :", segments_file, "\n")

#########################################################
# 6) Génération des statistiques par classe avec p-value
#########################################################
res_stats_list <- rainette_stats(
  dtm          = dfm,
  groups       = docvars(filtered_corpus)$Classes,
  measure      = c("chi2", "lr", "frequency", "docprop"),
  n_terms      = 9999,        # Pas de limite
  show_negative = TRUE,
  max_p = 0.05                # Filtrage sur la p-value
)

print(res_stats_list)

# Export des statistiques dans un fichier CSV
res_stats_df <- bind_rows(res_stats_list, .id = "ClusterID")
res_stats_df <- res_stats_df %>%
  rename(Terme = feature, Classe = ClusterID) %>%
  mutate(
    Classe = as.numeric(Classe),
    p_value_filter = ifelse(p <= 0.05, "≤ 0.05", "> 0.05")  # Ajout de la colonne filtrage p-value
  ) %>%
  arrange(Classe, desc(chi2))

stats_file <- file.path(export_dir, "stats_par_classe.csv")
write.csv(res_stats_df, stats_file, row.names = FALSE)
cat("Statistiques par classe exportées dans :", stats_file, "\n")

#########################################################
# 7) Export HTML avec surlignage
#########################################################
html_file <- file.path(export_dir, "segments_par_classe.html")
highlight_terms <- subset(res_stats_df, p <= 0.05)$Terme  # Termes significatifs

# Fonction corrigée pour surligner les termes dans un texte HTML
highlight_text_html <- function(text, terms, start_tag, end_tag) {
  for (term in terms) {
    # Escaper les caractères spéciaux et insérer les balises HTML
    escaped_term <- gsub("([\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|])", "\\\\\\1", term)
    text <- gsub(
      paste0("\\b", escaped_term, "\\b"),  # Ajouter des limites pour correspondre aux mots complets
      paste0(start_tag, term, end_tag),
      text,
      ignore.case = TRUE
    )
  }
  return(text)
}

# Générer le fichier HTML
sink(html_file)
cat("<html><head><style>body { font-family: Arial, sans-serif; } span.highlight { background-color: yellow; }</style></head><body>\n")
cat("<h1>Segments par Classe (avec surlignage)</h1>\n")

for (cl in names(segments_by_class)) {
  cat(paste0("<h2>Classe ", cl, "</h2>\n"))
  for (segment in segments_by_class[[cl]]) {
    highlighted_segment <- highlight_text_html(segment, highlight_terms, "<span class='highlight'>", "</span>")
    cat(paste0("<p>", highlighted_segment, "</p>\n"))
  }
}
cat("</body></html>\n")
sink()
cat("Fichier HTML avec surlignage exporté dans :", html_file, "\n")


#########################################################
# 9) Visualisations (nuages de mots et cooccurrences)
#########################################################

### Nuages de mots ###
wordcloud_dir <- file.path(export_dir, "wordclouds")
dir.create(wordcloud_dir, showWarnings = FALSE, recursive = TRUE)

clusters <- sort(unique(docvars(filtered_corpus)$Classes))
for (cl in clusters) {
  subset_stats <- subset(res_stats_df, Classe == cl & p <= 0.05)  # Filtrage p-value ≤ 0.05
  subset_stats <- subset_stats[order(-subset_stats$chi2), ]
  
  top_n <- 20
  if (nrow(subset_stats) > top_n) {
    subset_stats <- subset_stats[1:top_n, ]
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

### Graphes de cooccurrence ###
cooc_dir <- file.path(export_dir, "cooccurrences")
dir.create(cooc_dir, showWarnings = FALSE, recursive = TRUE)

for (cl in clusters) {
  # Filtrer les tokens pour la classe en cours
  tokens_cluster <- tokens_subset(tok, docvars(filtered_corpus)$Classes == cl)
  if (length(tokens_cluster) == 0) {
    cat("Classe", cl, ": aucun token.\n")
    next
  }
  
  # Calcul des cooccurrences avec une fenêtre glissante
  fcm_cluster <- fcm(tokens_cluster, context = "window", window = 5, tri = FALSE) # Fenêtre de 5 termes
  term_freq <- sort(colSums(fcm_cluster), decreasing = TRUE)
  
  # Sélection des termes les plus fréquents
  top_feat <- 20
  feat_cluster <- names(term_freq)[seq_len(min(top_feat, length(term_freq)))]
  fcm_cluster_sel <- fcm_select(fcm_cluster, feat_cluster, selection = "keep")
  
  # Conversion en matrice d'adjacence
  adj <- as.matrix(fcm_cluster_sel)
  g <- graph_from_adjacency_matrix(adj, mode = "undirected", weighted = TRUE, diag = FALSE)
  
  # Génération des couleurs pour les nœuds
  library(RColorBrewer)
  num_nodes <- length(V(g))  # Nombre total de nœuds dans le graphe
  palette_colors <- brewer.pal(min(8, num_nodes), "Set3")  # Utiliser la palette "Set3"
  V(g)$color <- palette_colors[seq_along(V(g))]  # Associer une couleur à chaque nœud
  
  # Sauvegarde du graphe en PNG
  png_filename <- file.path(cooc_dir, paste0("cluster_", cl, "_fcm_network.png"))
  png(png_filename, width = 1600, height = 1200)
  
  # Visualisation du graphe
  plot(
    g,
    layout         = layout_with_fr(g),  # Disposition Fruchterman-Reingold
    main           = paste("Cooccurrences - Classe", cl),
    vertex.size    = 16,                 # Taille des nœuds
    vertex.color   = V(g)$color,         # Couleur des nœuds
    vertex.label   = V(g)$name,          # Étiquettes des nœuds
    vertex.label.cex = 1,                # Taille des étiquettes
    edge.width     = E(g)$weight / 2,    # Épaisseur des liens proportionnelle au poids
    edge.color     = "gray80"            # Couleur des liens
  )
  dev.off()
  
  cat("Graph de cooccurrences pour la classe", cl, ":", png_filename, "\n")
}


#########################################################
# 10) Affichage de la classification avec Rainette
#########################################################
rainette_explor(res, dfm, filtered_corpus)
