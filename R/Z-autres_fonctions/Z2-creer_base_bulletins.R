# Fonction pour convertir les données de bureaux de vote en bulletins individuels
creer_base_bulletins <- function(data, tour = "T1") {
  
  if (tour == "T1") {
    # Liste des candidats du T1 (2022)
    candidats <- c("ARTHAUD", "ROUSSEL", "MACRON", "LASSALLE", "LEPEN", 
                   "ZEMMOUR", "MELENCHON", "HIDALGO", "JADOT", "PECRESSE", 
                   "POUTOU", "DUPONTAIGNAN")
    
    # Colonnes de votes à utiliser
    cols_votes <- paste0(candidats, "_T1")
    
  } else if (tour == "T2") {
    # Liste des candidats du T2 (2022)
    candidats <- c("MACRON", "LEPEN")
    
    # Colonnes de votes à utiliser
    cols_votes <- paste0(candidats, "_T2")
    
  } else {
    stop("Le paramètre 'tour' doit être 'T1' ou 'T2'")
  }
  
  # Créer la base de bulletins
  bulletins_list <- list()
  
  for (i in 1:nrow(data)) {
    bv <- data[i, ]
    id_bv <- bv$ID
    
    # Pour chaque candidat, créer autant de lignes que de votes
    for (j in seq_along(candidats)) {
      candidat <- candidats[j]
      col_vote <- cols_votes[j]
      nb_votes <- bv[[col_vote]]
      
      if (!is.na(nb_votes) && nb_votes > 0) {
        # Créer les bulletins pour ce candidat
        bulletins_candidat <- tibble(
          ID = id_bv,
          candidat_vote = candidat
        )
        
        # Répéter nb_votes fois
        bulletins_candidat <- bulletins_candidat[rep(1, nb_votes), ]
        
        bulletins_list[[length(bulletins_list) + 1]] <- bulletins_candidat
      }
    }
  }
  
  # Combiner tous les bulletins
  bulletins <- bind_rows(bulletins_list)
  
  # Mélanger aléatoirement les bulletins au sein de chaque bureau de vote
  bulletins <- bulletins %>%
    group_by(ID) %>%
    slice_sample(prop = 1) %>%  # Mélange aléatoire
    mutate(num_bulletin = row_number()) %>%
    ungroup() %>%
    mutate(id_bulletin = paste0(ID, "_", num_bulletin)) %>%
    select(id_bulletin, ID, candidat_vote)
  
  # Créer les variables indicatrices pour chaque candidat
  for (candidat in candidats) {
    bulletins[[candidat]] <- as.integer(bulletins$candidat_vote == candidat)
  }
  
  # Retirer la colonne temporaire
  bulletins <- bulletins %>%
    select(-candidat_vote)
  
  return(bulletins)
}