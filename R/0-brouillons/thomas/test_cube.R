#Définition de la taille de la population
N <- 2500
n <- 100
#Définition des variables auxiliaires
x1 <- rnorm(N)
x2 <- rnorm(N)
x3 <- rnorm(N)
x4 <- rexp(N)
x5 <- rexp(N)

#Création de la matrice contenant les variables d'équilibrage
#En ligne : les individus,
#En colonne : les variables.
x <- cbind(x1, x2, x3, x4, x5)
is.matrix(x)


#Création des variables d'intérêt :
y1 <- rnorm(nrow(x))



beta <- c(1, 1, 1, 1, 1)

sigma2 <- 3
y2 <- x %*% beta + rnorm(nrow(x), sd = sigma2)

# Transformer x en data.frame avec des noms de colonnes
df2 <- data.frame(y2 = y2, x)
reg2 <- lm(data = df2, y2 ~ . - 1)
summary(reg2)


sigma3 <- 1
y3 <- x %*% beta + rnorm(nrow(x), sd = sigma3)

# Transformer x en data.frame avec des noms de colonnes
df3 <- data.frame(y2 = y3, x)
reg3 <- lm(data = df2, y3 ~ . - 1)
summary(reg3)



#estimation va calculer l'estimation associée à l'estimateur de
#HT en tirant l'échantillon selon un plan suivant la méthode du cube.
estimation <- function(n, N, bal_mat){
  #Probabilité d'inclusion d'ordre 1.
  pi <- rep(n/N, N)
  #Tire un échantillon selon la méthode cube
  ech <- samplecube(bal_mat, pi, method = 2)
  ind <- which(ech == 1)
  
  #Calcul des réalisations des estimateurs du total de 
  #HT. 
  res <- data.frame("t1" = sum(y1[ind]/pi[ind]),
                    "t2" = sum(y2[ind]/pi[ind]),
                    "t3" = sum(y3[ind]/pi[ind]))
  return(res)
}

#Taille fixe --> ajout de la variable pi dans les variables
#d'équilibrage.
bal_mat <- cbind(pi, 1, x)

corrplot::corrplot(cor(bal_mat_modifie[,-c(1,2)], use = "complete.obs"), method = "color",
                   type = "upper", tl.col = "black", tl.srt = 45, tl.cex = 0.65)



#Monte-Carlo
nb_sim <- 1000
res <- lapply(X = 1:nb_sim, FUN = function(i){estimation(n, N, bal_mat_modifie)})
res <- Reduce(f = rbind, x = res)


varMC <- apply(X = res, 2, var)
espMC <- apply(X = res, 2, mean)


total <- c(sum(y1), sum(y2), sum(y3))
(total - espMC)/total

abs(sqrt(varMC)/total)
