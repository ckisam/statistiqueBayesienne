V <- 10000 
K <- 100000


PY_draw <- function(K, d, theta, V){ 
  
  # Tire K observations iid d'un processus PY(d,theta,G0), pour un vocabulaire de taille V et G0 uniforme.
  
  W <- c(1:V) # Vocabulaire.
  counts <- c(rep(c(0),V)) # Pour chaque valeur possible, combien de fois elle a été tirée auparavant. 
    #Mis à jour dans la boucle.
  unif <- runif(K) # Tirage de K v.a. suivant une loi uniforme sur [0,1]. Utilisée pour déterminer 
    # si, pour le tirage k, on tire selon la distribution a priori ou parmi les valeurs déjà tirées auparavant.
  G0_draws <- sample(W, K, replace=TRUE) # K tirages de la distribution a priori uniforme.
  y <- c(rep(c(NA),K)) # Vecteur pour stocker les K tirages du processus.
  
  # Tirage de la première observation.
  y[1] <- G0_draws[1] # La première observation est tirée selon l'a priori uniforme.
  counts[y[1]] <- counts[y[1]]+1 # Mise à jour du nombre d'observations de chaque valeur.
  c <- 1 # Nombre de tirages actuels de G.
  t <- 1 # Nombre de fois où l'on a recours à l'a priori uniforme. Mis à jour dans la boucle.

  for (i in 2:K){
    if (unif[i] <= (c-t*d)/(theta+c)){ # Si l'on tire parmi les valeurs déjà sorties.
      y[i] <- which(rmultinom(1, 1, counts)==1)
      counts[y[i]] <- counts[y[i]]+1
      c <- c+1
    } else { # Si l'on recourt à la distribution a priori.
      y[i] <- G0_draws[i]
      counts[y[i]] <- counts[y[i]]+1
      c <- c+1
      t <- t+1
    }
  }
  y
}

unique_words <- function(draw){ # Counts number of unique words over time
  count <- c(cumsum((1-duplicated(draw))))
}
par(mfrow=c(2,2))
y1 <-ts(unique_words(PY_draw(K, 0.5, 1, V)))
y2 <-ts(unique_words(PY_draw(K, 0.5, 10, V)))
y3 <-ts(unique_words(PY_draw(K, 0.5, 100, V)))
ts.plot(y1, y2, y3, log="xy")

y4 <-ts(unique_words(PY_draw(K, 0, 10, V)))
y5 <-ts(unique_words(PY_draw(K, 0.5, 10, V)))
y6 <-ts(unique_words(PY_draw(K, 0.9, 10, V)))
ts.plot(y4, y5, y6, log="xy")
