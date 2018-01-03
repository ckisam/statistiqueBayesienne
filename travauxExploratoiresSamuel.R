##################################################
##### PROJET STATISTIQUE BAYESIENNE
##### Modelisation bayesienne non-parametrique du langage
##### (03/01/2017)
##################################################

rm(list = ls())
wd <-
  "C:/Users/Samuel/Documents/ENSAE - Stat Bayes/statistiqueBayesienne"
setwd(wd)
getwd()

##################################################
##### GENERATION PROCESSUS PITMAN-YOR

### Prior simple : uniforme sur [0,1]
genPriorUniform <- function() {
  return(runif(1))
}

### Pour un vecteur compose de 0 et de un 1,
### retourne l'index du 1
getIndexSelected <- function(v) {
  for (i in 1:length(v)) {
    if (v[i] == 1) {
      return(i)
    }
  }
  return(NULL)
}

### Genere un echantillon suivant un processus de
### Pitman-Yor, selon le principe du restaurant chinois
genPitmanYor <- function(n, d, theta, genPrior) {
  require(stats)
  res <- c()
  occ <- c()
  for (i in 1:n) {
    drawPrior <- genPrior()
    t <- length(res)
    #nbG <- sum(occ)
    probaList <- c((occ - d), (theta + d * t))
    index <- getIndexSelected(as.vector(rmultinom(1, 1, probaList)))
    if (index <= t) {
      occ[index] <- occ[index] + 1
    } else{
      prec <- which(res == drawPrior)
      if (length(prec) > 0) {
        index <- prec[1]
        occ[index] <- occ[index] + 1
      } else{
        res <- append(res, drawPrior)
        occ <- append(occ, 1)
      }
    }
  }
  return(res)
}
