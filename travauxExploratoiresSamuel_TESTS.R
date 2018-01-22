##################################################
##### PROJET STATISTIQUE BAYESIENNE
##### Modelisation bayesienne non-parametrique du langage
##### --- TESTS
##### (03/01/2017)
##################################################

rm(list = ls())
wd <-
  "C:/Users/Samuel/Documents/ENSAE - Stat Bayes/statistiqueBayesienne"
setwd(wd)
getwd()

source("travauxExploratoiresSamuel.R")

##################################################
##### TESTS DES FONCTIONS

### Generation d'un processus de Pitman-Yor sur [0,1]
### avec prior uniforme

n <- 10000
d <- 0
theta <- 10

ech <- genPitmanYor(n, d, theta, genPriorUniform)
print(length(ech))

### Influence des parametres

dList <- c(0, .5, .9)
thetaList <- c(1, 10, 100)
echList <- list()
for (d in dList) {
  echList[[toString(d)]] <- list()
  for (theta in thetaList) {
    echList[[toString(d)]][[toString(theta)]] <-
      genPitmanYor(n, d, theta, genPriorUniform)
  }
}
for (d in dList) {
  for (theta in thetaList) {
    ech <- echList[[toString(d)]][[toString(theta)]]
    print(paste("d = ", d, " ; theta = ", theta, " ; ech size = ", length(ech), sep =
                  ""))
  }
}

##################################################
##### INFERENCE BAYESIENNE

### Test de la structure

initChineseRestaurant()
print(info_)
for (i in 1:10) {
  drawWord()
}
print(info_)

### Add et Remove









