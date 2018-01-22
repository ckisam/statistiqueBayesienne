######################################################################
##### PROJET STATISTIQUE BAYESIENNE
##### Modelisation bayesienne non-parametrique du langage
##### (03/01/2017)
######################################################################

rm(list = ls())
wd <-
  "C:/Users/Samuel/Documents/ENSAE - Stat Bayes/statistiqueBayesienne"
setwd(wd)
getwd()

######################################################################
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

######################################################################
##### GENERATION HIERARCHICAL CHINESE RESTAURANT PROCESS
##### ----- 2nde implementation, moins structuree

naiveVoc <-
  c(
    "lapin",
    "gateau",
    "bijou",
    "caillou",
    "vivre",
    "fermier",
    "prairie",
    "porte",
    "stylo",
    "vague",
    "morale",
    "coiffeur",
    "metro",
    "biere",
    "tennis",
    "uniforme"
  )

getIndex <- function(word, voc = naiveVoc) {
  return(match(word, voc))
}

getContextByKey <- function(key = "") {
  if (key == "empty" || nchar(key) == 0) {
    return(c())
  }
  return(do.call(c, strsplit(key, "_")))
}

getKeyByContext <- function(u = c()) {
  if (length(u) == 0) {
    return("empty")
  }
  return(paste(u, collapse = "_"))
}

initChineseRestaurant <-
  function(depth = 3,
           d = rep(.5, depth),
           theta = rep(1, depth),
           voc = naiveVoc) {
    info_ <<-
      list(d = d,
           theta = theta,
           context = list(empty = list(
             tableList = c(), custList = c()
           )))
  }

getProbaContext <- function(u = c()) {
  key <- getKeyByContext(u)
  d <- info_$d[1 + length(u)]
  theta <- info_$theta[1 + length(u)]
  custList <- info_$context[[key]]$custList
  constNorm <- theta + sum(custList)
  res <- list()
  res$existing <-
    (custList - d) / constNorm
  res$new <-  (theta + d * (length(custList))) / constNorm
  return(res)
}

drawWord <-
  function(u = c(),
           voc = naiveVoc) {
    key <- getKeyByContext(u)
    custList <- info_$context[[key]]$custList
    tableList <- info_$context[[key]]$tableList
    proba <- getProbaContext(u)
    if (rbinom(1, 1, proba$new) == 1) {
      # Nouveau tirage
      if (length(u) == 0) {
        word <- sample(1:length(voc), 1)
      } else{
        word <- drawWord(u[-1])
      }
      tableList <-
        append(tableList, word)
      custList <-
        append(custList, 1)
    } else{
      # Ancien tirage
      k <-
        sample(
          x = 1:length(tableList),
          size = 1,
          prob = proba$existing
        )
      word <- tableList[k]
      custList[k] <- custList[k] + 1
    }
    info_$context[[key]]$custList <<- custList
    info_$context[[key]]$tableList <<- tableList
    return(word)
  }

wordProbability <- function(u = c(), w, voc = naiveVoc) {
  key <- getKeyByContext(u)
  tableList <- info_$context[[key]]$tableList
  custList <- info_$context[[key]]$custList
  c <- sum(custList[tableList == w])
  t <- length(tableList[tableList == w])
  d <- info_$d[1 + length(u)]
  theta <- info_$theta[1 + length(u)]
  constNorm <- sum(custList) + theta
  res <- (c - d * t) / constNorm
  if (key == "empty") {
    previousPrev <- 1 / length(voc)
  } else{
    previousPrev <- wordProbability(u[-1], w, voc)
  }
  res <-
    res + ((theta + d * length(tableList)) / constNorm) * previousPrev
  return(res)
}

addCustomer <- function(u = c(), w, voc = naiveVoc) {
  key <- getKeyByContext(u)
  tableList <- info_$context[[key]]$tableList
  custList <- info_$context[[key]]$custList
  goodTable <- tableList == w
  d <- info_$d[1 + length(u)]
  theta <- info_$theta[1 + length(u)]
  if (key == "empty") {
    previousProba <- 1 / length(voc)
  } else{
    previousProba <- wordProbability(u[-1], w)
  }
  proba <-
    c(max(0, custList * goodTable - d),
      (theta + d * length(tableList)) * previousProba)
  proba <- proba / sum(proba)
  case <- sample(x = 1:length(proba),
                 size = 1,
                 prob = proba)
  if (case == length(proba)) {
    # Nouvelle table
    tableList <- append(tableList, w)
    custList <- append(custList, 1)
    if (key != "empty") {
      addCustomer(u[-1], w, voc)
    }
  } else{
    # Ancienne table
    custList[case] <- custList[case] + 1
  }
  info_$context[[key]]$tableList <<- tableList
  info_$context[[key]]$custList <<- custList
}

removeCustomer <- function(u = c(), w, voc = naiveVoc) {
  key <- getKeyByContext(u)
  tableList <- info_$context[[key]]$tableList
  custList <- info_$context[[key]]$custList
  goodTable <- tableList == w
  proba <- custList * goodTable
  case <- sample(x = 1:length(proba),
                 size = 1,
                 prob = proba)
  custList[case] <- custList[case] - 1
  if (custList[case] == 0) {
    # Cas ou la table se vide
    custList <- custList[-case]
    tableList <- tableList[-case]
    if (key != "empty") {
      # On supprime pour le contexte suffixe si non vide
      removeCustomer(u[-1], w, voc)
    }
  }
  info_$context[[key]]$tableList <<- tableList
  info_$context[[key]]$custList <<- custList
}

updateDiscount <- function(depth = 0, prior=c(1,1),voc = naiveVoc) {
  theta <- info_$theta[depth+1]
  d <- info_$d[depth+1]
  lapply(names(info_$context),function(key){
    context <- getContextByKey(key)
    if(length(context)!=depth){
      return(0)
    }else{
      y <- do.call(c,lapply(1:(length(info_$context[[key]]$tableList)-1)),function(i){
        proba <- theta/(theta+d*i)
        return(rbinom(1,1,proba))
      })
      return(sum(1-y))
    }
  })
  
}


