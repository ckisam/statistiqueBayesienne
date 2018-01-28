######################################################################
##### PROJET STATISTIQUE BAYESIENNE
##### Modelisation bayesienne non-parametrique du langage
##### (03/01/2017)
######################################################################

rm(list = ls())
# wd <-
#   "C:/Users/Samuel/Documents/ENSAE - Stat Bayes/statistiqueBayesienne"
wd <-
  "/home/samuel/Documents/statistiqueBayesienne"
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

### Ajoute un consommateur
### - u : contexte, vecteur d'entier (= index du mot dans le dico)
### - w : mot a ajouter
### - voc : dictionnaire
addCustomer <- function(u = c(), w, voc = naiveVoc) {
  key <- getKeyByContext(u)
  tableList <- info_$context[[key]]$tableList
  custList <- info_$context[[key]]$custList
  goodTable <- tableList == w
  if(length(custList)!=length(tableList)){
    print("Attention !!")
  }
  d <- info_$d[1 + length(u)]
  theta <- info_$theta[1 + length(u)]
  if (key == "empty") {
    previousProba <- 1 / length(voc)
  } else{
    previousProba <- wordProbability(u[-1], w)
  }
  proba <-
    c(pmax(0, custList * goodTable - d),
      (theta + d * length(tableList)) * previousProba)
  proba <- proba / sum(proba)
  case <- sample(x = 1:length(proba),
                 size = 1,
                 prob = proba)
  if (case == length(proba)) {
    # Nouvelle table
    tableList <- c(tableList, w)
    custList <- c(custList, 1)
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
  if (sum(proba) == 0) {
    print(u)
    print(w)
    print(info_$context[[key]])
    print(proba)
  }
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

updateDiscount <- function(depth = 0,
                           prior = c(1, 1)) {
  theta <- info_$theta[depth + 1]
  fullD <- info_$d
  d <- fullD[depth + 1]
  alpha <-
    prior[1] + sum(do.call(c, lapply(names(info_$context), function(key) {
      context <- getContextByKey(key)
      t <- length(info_$context[[key]]$tableList)
      if (length(context) != depth | t < 2) {
        return(0)
      }
      y <- do.call(c, lapply(1:(t - 1), function(i) {
        proba <- theta / (theta + d * i)
        return(rbinom(1, 1, proba))
      }))
      return(sum(1 - y))
    })))
  beta <-
    prior[2] + sum(do.call(c, lapply(names(info_$context), function(key) {
      context <- getContextByKey(key)
      custList <- info_$context[[key]]$custList
      t <- length(custList)
      if (length(context) != depth | t < 2) {
        return(0)
      }
      return(sum(do.call(c, lapply(custList, function(cust) {
        if (cust < 2) {
          return(0)
        }
        z <- do.call(c, lapply(1:(cust - 1), function(j) {
          proba <- (j - 1) / (j - d)
          return(rbinom(1, 1, proba))
        }))
        return(sum(1 - z))
      }))))
    })))
  fullD[depth + 1] <- rbeta(1, alpha, beta)
  info_$d <<- fullD
}

updateTheta <- function(depth = 0,
                        prior = c(1, 1)) {
  d <- info_$d[depth + 1]
  fullTheta <- info_$theta
  theta <- fullTheta[depth + 1]
  alpha <-
    prior[1] + sum(do.call(c, lapply(names(info_$context), function(key) {
      context <- getContextByKey(key)
      t <- length(info_$context[[key]]$tableList)
      if (length(context) != depth | t < 2) {
        return(0)
      }
      y <- do.call(c, lapply(1:(t - 1), function(i) {
        proba <- theta / (theta + d * i)
        return(rbinom(1, 1, proba))
      }))
      return(sum(y))
    })))
  beta <-
    prior[2] - sum(do.call(c, lapply(names(info_$context), function(key) {
      context <- getContextByKey(key)
      t <- length(info_$context[[key]]$tableList)
      custList <- info_$context[[key]]$custList
      if (length(context) != depth | t < 2) {
        return(0)
      }
      return(log(rbeta(1, theta + 1, sum(custList - 1))))
    })))
  fullTheta[depth + 1] <- rgamma(1, alpha, beta)
  info_$theta <<- fullTheta
}

gibbsSampler <- function(data, voc = naiveVoc, n) {
  # Initialisation
  initChineseRestaurant()
  print("Init")
  for (g in seq_along(data$ngrams)) {
    u <- getContextByKey(data$ngrams[g])
    occ <- data$freq[g]
    for (o in 1:occ) {
      addCustomer(u = u[-length(u)], w = u[length(u)])
    }
    if (g %% 1000 == 0) {
      print(g)
    }
  }
  res <- rep(0, nrow(data))
  for (i in 1:n) {
    print(paste("Iteration ",i,sep=""))
    for (g in seq_along(data$ngrams)) {
      u <- getContextByKey(data$ngrams[g])
      occ <- data$freq[g]
      for (o in 1:occ) {
        removeCustomer(u = u[-length(u)], w = u[length(u)])
        addCustomer(u = u[-length(u)], w = u[length(u)])
      }
      if (g %% 1000 == 0) {
        print(g)
      }
    }
    for (j in 1:length(info_$theta)) {
      updateTheta(j - 1)
      updateDiscount(j - 1)
    }
    temp <- do.call(c, lapply(data$ngrams, function(g) {
      u <- getContextByKey(g)
      return(wordProbability(u = u[-length(u)], w = u[length(u)]))
    }))
    res <- ((i-1)*res+temp)/i
  }
  return(res)
}

# gibbs <- gibbsSampler(data = res, n = 2)
gibbs <- gibbsSampler(data = res[1:1000,], n = 2)
