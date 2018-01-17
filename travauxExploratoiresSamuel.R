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
##### ----- 1ere implementation, un peu complexe

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

initChineseRestaurant <-
  function(depth = 3,
           d = rep(1, depth),
           theta = rep(1, depth),
           voc = naiveVoc) {
    context <<-
      list(empty = initChineseRestaurantRec(depth, voc),
           d = d,
           theta = theta)
  }

initChineseRestaurantRec <-
  function(depth = 3,
           voc = naiveVoc) {
    res <-
      list(tableList = c(),
           custList = c())
    if (depth > 1) {
      res$sub <-
        lapply(1:length(voc), function(i) {
          return(initChineseRestaurantRec(depth - 1, voc))
        })
    }
    return(res)
  }
initChineseRestaurant()
print(context)

getProbaContext <- function(u = c(), context = context) {
  infoContext <- context$empty
  for (w in u) {
    infoContext <- infoContext$sub[[w]]
  }
  d <- context$d[1 + length(u)]
  theta <- context$theta[1 + length(u)]
  constNorm <- theta + sum(infoContext$custList)
  res <- list()
  res$existing <-
    (infoContext$custList - d * infoContext$tableList) / constNorm
  res$new <-  (theta + d * (sum(infoContext$tableList))) / constNorm
  return(res)
}

incrementCustomer <- function(u = c(), k, context = context) {
  context$empty <<- incrementCustomerRec(u, k, context$empty)
}

incrementCustomerRec <- function(u = c(),
                                 k,
                                 context = context$empty) {
  if (lentgh(u) == 0) {
    context$custList[k] <- context$custList[k] + 1
    return(context)
  } else{
    return(incrementCustomerRec(u[-1], k, context[[u[1]]]))
  }
}

drawWord_1 <-
  function(u = c(),
           d = naiveD,
           theta = naiveTheta,
           voc = naiveVoc) {
    contextSize <- length(u)
    if (contextSize == 0) {
      # Du mal avec cette etape : le contexte vide n'est pas Pitman ?
      index <- sample(1:length(voc), 1)
      return(voc[index])
    }
    proba <- getProbaContext(u)
    if (rbinom(1, 1, proba$new) == 1) {
      # Nouveau tirage
      word <- drawWord_1(u[-1])
    } else{
      # Ancien tirage
    }
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
           d = rep(1, depth),
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
  tableList <- info_$context[[key]]$tableList
  custList <- info_$context[[key]]$custList
  constNorm <- theta + sum(custList)
  #print(constNorm)
  res <- list()
  res$existing <-
    (custList - d * tableList) / constNorm
  res$new <-  (theta + d * (sum(tableList))) / constNorm
  return(res)
}

drawWord <-
  function(u = c(),
           voc = naiveVoc) {
    key <- getKeyByContext(u)
    proba <- getProbaContext(u)
    if (rbinom(1, 1, proba$new) == 1) {
      # Nouveau tirage
      if (length(u) == 0) {
        word <- sample(1:length(voc), 1)
      } else{
        word <- drawWord_1(u[-1])
      }
      info_$context$empty$tableList <<-
        append(info_$context$empty$tableList, word)
      info_$context$empty$custList <<-
        append(info_$context$empty$custList, 1)
    } else{
      # Ancien tirage
      custList <- info_$context[[key]]$custList
      tableList <- info_$context[[key]]$tableList
      k <- sample(1:length(tableList), 1, proba$existing)
      word <- tableList[k]
      custList[k] <- custList[k] + 1
      info_$context[[key]]$custList <<- custList
    }
    return(word)
  }

initChineseRestaurant()
print(info_)
drawWord()
drawWord()