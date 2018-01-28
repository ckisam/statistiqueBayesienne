##################################################
##### PROJET STATISTIQUE BAYESIENNE
##### Lecture du fichier APNews
##### (26/01/2017)
##################################################

rm(list = ls())
# wd <-
#   "C:/Users/Samuel/Documents/ENSAE - Stat Bayes/statistiqueBayesienne"
wd <-
  "/home/samuel/Documents/statistiqueBayesienne"
setwd(wd)
getwd()

if (FALSE) {
  if (FALSE) {
    # data <-
    #   tolower(readLines("D:/ENSAE/3A/STATISTIQUE BAYESIENNE/apnews/apnews.dat"))
    data <-
      tolower(readLines("/media/samuel/DATA/ENSAE/3A/STATISTIQUE BAYESIENNE/apnews/apnews.dat"))
    saveRDS(data, "apnews.rds")
  }
  data <-
    readRDS("apnews.rds")
  treatedData <- strsplit(data, "   ")
  treatedData <- lapply(treatedData, function(t) {
    require(qdapRegex)
    require(stringr)
    # str_replace_all(data, "[^[:alnum:]]", " ")
    # return(rm_white(gsub("[-_().?!:,;'`\"/\\]", " ", t[2:length(t)])))
    return(rm_white(str_replace_all(t[2:length(t)], "[^[:alnum:]]", " ")))
  })
  saveRDS(treatedData, "apnews-pretreat.rds")
}
if (FALSE) {
  treatedData <- readRDS("apnews-pretreat.rds")
  if (FALSE) {
    dico <- sort(unique(do.call(c, lapply(treatedData, function(t) {
      unique(do.call(c, strsplit(t, " ")))
    }))))
    saveRDS(dico, "dictionnaire.rds")
  }
  if (FALSE) {
    sentences <- do.call(c, treatedData)
    saveRDS(sentences, "apnews-sentences.rds")
  }
  if(FALSE){
    sentences <- readRDS("apnews-sentences.rds")
    # > length(sentences)
    # [1] 2520621
    nword <- do.call(c, lapply(strsplit(sentences," "),length))
    sentences <- sentences[nword>2]
    # > length(sentences)
    # [1] 2473904
    library(ngram)
    ngram <- get.phrasetable(ngram(sentences, n = 3))
  }
}

dico <- readRDS("dictionnaire.rds")

### Travailler sur treatedData
test <- list()
for(i in 1:1000){
  test[[i]] <- treatedData[[i]]
}
test <- lapply(test, function(t){return(strsplit(t," "))})
extractNGram <- function(l){
  s <- length(l)
  if(s<=1){
    return(l)
  }else if(s==2){
    return(c(l[1], paste(l, collapse="_")))
  }
  return(do.call(c, lapply(3:s, function(i){
    if(i-2<=0){print(l)}
    return(paste(l[(i-2):i], collapse="_"))
  })))
}
caca <- lapply(test, function(t){
  return(do.call(c,lapply(t, extractNGram)))
})

###
test <- list()
for(i in 1:1000){
  test[[i]] <- treatedData[[i]]
}
test <- do.call(c,test)
nul <- lengths(regmatches(test, gregexpr(" ", test)))
test <- get.phrasetable(ngram(testFullFin, n = 3, " "))

### Eureka ?
library(ngram)
ngram <- do.call(c, treatedData)
ngram <-
  get.phrasetable(ngram(ngram[lengths(regmatches(ngram, gregexpr(" ", ngram)))], n = 3, " "))

### Eureka !
library(ngram)
res <- NULL
pas <- 20
# nchunk <- floor(length(treatedData) / pas)
nchunk <- 0
for (i in 1:(nchunk + 1)) {
  print(i)
  d <- (i - 1) * pas + 1
  f <- i * pas
  chunk <-
    do.call(c, lapply(d:min(f, length(treatedData)), function(j) {
      return(treatedData[[j]])
    }))
  chunk <-
    get.phrasetable(ngram(chunk[lengths(regmatches(chunk, gregexpr(" ", chunk))) >
                                  1], n = 3, " "))
  if (is.null(res)) {
    res <- chunk
  } else{
    res$freq[res$ngrams %in% chunk$ngrams] <-
      res$freq[res$ngrams %in% chunk$ngrams] + chunk$freq[chunk$ngrams %in% res$ngrams]
    res <- rbind(res, chunk[!chunk$ngrams %in% res$ngrams,])
  }
}
res$ngrams <- substr(res$ngrams, 1, nchar(res$ngrams) - 1)
res$ngrams <- do.call(c, lapply(res$ngrams, function(t) {
  return(gsub(" ", "_", t))
}))
dico <- unique(do.call(c, strsplit(res$ngrams, "_")))

### Avec un petit jeu de donn?es
rm(list = ls())
library(qdapRegex)
library(ngram)
data <- readRDS("apnews.rds")[1:100]
separator <- " "
ngram <- get.phrasetable(ngram(data, n = 3, separator))
dico <-
  sort(rm_white(get.phrasetable(ngram(data, n = 1, separator))$ngrams))
studied <- do.call(c,
                   lapply(strsplit(ngram$ngrams, separator), function(t) {
                     return(paste(match(t, dico), collapse = "_"))
                   }))

getIndex <- function(word, dico){
  return(match(word, dico))
}

### Essai basique
data <- readRDS("apnews.rds")
library(qdapRegex)
#nWord <- do.call(c,lapply(strsplit(rm_white(data)," "),length))
#data <- data[nWord>2]
data <- paste(data, collapse = " ")
data <- rm_white(data)
#data <- str_replace_all(data, "[^[:alnum:]]", " ")
library(ngram)
n3gram <- ngram(data, n = 3)

data <- data[1:100]

library(quanteda) # Facilite le traitement de donn?es textuelles.
library(tidyr) # S?paration des bigrams dans les dataframe; pas ?l?gant...

data1 <- data[1]
library(qdap)
freqTerms <- freq_terms(data1, Inf)
freqTermsFull <- freq_terms(data, Inf)

library(ngram)
separator <- ", .?!`'\""
ngram1 <- ngram(data1, n = 3, sep = separator)
ngramFull <- ngram(data, n = 3, sep = separator)
test1 <- get.phrasetable(ngram1)
testFull <- get.phrasetable(ngramFull)
uniqueWords1 <-
  sort(get.phrasetable(ngram(data1, n = 1, sep = separator))$ngrams)

#dictionnaire <- sort(freq_terms(data, Inf)$WORD)
data1 <- data[1]
data1 <- lapply(strsplit(rm_white(data1), "[?.!:;]")[[1]], rm_white)


data <- readRDS("apnews.rds")
data <-
  gsub("[^.?![:^punct:]]", "", data, perl = T) # Enleve toute ponctuation sauf les points

trigram <-
  dfm(
    data,
    remove = c("'", "`", ","),
    remove_punct = FALSE,
    ngrams = 3
  )

trigram <- data.frame(as.matrix(colSums(trigram)))
trigram <-
  cbind(rownames(trigram), data.frame(trigram, row.names = NULL))
colnames(trigram)[1] <- "words"
colnames(trigram)[2] <- "counts"
trigram <-
  trigram %>% separate(words, c("one", "two", "three"), "_")
trigram <-
  trigram[trigram[, 3] != ".", ]  # On retire les ngrams finissant par un point
trigram[trigram[, 2] == ".", ][, 1] <-
  "." # "mot1 . mot2" devient ". . mot2"
trigram <-
  trigram[trigram[, 3] != "?", ]  # On consid?re les ? et ! comme des points.
trigram[trigram[, 2] == "?", ][, 1:2] <- "."
trigram <- trigram[trigram[, 3] != "!", ]
trigram[trigram[, 2] == "!", ][, 1:2] <- "."

# Il serait pr?f?rable de le d?finir ? partir de data directement...
# Car ici dico cree ? partir des 3-grams: pas logique par rapport aux donn?es de test.
dico <-
  data.frame(as.vector(unique(c(
    trigram[, 1], trigram[, 2], trigram[, 3]
  ))))
colnames(dico)[1] <- "words"
dico[c(1, which(dico == ".")), ] <- dico[c(which(dico == "."), 1), ]
dico$integers <- c(0:(nrow(dico) - 1))

# V1
word2int <- function(word, voc = naiveVoc) {
  return(match(word, dico$words) - 1)
}
trigramInt <- trigram[, 1:3]
trigramInt[] <- lapply(trigramInt, word2int)
trigramInt$counts <- trigram$counts
trigram <- trigramInt

# V2
# word2int <- function(word) {
#   dico[which(dico[, 1] == word), 2]
# }
# int2word <- function(int) {
#   dico[which(dico[, 2] == int), 1]
# }
#
# for (i in 1:nrow(trigram)) {
#   for (k in 1:3) {
#     trigram[i, k] <- word2int(trigram[i, k])
#   }
# }

trigram <-
  data.frame(paste(trigram[, 1], trigram[, 2], trigram[, 3], sep = "_"), trigram[, 4])
colnames(trigram)[c(1, 2)] <- c("pats", "occs")
# trigram <- list(trigram)
