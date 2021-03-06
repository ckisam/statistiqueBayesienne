library(quanteda) # Facilite le traitement de donn�es textuelles.
library(tidyr) # S�paration des bigrams dans les dataframe; pas �l�gant...

#data <- readLines("C:/Users/Guillaume/Desktop/apnews/apnews.dat", n=100) # Sous-�chantillon
data <- readLines("D:/ENSAE/3A/STATISTIQUE BAYESIENNE/apnews/apnews.dat", n=100) # Sous-�chantillon

unigram <- dfm(data, remove = stopwords("english"), remove_punct = TRUE) 
  # Traitement du texte (ponctuations, etc...) et comptage des mots

unigram <- data.frame(as.matrix(colSums(unigram))) # Matrice associant � chaque mot son nombre d'occurences
unigram <- cbind(rownames(unigram), data.frame(unigram, row.names=NULL))
colnames(unigram)[1] <- "words"
colnames(unigram)[2] <- "counts"

bigram <- dfm(data, remove = stopwords("english"), remove_punct = TRUE, ngrams=2) 
bigram <- data.frame(as.matrix(colSums(bigram))) 
bigram <- cbind(rownames(bigram), data.frame(bigram, row.names=NULL))
colnames(bigram)[1] <- "words"
colnames(bigram)[2] <- "counts"
bigram <- bigram %>% separate(words, c("one", "two"), "_")

