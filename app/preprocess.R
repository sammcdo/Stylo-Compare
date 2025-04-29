library(shiny)
library(shinydashboard)
library(ggplot2)
library(stylo)
library(ggplot2)
library(tidyr)
library(openNLP)
library(sylcount)
library(DT)
library(stylo)
library(NLP)
library(stringr)
library(dplyr)
library(tidytext)
library(tm)

source("helpers.R")

targets <- c(
  "pride_and_prejudice",
  "jane_eyre",
  "woman_in_white",
  "frankenstein",
  "chatgpt"
)


for(target in targets) {
  text <- readLines(paste("../sample_data/",target,".txt", sep=""))
  text_w <- txt.to.words(text)
  text_s <- split_into_sentences(text)
  
  text_cs <- split(text_s, ceiling(seq_along(text_s) / CHUNK_SIZE_SENT))
  text_c <- lapply(text_cs, function(sentences) txt.to.words(paste(sentences, collapse = " ")))
  
  # Convert the word list to a Corpus
  corpus <- Corpus(VectorSource(text_w))
  
  # Preprocess the Corpus (remove stop words, punctuation, etc.)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("en"))  # English stop words
  corpus <- tm_map(corpus, stripWhitespace)
  print("corpus")
  print(target)
  
  
  out <- list(
    text = text,
    text_w = text_w,
    text_s = text_s,
    text_cs = text_cs,
    text_c = text_c,
    text_corpus = corpus
  )
  
  save(out, file=paste("./cache/",target,".RData", sep=""))
  
}
