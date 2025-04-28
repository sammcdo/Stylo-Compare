library(shiny)
library(shinydashboard)
library(ggplot2)
library(stylo)
library(ggplot2)
library(tidyr)
library(openNLP)
library(sylcount)
library(stringr)
library(tm)
library(DT)
source("helpers.R")

CHUNK_SIZE = 5000

austen <- readLines("pride_and_prejudice.txt")
austen_w <- txt.to.words(austen)
austen_s <- split_into_sentences(austen)

austen_cs <- split(austen_s, ceiling(seq_along(austen_s) / CHUNK_SIZE_SENT))
austen_c <- lapply(austen_cs, function(sentences) txt.to.words(paste(sentences, collapse = " ")))

bronte <- readLines("jane_eyre.txt")
bronte_w <- txt.to.words(bronte)
bronte_s <- split_into_sentences(bronte)

bronte_cs <- split(bronte_s, ceiling(seq_along(bronte_s) / CHUNK_SIZE_SENT))
bronte_c <- lapply(bronte_cs, function(sentences) txt.to.words(paste(sentences, collapse = " ")))

mlen <- min(length(austen_c), length(bronte_c))
austen_c <- austen_c[1:mlen]
bronte_c <- bronte_c[1:mlen]

mlen2 <- min(length(austen_cs), length(bronte_cs))
austen_cs <- austen_cs[1:mlen2]
bronte_cs <- bronte_cs[1:mlen2]

print("austen")
print(length(austen_c))
print("bronte")
print(length(bronte_c))

# Assuming the calculate_ttr function is already defined
calculate_ttr <- function(words) {
  types <- length(unique(words))  # Number of unique words
  tokens <- length(words)         # Total number of words
  return(types / tokens)          # TTR formula
}

# Apply the TTR function to each chunk in both austen_c and bronte_c
austen_ttr <- sapply(austen_c, calculate_ttr)
bronte_ttr <- sapply(bronte_c, calculate_ttr)

# Create a sample_id for each chunk (e.g., Austen1, Austen2, ..., Bronte1, Bronte2, ...)
austen_sample_ids <- paste0("Austen", seq_along(austen_ttr))
bronte_sample_ids <- paste0("Bronte", seq_along(bronte_ttr))

# Combine TTR values and sample_ids from both Austen and Bronte
all_ttr <- c(austen_ttr, bronte_ttr)
all_sample_ids <- c(austen_sample_ids, bronte_sample_ids)

austen_fre <- sapply(austen_cs, calculate_fre)
bronte_fre <- sapply(bronte_cs, calculate_fre)
all_fre <- c(austen_fre, bronte_fre)

austen_dce <- sapply(austen_cs, calculate_dce)
bronte_dce <- sapply(bronte_cs, calculate_dce)
all_dce <- c(austen_dce, bronte_dce)

austen_gfi <- sapply(austen_cs, calculate_gfi)
bronte_gfi <- sapply(bronte_cs, calculate_gfi)
all_gfi <- c(austen_gfi, bronte_gfi)

austen_awl <- sapply(austen_c, calculate_awl)
bronte_awl <- sapply(bronte_c, calculate_awl)
all_awl <- c(austen_awl, bronte_awl)

# POS
input_cs <- c(austen_cs, bronte_cs)
austen_pos_list <- lapply(input_cs, function(sentences) {
  text_chunk <- paste(sentences, collapse = " ")  # Join sentences into one string
  create_pos_dataframe(text_chunk)
})
summarize_pos <- function(pos_df) {
  aggregate(Frequency ~ Category, data = pos_df, sum)
}
austen_pos_summary <- lapply(austen_pos_list, summarize_pos)
austen_pos_vectors <- lapply(austen_pos_summary, function(df) {
  setNames(df$Frequency, df$Category)
})
austen_pos_df <- bind_rows(lapply(austen_pos_vectors, as.data.frame.list))
austen_pos_df[is.na(austen_pos_df)] <- 0


res <- data.frame(
  sample_id = all_sample_ids,
  TTR = all_ttr,
  FRE = all_fre,
  DCE = all_dce,
  GFI = all_gfi,
  AWL = all_awl
  
)
res <- cbind(res, austen_pos_df)

sample_ids <- c(
  paste0("Source 1.", seq_along(austen_cs)),
  paste0("Source 2.", seq_along(bronte_cs))
)

res$sample_id <- sample_ids

res_n <- res[, sapply(res, is.numeric)]

res_n <- res_n[, apply(res_n, 2, var) != 0]

res <- cbind(res[, "sample_id", drop = FALSE], res_n)

pca_result <- prcomp(res[, -1], scale. = TRUE)

View(res)
pca_df <- data.frame(pca_result$x)
pca_df$sample_id <- res$sample_id
pca_df$author <- ifelse(grepl("^Source 1", res$sample_id), "Source 1", "Source 2")

ggplot(pca_df, aes(x = PC1, y = PC2, label = sample_id, color = author)) +
  geom_point() +
  geom_text(vjust = -0.5) +
  theme_minimal() + 
  scale_color_manual(values = c("Source 1" = "#F8766D", "Source 2" = "#00BFC4")) +
  theme(legend.position = "none")
