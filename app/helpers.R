CHUNK_SIZE <- 5000
CHUNK_SIZE_SENT <- 500

DALE_CHALL_EASY <- readLines("dale-chall.txt")
DALE_CHALL_EASY <- txt.to.words(DALE_CHALL_EASY)


split_into_sentences <- function(text) {
  # Ensure that text is properly formatted as a character string
  
  # Annotator for sentence tokenization
  sentence_annotator <- Maxent_Sent_Token_Annotator()
  
  text_as_string <- toString(text)
  
  # Annotate the text for sentence boundaries
  sentence_boundaries <- NLP::annotate(text_as_string, sentence_annotator)
  
  # Extract sentences using the annotation indices
  sentences <- sapply(sentence_boundaries[sentence_boundaries$type == "sentence"], function(annotation) {
    substr(text_as_string, annotation$start, annotation$end)
  })
  
  return(sentences)
  
}


# Count Syllables of a Word
count_syllables <- function(word) {
  # A simple heuristic for counting syllables
  vowels <- c("a", "e", "i", "o", "u", "y")
  word <- tolower(word)
  syllables <- str_count(word, paste(vowels, collapse = "|"))
  syllables <- max(1, syllables)  # Ensure at least 1 syllable per word
  return(syllables)
}



# Type-Token Ratio
calculate_ttr <- function(words) {
  types <- length(unique(words))  # Number of unique words
  tokens <- length(words)         # Total number of words
  return(types / tokens)          # TTR formula
}

# Flesch Reading Ease
calculate_fre <- function(sentences) {
  num_sentences <- length(sentences)
  words <- unlist(txt.to.words(sentences))
  num_words <- length(words)
  num_syllables <- sum(unlist(sapply(words, sylcount)))
  
  fre <- 206.835 - (1.015 * (num_words / num_sentences)) - (84.6 * (num_syllables / num_words))
  
  return(fre)
}

# Dale-Chall Reading Ease
calculate_dce <- function(sentences) {
  num_sentences <- length(sentences)
  words <- unlist(txt.to.words(sentences))
  num_words <- length(words)
  
  difficult_words <- sum(!tolower(words) %in% DALE_CHALL_EASY)
  
  difficult_word_percent <- (difficult_words / num_words) * 100
  
  score <- 0.1579 * difficult_word_percent + 0.0496 * (num_words / num_sentences)
  
  # Adjust if difficult_word_percent is greater than 5%
  if (difficult_word_percent > 5) {
    score <- score + 3.6365
  }
  
  return(score)
}

calculate_gfi <- function(sentences) {
  num_sentences <- length(sentences)
  words <- unlist(txt.to.words(sentences))
  num_words <- length(words)
  
  complex_words <- sum(sapply(words, function(word) count_syllables(word) >= 3))
  
  # Calculate the Gunning Fog Index
  fog_index <- 0.4 * ((num_words / num_sentences) + (complex_words / num_words) * 100)
  
  return(fog_index)
  
}

calculate_awl <- function(words) {
    return(mean(sapply(words, nchar)))
}

create_frequency_dataframe <- function(word_list, common=TRUE) {
  # Convert the word list to a Corpus
  #corpus <- Corpus(VectorSource(word_list))
  
  # Preprocess the Corpus (remove stop words, punctuation, etc.)
  #corpus <- tm_map(corpus, removePunctuation)
  #corpus <- tm_map(corpus, removeNumbers)
  #corpus <- tm_map(corpus, removeWords, stopwords("en"))  # English stop words
  #corpus <- tm_map(corpus, stripWhitespace)
  
  # Create a Term-Document Matrix
  tdm <- TermDocumentMatrix(word_list)
  matrix <- as.matrix(tdm)
  
  # Calculate word frequencies
  if (common) {
    word_freq <- sort(rowSums(matrix), decreasing = TRUE)
  } else {
    word_freq <- sort(rowSums(matrix), decreasing = FALSE)
  }
  
  # Create a dataframe with the top 15 words
  top_words <- head(word_freq, 15)
  freq_df <- data.frame(word = names(top_words), frequency = top_words)
  
  return(freq_df)
}

create_pos_dataframe <- function(text) {
  text <- as.String(text)
  # Annotators
  word_token_annotator <- Maxent_Word_Token_Annotator()
  sentence_token_annotator <- Maxent_Sent_Token_Annotator()
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  
  # Annotations
  annotations <- NLP::annotate(text, list(sentence_token_annotator, word_token_annotator))
  pos_annotations <- NLP::annotate(text, pos_tag_annotator, annotations)
  
  pos_mapping <- list(
    Noun = c("NN", "NNS", "NNP", "NNPS"),
    Verb = c("VB", "VBD", "VBG", "VBN", "VBP", "VBZ"),
    Adjective = c("JJ", "JJR", "JJS"),
    Adverb = c("RB", "RBR", "RBS"),
    Pronoun = c("PRP", "PRP$", "WP", "WP$"),
    Preposition = c("IN"),
    Conjunction = c("CC"),
    Interjection = c("UH")
  )
  
  
  # Extract POS Tags
  #pos_tags <- sapply(pos_annotations$features, `[[`, "POS")
  pos_tags <- unlist(lapply(pos_annotations$features, function(x) x["POS"]))
  
  pos_freq <- table(pos_tags)
  pos_freq_df <- as.data.frame(pos_freq)
  colnames(pos_freq_df) <- c("POS", "Frequency")
  pos_freq_df$Category <- sapply(pos_freq_df$POS, function(tag) {
    category <- names(pos_mapping)[sapply(pos_mapping, function(tags) tag %in% tags)]
    if (length(category) > 0) category else "Other"
  })
  summary_df <- aggregate(Frequency ~ Category, data = pos_freq_df, sum)
  
  return(pos_freq_df)
}

create_phrase_df <- function(sentences, n=2) {
  
  # Create a dataframe from the list of sentences
  text_data <- data.frame(text = sentences, stringsAsFactors = FALSE)
  
  # Tokenize text into n-grams
  ngrams <- text_data %>%
    unnest_tokens(ngram, text, token = "ngrams", n = n) %>% 
    count(ngram, sort = TRUE) %>%
    head(31)
  
  # Rename columns for clarity
  colnames(ngrams) <- c("Phrase", "Frequency")
  ngrams <- drop_na(ngrams)
  
  # Return the dataframe of common phrases
  return(ngrams)
}

count_words_in_sample <- function(sample_words, top_words) {
  sample_table <- table(sample_words)
  counts <- sapply(top_words, function(w) {
    if (w %in% names(sample_table)) sample_table[[w]] else 0
  })
  return(counts)
}
