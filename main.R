library(shiny)
library(shinydashboard)
library(ggplot2)
library(stylo)
library(ggplot2)
library(tidyr)
library(openNLP)
library(sylcount)
library(DT)


source("helpers.R")



ui <- dashboardPage(
  dashboardHeader(title = "Stylo-Compare", titleWidth = 240),
  dashboardSidebar(width=240,
                   sidebarMenu(
                     menuItem("Vocab", tabName="vocab", icon=icon("book")),
                     menuItem("Readability", tabName="read", icon=icon("book")),
                     menuItem("Lexical Analysis", tabName="lex", icon=icon("font")),
                     menuItem("Phraseology", tabName="phrase", icon=icon("comment")),
                     menuItem("Identification", tabName="pca", icon=icon("id-badge")),
                     menuItem("Settings", tabName="settings", icon=icon("gear"))
                   )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "vocab",
        fluidRow(
          box(
            title="Source 1 Type-Token Ratio",
            status="info", solidHeader = TRUE,
            textOutput("ttr_total_a"), width=6
          ),
          box(
            title="Source 2 Type-Token Ratio",
            status="info", solidHeader = TRUE,
            textOutput("ttr_total_b"), width=6
          )
        ),
        fluidRow(
          box(
            title="Type-Token Ratio",
            status="info", solidHeader = TRUE,
            plotOutput("ttr"), width=12
          )
        ),
        fluidRow(
          box(
            title="Rolling Type-Token Ratio",
            status="info", solidHeader = TRUE,
            plotOutput("rolling_ttr"), width=12
          )
        )
      ),
      tabItem(
        tabName = "read",
        fluidRow(
          box(
            title="Flesch Readability Score",
            status="info", solidHeader = TRUE,
            plotOutput("fre"), width=9
          ),
          column(width=3,
            box(
              title="Source 1 Flesch Readability Score",
              status="info", solidHeader = TRUE,
              textOutput("fre_a"), width=NULL
            ),
            box(
              title="Source 2 Flesch Readability Score",
              status="info", solidHeader = TRUE,
              textOutput("fre_b"), width=NULL
            )
          )
        ),
        fluidRow(
          box(
            title="Dale-Chall Readability Score",
            status="info", solidHeader = TRUE,
            plotOutput("dce"), width=9
          ),
          column(width=3,
             box(
               title="Source 1 Dale-Chall Readability Score",
               status="info", solidHeader = TRUE,
               textOutput("dce_a"), width=NULL
             ),
             box(
               title="Source 2 Dale-Chall Readability Score",
               status="info", solidHeader = TRUE,
               textOutput("dce_b"), width=NULL
             )
          )
        ),
        fluidRow(
          box(
            title="Gunning Fog Index",
            status="info", solidHeader = TRUE,
            plotOutput("gfi"), width=9
          ),
          column(width=3,
                 box(
                   title="Source 1 Gunning Fog Index",
                   status="info", solidHeader = TRUE,
                   textOutput("gfi_a"), width=NULL
                 ),
                 box(
                   title="Source 2 Gunning Fog Index",
                   status="info", solidHeader = TRUE,
                   textOutput("gfi_b"), width=NULL
                 )
          )
        )
      ),
      tabItem(
        tabName = "lex",
        fluidRow(
          box(
            title="Source 1 Average Word Length",
            status="info", solidHeader = TRUE,
            textOutput("awl_a"), width=6
          ),
          box(
            title="Source 2 Average Word Length",
            status="info", solidHeader = TRUE,
            textOutput("awl_b"), width=6
          )
        ),
        fluidRow(
          box(
            title="Source 1 Most Common Words",
            status="info", solidHeader = TRUE,
            plotOutput("wf_a"), width=6
          ),
          box(
            title="Source 2 Most Common Words",
            status="info", solidHeader = TRUE,
            plotOutput("wf_b"), width=6
          )
        ),
        fluidRow(
          box(
            title="Source 1 Least Common Words",
            status="info", solidHeader = TRUE,
            plotOutput("wf2_a"), width=6
          ),
          box(
            title="Source 2 Least Common Words",
            status="info", solidHeader = TRUE,
            plotOutput("wf2_b"), width=6
          )
        ),
        fluidRow(
          box(
            title="Source 1 Part of Speech Frequency",
            status="info", solidHeader = TRUE,
            plotOutput("pos_a"), width=6
          ),
          box(
            title="Source 2 Part of Speech Frequency",
            status="info", solidHeader = TRUE,
            plotOutput("pos_b"), width=6
          )
        )
      ),
      tabItem(
        tabName = "phrase",
        fluidRow(
          box(
            title="Source 1 Common Phrases (3 words)",
            status="info", solidHeader = TRUE,
            DTOutput("phrase3_a"), width=6
          ),
          box(
            title="Source 2 Common Phrases (3 words)",
            status="info", solidHeader = TRUE,
            DTOutput("phrase3_b"), width=6
          )
        ),
        fluidRow(
          box(
            title="Source 1 Common Phrases (4 words)",
            status="info", solidHeader = TRUE,
            DTOutput("phrase4_a"), width=6
          ),
          box(
            title="Source 2 Common Phrases (4 words)",
            status="info", solidHeader = TRUE,
            DTOutput("phrase4_b"), width=6
          )
        ),
        fluidRow(
          box(
            title="Source 1 Common Phrases (5 words)",
            status="info", solidHeader = TRUE,
            DTOutput("phrase5_a"), width=6
          ),
          box(
            title="Source 2 Common Phrases (5 words)",
            status="info", solidHeader = TRUE,
            DTOutput("phrase5_b"), width=6
          )
        )
      ),
      tabItem(
        tabName = "pca",
        fluidRow(
          box(
            title="PCA of Word Frequencies",
            status="info", solidHeader = TRUE,
            plotOutput("pca_freq"), width=12
          )
        ),
        fluidRow(
          box(
            title="First Component Highest Weighted Words",
            status="info", solidHeader = TRUE,
            DTOutput("pca_freq_words1"), width=6
          ),
          box(
            title="Second Componet Highest Weighted Words",
            status="info", solidHeader = TRUE,
            DTOutput("pca_freq_words2"), width=6
          )
        ),
        fluidRow(
          box(
            title="PCA of Stylo-Compare Metrics",
            status="info", solidHeader = TRUE,
            plotOutput("pca_cust"), width=12
          )
        ),
        fluidRow(
          box(
            title="First Component Highest Weighted Metrics",
            status="info", solidHeader = TRUE,
            DTOutput("pca_cust_words1"), width=6
          ),
          box(
            title="Second Componet Highest Weighted Metrics",
            status="info", solidHeader = TRUE,
            DTOutput("pca_cust_words2"), width=6
          )
        )
      ),
      tabItem(
        tabName = "settings",
        fluidRow(
          box(
            title="First Source:",
            status="info", solidHeader=TRUE,
            fileInput("austen_file", "Upload First Source Text", accept = c(".txt")),
          ),
          box(
            title="Second Source:",
            status="info", solidHeader=TRUE,
            fileInput("bronte_file", "Upload Second Source Text", accept = c(".txt")),
          )
        )
      )
    )
  )
)



server <- function(input, output){
  
  text_data <- reactive({
    # Ensure files are uploaded
    req(input$austen_file, input$bronte_file)
    
    if (is.null(input$austen_file) || is.null(input$bronte_file)) {
      austen <- readLines("pride_and_prejudice.txt")
      bronte <- readLines("jane_eyre.txt")
    } else {
      austen <- readLines(input$austen_file$datapath)
      bronte <- readLines(input$bronte_file$datapath)
    }
    
    # read source 1
    austen <- readLines("pride_and_prejudice.txt")
    austen_w <- txt.to.words(austen)
    austen_s <- split_into_sentences(austen)
    
    austen_cs <- split(austen_s, ceiling(seq_along(austen_s) / CHUNK_SIZE_SENT))
    austen_c <- lapply(austen_cs, function(sentences) txt.to.words(paste(sentences, collapse = " ")))
    
    # read source 2
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
    
    # Return everything in a list
    list(
      austen = austen,
      bronte = bronte,
      austen_w = austen_w,
      bronte_w = bronte_w,
      austen_s = austen_s,
      bronte_s = bronte_s,
      austen_c = austen_c,
      bronte_c = bronte_c,
      austen_cs = austen_cs,
      bronte_cs = bronte_cs
    )
  })
  
  output$ttr_total_a <- renderText({
    a <- calculate_ttr(text_data()$austen_w)
    a
  })
  
  output$ttr_total_b <- renderText({
    b <- calculate_ttr(text_data()$bronte_w)
    b
  })
  
  
  output$ttr <- renderPlot({
    a_ttr <- sapply(text_data()$austen_c, calculate_ttr)
    b_ttr <- sapply(text_data()$bronte_c, calculate_ttr)
    
    res <- data.frame(
      Source1 = a_ttr,
      Source2 = b_ttr,
      x = 1:mlen
    )
    
    res <- pivot_longer(res, cols = c(Source1, Source2), names_to = "auth", values_to = "vals")
    
    ggplot( res, aes(x=x, y=vals, fill=auth)) + 
      geom_bar(stat="identity", position="dodge") +
      labs(x="Chunk", y="Type-Token Ratio", fill="Authors") +
      theme_minimal()
  
  })
  
  output$rolling_ttr <- renderPlot({
    rolling_ttr_a <- c()
    rolling_ttr_b <- c()
    
    for (i in 1:length(austen_c)) {
      currentSubsetA <- text_data()$austen_w[1:(i*CHUNK_SIZE)]
      currentSubsetB <- text_data()$bronte_w[1:(i*CHUNK_SIZE)]
      rolling_ttr_a[i] <- calculate_ttr(currentSubsetA)
      rolling_ttr_b[i] <- calculate_ttr(currentSubsetB)
    }
    
    res <- data.frame(
      Source1 = rolling_ttr_a,
      Source2 = rolling_ttr_b,
      x = 1:mlen
    )
    res <- pivot_longer(res, cols = c(Source1, Source2), names_to = "auth", values_to = "vals")
    
    ggplot( res, aes(x=x, y=vals, fill=auth)) + 
      geom_bar(stat="identity", position="dodge") +
      labs(x="Chunk", y="Type-Token Ratio", fill="Authors") +
      theme_minimal()
  })
  
  output$fre <- renderPlot({
    a_fre <- sapply(text_data()$austen_cs, calculate_fre)
    b_fre <- sapply(text_data()$bronte_cs, calculate_fre)
    
    
    res <- data.frame(
      Source1 = a_fre,
      Source2 = b_fre,
      x = 1:mlen2
    )
    
    res <- pivot_longer(res, cols = c(Source1, Source2), names_to = "auth", values_to = "vals")
    
    ggplot( res, aes(x=x, y=vals, fill=auth)) + 
      geom_bar(stat="identity", position="dodge") +
      labs(x="Chunk", y="Flesh Score") +
      theme_minimal()
  })
  
  output$fre_a <- renderText({
    calculate_fre(text_data()$austen_s)
  })
  
  output$fre_b <- renderText({
    calculate_fre(text_data()$bronte_s)
  })
  
  output$dce <- renderPlot({
    a_dce <- sapply(text_data()$austen_cs, calculate_dce)
    b_dce <- sapply(text_data()$bronte_cs, calculate_dce)
    
    res <- data.frame(
      Source1 = a_dce,
      Source2 = b_dce,
      x = 1:mlen2
    )
    
    res <- pivot_longer(res, cols = c(Source1, Source2), names_to = "auth", values_to = "vals")
    
    ggplot( res, aes(x=x, y=vals, fill=auth)) + 
      geom_bar(stat="identity", position="dodge") +
      labs(x="Chunk", y="Dale-Chall Score") +
      theme_minimal()
  })
  
  output$dce_a <- renderText({
    calculate_dce(text_data()$austen_s)
  })
  
  output$dce_b <- renderText({
    calculate_dce(text_data()$bronte_s)
  })
  
  output$gfi <- renderPlot({
    a_gfi <- sapply(text_data()$austen_cs, calculate_gfi)
    b_gfi <- sapply(text_data()$bronte_cs, calculate_gfi)
    
    res <- data.frame(
      Source1 = a_gfi,
      Source2 = b_gfi,
      x = 1:mlen2
    )
    
    res <- pivot_longer(res, cols = c(Source1, Source2), names_to = "auth", values_to = "vals")
    
    ggplot( res, aes(x=x, y=vals, fill=auth)) + 
      geom_bar(stat="identity", position="dodge") +
      labs(x="Chunk", y="Dale-Chall Score") +
      theme_minimal()
  })
  
  output$gfi_a <- renderText({
    calculate_gfi(text_data()$austen_s)
  })
  
  output$gfi_b <- renderText({
    calculate_gfi(text_data()$bronte_s)
  })
  
  output$awl_a <- renderText({
    calculate_awl(text_data()$austen_w)
  })
  
  output$awl_b <- renderText({
    calculate_awl(text_data()$bronte_w)
  })
  
  output$wf_a <- renderPlot({
    wf_a <- create_frequency_dataframe(text_data()$austen_w)
    wf_a$word <- factor(wf_a$word, levels = unique(wf_a$word))
    ggplot( wf_a, aes(x=word, y=frequency)) + 
      geom_bar(stat="identity", fill="#F8766D") +
      labs(x="Words", y="Occurences") +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  output$wf_b <- renderPlot({
    wf_b <- create_frequency_dataframe(text_data()$bronte_w)
    wf_b$word <- factor(wf_b$word, levels = unique(wf_b$word))
    ggplot( wf_b, aes(x=word, y=frequency)) + 
      geom_bar(stat="identity", fill="#00BFC4") +
      labs(x="Words", y="Occurences") +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$wf2_a <- renderPlot({
    wf_a <- create_frequency_dataframe(text_data()$austen_w, common=FALSE)
    wf_a$word <- factor(wf_a$word, levels = unique(wf_a$word))
    ggplot( wf_a, aes(x=word, y=frequency)) + 
      geom_bar(stat="identity", fill="#F8766D") +
      labs(x="Words", y="Occurences") +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  output$wf2_b <- renderPlot({
    wf_b <- create_frequency_dataframe(text_data()$bronte_w, common=FALSE)
    wf_b$word <- factor(wf_b$word, levels = unique(wf_b$word))
    ggplot( wf_b, aes(x=word, y=frequency)) + 
      geom_bar(stat="identity", fill="#00BFC4") +
      labs(x="Words", y="Occurences") +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$pos_a <- renderPlot({
    wf_a <- create_pos_dataframe(text_data()$austen)
    ggplot( wf_a, aes(x=Category, y=Frequency)) + 
      geom_bar(stat="identity", fill="#F8766D") +
      labs(x="Part of Speech", y="Occurences") +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  output$pos_b <- renderPlot({
    wf_b <- create_pos_dataframe(text_data()$bronte)
    ggplot( wf_b, aes(x=Category, y=Frequency)) + 
      geom_bar(stat="identity", fill="#00BFC4") +
      labs(x="Part of Speech", y="Occurences") +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$phrase3_a <- renderDT({
    df <- create_phrase_df(text_data()$austen_s, 3)
    datatable(df, options = list(pageLength = 5, autoWidth = TRUE))
  })
  
  output$phrase3_b <- renderDT({
    df <- create_phrase_df(text_data()$bronte_s, 3)
    datatable(df, options = list(pageLength = 5, autoWidth = TRUE))
  })
  
  output$phrase4_a <- renderDT({
    df <- create_phrase_df(text_data()$austen_s, 4)
    datatable(df, options = list(pageLength = 5, autoWidth = TRUE))
  })
  
  output$phrase4_b <- renderDT({
    df <- create_phrase_df(text_data()$bronte_s, 4)
    datatable(df, options = list(pageLength = 5, autoWidth = TRUE))
  })
  
  output$phrase5_a <- renderDT({
    df <- create_phrase_df(text_data()$austen_s, 5)
    datatable(df, options = list(pageLength = 5, autoWidth = TRUE))
  })
  
  output$phrase5_b <- renderDT({
    df <- create_phrase_df(text_data()$bronte_s, 5)
    datatable(df, options = list(pageLength = 5, autoWidth = TRUE))
  })
  
  
  pca_data <- reactive({
    all_samples <- c(text_data()$austen_c, text_data()$bronte_c)
    counts_list <- lapply(all_samples, count_words_in_sample, top_words = top_words)
    
    counts_df <- as.data.frame(do.call(rbind, counts_list))
    
    sample_ids <- c(
      paste0("Source 1.", seq_along(text_data()$austen_c)),
      paste0("Source 2.", seq_along(text_data()$bronte_c))
    )
    
    counts_df$sample_id <- sample_ids
    
    numeric_counts_df <- counts_df[, sapply(counts_df, is.numeric)]
    
    numeric_counts_df <- numeric_counts_df[, apply(numeric_counts_df, 2, var) != 0]
    
    counts_df <- cbind(counts_df[, "sample_id", drop = FALSE], numeric_counts_df)
    
    pca_result <- prcomp(counts_df[, -1], scale. = TRUE)
    
    list(
      pca_result = pca_result,
      counts_df = counts_df
    )
  })
  
  output$pca_freq <- renderPlot({
    pca_result <- pca_data()$pca_result
    counts_df <- pca_data()$counts_df
    
    pca_df <- data.frame(pca_result$x)
    pca_df$sample_id <- counts_df$sample_id
    pca_df$author <- ifelse(grepl("^Source 1", pca_df$sample_id), "Source 1", "Source 2")
    
    ggplot(pca_df, aes(x = PC1, y = PC2, label = sample_id, color = author)) +
      geom_point() +
      geom_text(vjust = -0.5) +
      theme_minimal() + 
      scale_color_manual(values = c("Source 1" = "#F8766D", "Source 2" = "#00BFC4")) +
      theme(legend.position = "none")
  })
  
  output$pca_freq_words1 <- renderDT({
    loadings <- pca_data()$pca_result$rotation
    
    top10_PC1 <- sort(abs(loadings[, "PC1"]), decreasing = TRUE)[1:10]
    top10_PC1_df <- data.frame(
      word = names(top10_PC1),
      loading = loadings[names(top10_PC1), "PC1"]
    )
    
    datatable(top10_PC1_df, options = list(pageLength = 5, autoWidth = TRUE),
              rownames = FALSE)
  })
  
  output$pca_freq_words2 <- renderDT({
    loadings <- pca_data()$pca_result$rotation
    
    top10_PC2 <- sort(abs(loadings[, "PC2"]), decreasing = TRUE)[1:10]
    top10_PC2_df <- data.frame(
      word = names(top10_PC2),
      loading = loadings[names(top10_PC2), "PC2"]
    )
    
    datatable(top10_PC2_df, options = list(pageLength = 5, autoWidth = TRUE),
              rownames = FALSE)
  })
  
  pca_data_custom <- reactive({
    # Apply the TTR function to each chunk in both austen_c and bronte_c
    austen_ttr <- sapply(text_data()$austen_c, calculate_ttr)
    bronte_ttr <- sapply(text_data()$bronte_c, calculate_ttr)
    
    # Create a sample_id for each chunk (e.g., Austen1, Austen2, ..., Bronte1, Bronte2, ...)
    austen_sample_ids <- paste0("Austen", seq_along(austen_ttr))
    bronte_sample_ids <- paste0("Bronte", seq_along(bronte_ttr))
    
    # Combine TTR values and sample_ids from both Austen and Bronte
    all_ttr <- c(austen_ttr, bronte_ttr)
    all_sample_ids <- c(austen_sample_ids, bronte_sample_ids)
    
    austen_fre <- sapply(text_data()$austen_cs, calculate_fre)
    bronte_fre <- sapply(text_data()$bronte_cs, calculate_fre)
    all_fre <- c(austen_fre, bronte_fre)
    
    austen_dce <- sapply(text_data()$austen_cs, calculate_dce)
    bronte_dce <- sapply(text_data()$bronte_cs, calculate_dce)
    all_dce <- c(austen_dce, bronte_dce)
    
    austen_gfi <- sapply(text_data()$austen_cs, calculate_gfi)
    bronte_gfi <- sapply(text_data()$bronte_cs, calculate_gfi)
    all_gfi <- c(austen_gfi, bronte_gfi)
    
    austen_awl <- sapply(text_data()$austen_c, calculate_awl)
    bronte_awl <- sapply(text_data()$bronte_c, calculate_awl)
    all_awl <- c(austen_awl, bronte_awl)
    
    # POS
    input_cs <- c(text_data()$austen_cs, text_data()$bronte_cs)
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
      paste0("Source 1.", seq_along(text_data()$austen_cs)),
      paste0("Source 2.", seq_along(text_data()$bronte_cs))
    )
    
    res$sample_id <- sample_ids
    
    res_n <- res[, sapply(res, is.numeric)]
    
    res_n <- res_n[, apply(res_n, 2, var) != 0]
    
    res <- cbind(res[, "sample_id", drop = FALSE], res_n)
    
    pca_result <- prcomp(res[, -1], scale. = TRUE)
    
    list(
      pca_result = pca_result,
      counts_df = res
    )
  })
  
  output$pca_cust <- renderPlot({
    pca_result <- pca_data_custom()$pca_result
    counts_df <- pca_data_custom()$counts_df
    
    pca_df <- data.frame(pca_result$x)
    pca_df$sample_id <- counts_df$sample_id
    pca_df$author <- ifelse(grepl("^Source 1", pca_df$sample_id), "Source 1", "Source 2")
    
    ggplot(pca_df, aes(x = PC1, y = PC2, label = sample_id, color = author)) +
      geom_point() +
      geom_text(vjust = -0.5) +
      theme_minimal() + 
      scale_color_manual(values = c("Source 1" = "#F8766D", "Source 2" = "#00BFC4")) +
      theme(legend.position = "none")
  })
  
  output$pca_cust_words1 <- renderDT({
    loadings <- pca_data_custom()$pca_result$rotation
    
    top10_PC1 <- sort(abs(loadings[, "PC1"]), decreasing = TRUE)[1:10]
    top10_PC1_df <- data.frame(
      word = names(top10_PC1),
      loading = loadings[names(top10_PC1), "PC1"]
    )
    
    datatable(top10_PC1_df, options = list(pageLength = 5, autoWidth = TRUE),
              rownames = FALSE)
  })
  
  output$pca_cust_words2 <- renderDT({
    loadings <- pca_data_custom()$pca_result$rotation
    
    top10_PC2 <- sort(abs(loadings[, "PC2"]), decreasing = TRUE)[1:10]
    top10_PC2_df <- data.frame(
      word = names(top10_PC2),
      loading = loadings[names(top10_PC2), "PC2"]
    )
    
    datatable(top10_PC2_df, options = list(pageLength = 5, autoWidth = TRUE),
              rownames = FALSE)
  })
  
  
}

shinyApp(ui=ui, server=server)