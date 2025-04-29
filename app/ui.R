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
            #fileInput("austen_file", "Upload First Source Text", accept = c(".txt")),
            selectInput(
              inputId = "source1",         # A unique identifier for the input
              label = "Choose an option:",  # Label displayed above the dropdown
              choices = c("Pride and Prejudice", "Jane Eyre", "Frankenstein", "Woman In White"),  # Dropdown choices
              selected = "Pride and Prejudice"         # Default selected option
            )
          ),
          box(
            title="Second Source:",
            status="info", solidHeader=TRUE,
            #fileInput("bronte_file", "Upload Second Source Text", accept = c(".txt")),
            selectInput(
              inputId = "source2",         # A unique identifier for the input
              label = "Choose an option:",  # Label displayed above the dropdown
              choices = c("Pride and Prejudice", "Jane Eyre", "Frankenstein", "Woman In White"),  # Dropdown choices
              selected = "Jane Eyre"         # Default selected option
            )
          )
        )
      )
    )
  )
)