library(shiny)
library(shinyjs)

os <- read.csv("italian-word-list-verbs.csv")
clean_up <- function(data) {
  os <- data[4:nrow(data),1:2]
  os_col <- gsub("[0-9*#;!?+]", "" , os[,1])
  os[,1] <- os_col
  rownames(os) <- 1:nrow(os)
  as.matrix(os)
}
os <- clean_up(os)

validate_guess <- function(word, list_words = os) {
  tolower(word) %in% list_words
}


ui <- fluidPage(
  useShinyjs(),

  tags$style(
    'body {background-image: url("https://source.unsplash.com/3uJt73tr4hI");
    color: white;}
    .well {background-color: transparent;}
    .well img {position:absolute;
    border-radius: 50%;
    opacity: 0.7;
    top:150px;}'
    # 'body {background-image: url("jen-allen-symetra.jpg");
    # color: black;
    # .well {background-color: transparent;}}'
  ),
  
  titlePanel(
    "Impara Italiano!"
  ),
  
  sidebarLayout(
    sidebarPanel(
      textOutput("panel"),
      textOutput("word_list"),
      # img(src="jen-allen-symetra.jpg",
      #     height=300, width=150),
    HTML('<img src="jen-allen-symetra.jpg",
         height="300px" width="150px"/>')
    ),
    mainPanel(
      tabsetPanel(
        id = "tabset",
        tabPanel(
          "Parole Comuni",
          disabled(textInput("guess", "Indovina", "")),
          conditionalPanel(
            condition = "input.startBtn == 0",
            actionButton("startBtn", "Siete Pronti?")
          ),
          conditionalPanel(
            condition = "input.startBtn > 0",
            actionButton("guessBtn", "Sei Sicuro?"),
            actionButton("next_word", "Parola Successiva")
          )
        ),
        tabPanel(
          "Vino",
          textInput("stare", "Come stai?", ""),
          actionButton("submit", "Send!"),
          span(verbatimTextOutput("stare")),
          tags$head(tags$script(src = "message-handler.js")),
          actionButton("test", "testing!", class="btn-btn-success")
        ),
        tabPanel(
          "Cibo"
        )
      ),
      
      textOutput("correct"),
      textOutput("test")
    )
  )
)

ui