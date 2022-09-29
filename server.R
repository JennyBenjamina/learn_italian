source("ui.R")

# word list used
word_list <- os

# validate guess
validate_guess <- function(word, choices = word_list) {
  !grepl("[^A-Za-z]", word) && tolower(word) %in% word_list
}

# render word list function
counter <- 1
render_word_list <- function(counter, words = word_list[-1]) {
  counter <<- counter + 1
  words[counter]
}

# check if english matches italian
check_correct <- function(word, words = os) {
  word <- tolower(word)
  if ( word %in% words[counter,2]) {
    "E correcetto!"
  } else {
    "Almost! Try again!"
  }
}


server <- function(input, output, session) {
  
  observeEvent(input$test, {
    session$sendCustomMessage(type = 'testmessage', message = "thanks")
  })
  
  # renders to Parole Comuni
  output$word_list <- renderText(
    word_list_event()
  )
  
  word_list_event <- eventReactive(
    input$startBtn, {
      enable("guess")
      word_list[counter]
    }
  )
  
  
  
  
  output$panel <- renderText({
    paste("Current: ", input$tabset)
  })
  
  # User's guesses
  output$correct <- eventReactive(
    input$guessBtn, {
      alert(check_correct(input$guess))
      reset("guess")
    }
  )
  
  observeEvent(input$tabset, {
    if (input$tabset != "Parole Comuni") {
      hide(id = "word_list")
    } else {
      show("word_list")
    }
  })
  
  
  # resets the guess textInput box
  observeEvent(input$next_word,{
    reset("guess")
  })
  observeEvent(input$next_word,{
    output$word_list <- renderText(render_word_list(counter))
  })
  

  
  
  
}

server