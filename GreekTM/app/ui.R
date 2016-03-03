shinyUI(fluidPage(
  titlePanel("Thucydides Sight Passage Generator (based on topic-modelling data"),
  fluidRow(
    
    column(3, wellPanel(
      selectInput("book", "From which book?",
                  c("Book 1" = 1, "Book 2" = 2, "Book 3" = 3, "Book 4" = 4,
                    "Book 5" = 5, "Book 6" = 6, "Book 7" = 7,
                    "Book 8" = 8),
                  selected = c(1),
                  multiple = TRUE
      )
    )),
    
    column(3, wellPanel(
      # This outputs the dynamic UI component
      uiOutput("ui")
    )),
    
    column(3,
           tags$p("Input type:"),
           verbatimTextOutput("input_type_text"),
           tags$p("Dynamic input value:"),
           verbatimTextOutput("dynamic_value")
    )
  )
))