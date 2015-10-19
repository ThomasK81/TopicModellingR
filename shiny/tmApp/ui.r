library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Topic-Modelling based on DNZ API Queries"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("search_text", label = "Search Text", value = "christchurch+earthquake"),
      radioButtons("collection", label = "Collection", 
                 choices = list("ATL Cartoon Descriptions" = 1, "Index NZ Abstracts" = 2, "no search" = 3
                                ), selected = 3),
      textInput("date", label = "Year", value = "2011"),
      textInput("add_stopwords", label = "Additional Stopwords", value = "christchurch earthquake"),
      sliderInput("number_topics", label = "Number of Topics", min = 2, max = 25, value = 15),
      sliderInput("number_terms", label = "Number of Terms Shown", min = 15, max = 50, value = 25),
      sliderInput("iterations", label = "Iterations", min = 500, max = 5000, value = 500),
      submitButton("Submit")
      ),
    mainPanel(
    tabsetPanel(
      tabPanel("Plot", htmlOutput("topicmodels")),
      tabPanel("Search Results", tableOutput("catalogue")),
      tabPanel("Download Data", helpText(   
        a("Click Here to Download the Search Results",     href="/www/catalogue/catalogue.csv"),
        a("Click Here to Download the Search Results",     href="/www/catalogue/catalogue.csv")
      ))
    )))))