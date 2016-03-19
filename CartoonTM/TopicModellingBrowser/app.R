# setwd("~/OneDrive/TopicModellingR/CartoonTM/TopicModellingBrowser")

library(googleVis)
library(shiny)

## libraries needed

library(tm)
library(XML)
library(RCurl)
library(plyr)
library(lda)
library(LDAvis)
library(compiler)

## optional

library(RColorBrewer)
library(rCharts)
library(d3heatmap)
library(ngram)
library(reshape2)
library(rjson)

# Enable JIT-compiling

enableJIT(3)

# load data

CartoonWDoublesData <- readRDS(file="./www/CartoonWDoublesData.Rda")
CartoonData <- readRDS(file="./www/CartoonData.Rda")
CarINNZData <- readRDS(file="./www/CarINNZData.Rda")

# Shiny functions

server <- function(input, output) {
  datasetInput <- reactive({
    switch(input$dataset,
           "CartoonWDoublesData" = CartoonWDoublesData,
           "CartoonData" = CartoonData,
           "CarINNZData" = CarINNZData)})
  meansumInput <- reactive({
    switch(input$meansum,
           "Mean" = mean,
           "Sum" = sum)})
  CarartInput <- reactive({
    switch(input$carart,
           "Cartoons" = "Cartoons",
           "Articles" = "Articles",
           "Both" = "Both")})
  TopicInput <- reactive({
    switch(input$topic,
           "1" = 1,
           "2" = 2,
           "3" = 3,
           "4" = 4,
           "5" = 5,
           "6" = 6,
           "7" = 7,
           "8" = 8,
           "9" = 9,
           "10" = 10,
           "11" = 11,
           "12" = 12,
           "13" = 13,
           "14" = 14,
           "15" = 15,
           "16" = 16,
           "17" = 17
           )})
  Topic2Input <- reactive({
      switch(input$topic2,
             "0" = 0,
             "1" = 1,
             "2" = 2,
             "3" = 3,
             "4" = 4,
             "5" = 5,
             "6" = 6,
             "7" = 7,
             "8" = 8,
             "9" = 9,
             "10" = 10,
             "11" = 11,
             "12" = 12,
             "13" = 13,
             "14" = 14,
             "15" = 15,
             "16" = 16,
             "17" = 17
      )
  })
  
  output$topicmodels <- renderUI({  
    getPage<-function() {
      if (input$dataset == "CarINNZData") {link_topic <- "./CarINNZCompsRemoved_vis/index.html"}
      else if (input$dataset == "CartoonData") {link_topic <- "./CartoonCompsRemoved_vis/index.html"}
      else if (input$dataset == "CartoonWDoublesData") {link_topic <- "./CartoonWDoublesCompsRemoved_vis/index.html"}
      return(tags$iframe(src = link_topic
                         , style="width:100%;",  frameborder="0"
                         ,id="iframe"
                         , height = "700px"))
    }
    getPage()})

  output$view <- renderGvis({
    if (input$meansum == "Sum") {vaxisinput <- "[{viewWindowMode:'explicit', viewWindow:{min:0, max:24}}]"
                                 mean_sum <- sum
    }
    if (input$meansum == "Mean") {vaxisinput <- "[{viewWindowMode:'explicit', viewWindow:{min:0, max:1}}]"
                                  mean_sum <- mean
    }
    topic1 <- TopicInput()
    topic2 <- Topic2Input()
    if (topic2 != 0) {
      if (input$dataset == "CarINNZData") {
        themen <- 9 + topic1
        themen2 <- 9 + topic2
        INNZ <- grep("INNZ", as.character(datasetInput()[,"identifier"]), , fixed=TRUE)
        if (CarartInput() == "Cartoons") {
          eqset <- CarINNZData[-INNZ,]
        } else if (CarartInput() == "Articles") {
          eqset <- CarINNZData[INNZ,]
        } else if (CarartInput() == "Both") {
          eqset <- CarINNZData
        }
        test <- eqset[c(1,5,themen,themen2)]
        eqset.df <- aggregate(list(test[,3],test[,4]), list(month = test$Month), mean_sum)
        colnames(eqset.df) <- colnames(eqset[c(5,themen,themen2)])
      } else if (input$dataset == "CartoonData") {
        themen <- 39 + topic1
        themen2 <- 39 + topic2
        eqset <- CartoonData
        test <- eqset[c(1,9,themen,themen2)]
        eqset.df <- aggregate(list(test[,3], test[,4]), list(month = test$Month), mean_sum)
        colnames(eqset.df) <- colnames(eqset[c(9,themen,themen2)])
      }  else if (input$dataset == "CartoonWDoublesData") {
        themen <- 40 + topic1
        themen2 <- 40 + topic2
        eqset <- CartoonWDoublesData
        test <- eqset[c(1,10,themen,themen2)]
        eqset.df <- aggregate(list(test[,3],test[,4]), list(month = test$Month), mean_sum)
        colnames(eqset.df) <- colnames(eqset[c(10,themen,themen2)])
      }
      visualisation <- gvisSteppedAreaChart(eqset.df, xvar="Month", yvar=c(colnames(eqset.df)[2],colnames(eqset.df)[3]), 
                           options=list(
                             isStacked=FALSE, 
                             title="",
                             width=600, height=600, 
                             vAxes=vaxisinput, 
                             hAxis="{title:''}", 
                             legend="{position:'top'}"))
    }
    if (input$topic2 == "0") {
      if (input$dataset == "CarINNZData") {
      themen <- 9 + topic1
      INNZ <- grep("INNZ", as.character(datasetInput()[,"identifier"]), , fixed=TRUE)
      if (CarartInput() == "Cartoons") {
        eqset <- CarINNZData[-INNZ,]
      } else if (CarartInput() == "Articles") {
        eqset <- CarINNZData[INNZ,]
      } else if (CarartInput() == "Both") {
        eqset <- CarINNZData
      }
      test <- eqset[c(1,5,themen)]
      eqset.df <- aggregate(list(test[,3]), list(month = test$Month), mean_sum)
      colnames(eqset.df) <- colnames(eqset[c(5,themen)])
    } else if (input$dataset == "CartoonData") {
      themen <- 39 + topic1
      eqset <- CartoonData
      test <- eqset[c(1,9,themen)]
      eqset.df <- aggregate(list(test[,3]), list(month = test$Month), mean_sum)
      colnames(eqset.df) <- colnames(eqset[c(9,themen)])
    }  else if (input$dataset == "CartoonWDoublesData") {
      themen <- 40 + topic1
      eqset <- CartoonWDoublesData
      test <- eqset[c(1,10,themen)]
      eqset.df <- aggregate(list(test[,3]), list(month = test$Month), mean_sum)
      colnames(eqset.df) <- colnames(eqset[c(10,themen)])
    }
    visualisation <-gvisSteppedAreaChart(eqset.df, xvar="Month", yvar=c(colnames(eqset.df)[2]), 
                         options=list(
                           isStacked=FALSE, 
                           title="",
                           width=600, height=600, 
                           vAxes=vaxisinput, 
                           hAxis="{title:''}", 
                           legend="{position:'top'}"))
    }
    visualisation
    })
  
  output$view2 <- renderGvis({
    gvis_table <- datasetInput()
    if (input$dataset == "CarINNZData") {
      thema <- 9 + TopicInput()
      description <- 9
      monthdata <- 5
      author <- 7
      publisher <- 6
    } else if (input$dataset == "CartoonData") {
      thema <- 39 + TopicInput()
      description <- 14
      monthdata <- 9
      author <- 10
      publisher <- 11
    } else if (input$dataset == "CartoonWDoublesData") {
      thema <- 40 + TopicInput()
      description <- 15
      monthdata <- 10
      author <- 11
      publisher <- 12
    }
    gvis_table <- gvis_table[c(1,monthdata,author,publisher,description,thema)]
    gvis_table <- gvis_table[order(-gvis_table[,6], gvis_table[,2]),] 
    gvisTable(gvis_table, options=list(page='enable'))
  })
}

ui <- fluidPage(
  
  titlePanel("INNZ and Cartoons"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose a dataset:", 
                choices = c("CartoonWDoublesData", "CartoonData", "CarINNZData"),
                selected = "CarINNZData"
                ),
      selectInput("carart", "Cartoons, Articles, or both?", 
                choices = c("Cartoons", "Articles", "Both"),
                selected = "Both"
                ),
      selectInput("meansum", "Mean or Sum", 
                choices = c("Mean", "Sum"),
                selected = "Sum"
                ),
      selectInput("topic", "Choose a topic:", 
                choices = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17")),
      selectInput("topic2", "Choose a 2nd topic:", 
                  choices = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17"))
      ),
    mainPanel(
      tabsetPanel(
        tabPanel("Topics Sums/Means", htmlOutput("view")),
        tabPanel("Topic Model", htmlOutput("topicmodels")),
        tabPanel("Tables", htmlOutput("view2")))
      )))

shinyApp(ui = ui, server = server)
