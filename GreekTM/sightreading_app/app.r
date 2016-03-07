## setwd, modify according to your needs

load("./www/sightreading.RData", envir=.GlobalEnv)
## libraries needed

library(RColorBrewer)
library(d3heatmap)
library(markdown)

### functions

sightreading <- function(x,y) {
  i <- which(colnames(test_matrix) == x)
  sight_names <- names(head(sort(test_matrix[names(which(similarSentences[,2] == i)), i]), n=y))
  return(sight_names)
}

find_start <- function(x) {
  start_slice <- which(colnames(test_matrix) == x) - 1
  if (start_slice == 0) {return(which(colnames(test_matrix) == x))}
  return(start_slice)
}

find_end <- function(x) {
  end_slice <- which(colnames(test_matrix) == x) + 1
  if (end_slice == length(colnames(test_matrix))+1) {return(which(colnames(test_matrix) == x))}
  return(end_slice)
}

neighbours <- function(x) {
  base <- which(colnames(test_matrix) == x)
  if (base + 1 == length(colnames(test_matrix))+1) {
    right_neighbour <- base
  }
  else {
    right_neighbour <- base + 1
  }
  if (base - 1 == 0) {
    left_neighbour <- base
  }
  else {
    left_neighbour <- base - 1
  }
  return(c(left_neighbour, base, right_neighbour))
}

visualise_similarity <- function(x, y) {
  sightreadings <- sightreading(x, y)
  passages <- as.vector(unname(sapply(sightreadings, neighbours)))
  matrix_slice <- test_matrix[passages, find_start(x):find_end(x)]
  matrix_slice <- matrix_slice[order(rownames(matrix_slice)),]
  visualisation <- d3heatmap(matrix_slice, dendrogram = "none", color = "Blues", anim_duration = 0, xaxis_font_size = 12, yaxis_font_size = 12)
  return(visualisation)
}

extract_sentences <- function(x) {
  similar_sentences.list <- list(corpus[which(corpus[,1] == x), 1], corpus[which(corpus[,1] == x), 2], corpus_parsed[which(corpus_parsed[,1] == x), 2])
  return(similar_sentences.list)
}

sight_passages <- function(x, y) {
  filenamer <- unname(unlist(strsplit(x, "tlg001.perseus-grc1:", fixed = TRUE))[2])
  filename <- gsub(".", "_", filenamer, fixed=TRUE)
  similar_sentences <- sightreading(x, y)
  similar_sentences.df <- sapply(similar_sentences, extract_sentences)
  similar_sentences.df <- t(similar_sentences.df)
  similar_sentences.df <- as.data.frame(similar_sentences.df)
  colnames(similar_sentences.df) <- c(x, corpus[which(corpus == x), 2], corpus_parsed[which(corpus_parsed == x), 2])
  for (i in 1:length(similar_sentences.df[,3])){
    comparison <- unlist(strsplit(as.character(similar_sentences.df[i,3]), "[[:space:]]+"))
    similar_sentences.df[i,4] <- as.character(paste(comparison %in% base_parsed, sep="", collapse = " "))
    position_parsed <- which(unlist(strsplit(as.character(similar_sentences.df[i,4]), "[[:space:]]+")) == TRUE)
    position_notparsed <- which(unlist(strsplit(as.character(similar_sentences.df[i,4]), "[[:space:]]+")) == FALSE)
    text_vector <- unlist(strsplit(paste(similar_sentences.df[i,2], sep = "", collapse = " "), "[[:space:]]+"))
    text_vector[position_parsed] <- paste("**", text_vector[position_parsed], "**", sep="")
    text_vector[position_notparsed] <- paste("[", text_vector[position_notparsed], "]", "(http://www.perseus.tufts.edu/hopper/morph?l=", text_vector[position_notparsed], "&la=greek#lexicon)", sep="")
    similar_sentences.df[i,5] <-paste(text_vector, sep="", collapse=" ") 
    similar_sentences.df[i,6] <- unname(table(comparison %in% base_parsed)["TRUE"])/(unname(table(comparison %in% base_parsed)["TRUE"])+unname(table(comparison %in% base_parsed)["FALSE"]))*100
  }
  
  colnames(similar_sentences.df)[4] <- "Word from base sentence?"
  colnames(similar_sentences.df)[5] <- "Position known word"
  colnames(similar_sentences.df)[6] <- "Percent known"
  similar_sentences.frame <- data.frame(unname(unlist(similar_sentences.df[,1])),
                                        unname(unlist(similar_sentences.df[,5])),
                                        unname(unlist(similar_sentences.df[,6])))
  colnames(similar_sentences.frame) <- c(colnames(similar_sentences.df[1]), colnames(similar_sentences.df[2]), colnames(similar_sentences.df[6]))
  filepath <- paste('www/', filename, '.md', sep="")
  write.table(similar_sentences.frame, file = filepath, append = FALSE, quote = FALSE, sep = "<br>\n", eol = "<br><br>\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
}

# user-input

base <- c("urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:1.1.1", 
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:1.1.2", 
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:1.1.3", 
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:1.2.1",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:1.2.2",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:1.2.3",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:1.2.4",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:1.2.5",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:1.2.6",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:1.22.1",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:1.22.2",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:1.22.3",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:1.22.4",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:2.48.1",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:2.48.2",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:2.48.3",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:2.49.1",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:2.49.2",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:2.49.3",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:2.49.4",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:2.49.5",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:2.49.6",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:2.49.7",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:2.49.8",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:2.50.1",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:2.50.2",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:2.51.1",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:2.51.2",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:2.51.3",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:2.51.4",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:2.51.5",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:2.51.6",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:2.52.1",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:2.52.2",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:2.52.3",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:2.52.4",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:2.53.1",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:2.53.2",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:2.53.3",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:2.53.4",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:2.54.1",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:2.54.2",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:2.54.3",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:2.54.4",
          "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:2.54.5"
          )
similar_to <- "urn:cts:greekLit:tlg0003.tlg001.perseus-grc1:1.22.1"
number_of_examples <- 5
number_for_viz <- 1


# calculating additional variables

smallest_notZeroNA <- apply(na.omit(test_matrix), 1, FUN = function(x) {min(x[x > 0])})
similarSentences <- which(test_matrix <= max(smallest_notZeroNA) & test_matrix > 0 , arr.ind=T )

base_parsed <- vector()
for (i in base){
  base_parsed[[i]] <- unname(corpus_parsed[which(corpus_parsed == i),][2])
}
base_parsed <- paste(unname(base_parsed), sep="", collapse = " ")
base_parsed <- unique(unlist(strsplit(base_parsed, "[[:space:]]+")))


# visualisation$save('similarity.html', standalone = TRUE)

server <-function(input, output) {
  
  sight_reader <- reactive({  
    sight_passages(similar_to, number_of_examples)
  }
  )
  
  output$topicmodels <- renderUI({  
    similar_to <- input$sightreading
    number_of_examples <- input$sightreadingnumber
    sight_passages(similar_to, number_of_examples)
    getPage<-function() {
      return(tags$iframe(src = "./vis/index.html"
                         , style="width:100%;",  frameborder="0"
                         ,id="iframe"
                         , height = "700px"))
    }
    getPage()
    
  })
  
  output$visualisation <- renderD3heatmap({  
    similar_to <- input$sightreading
    number_of_examples <- input$sightreadingnumber
    sight_passages(similar_to, number_of_examples)
    number_for_viz <- input$viznumber
    visualise_similarity(similar_to, number_for_viz)
  })
  
  output$markdownfile <- renderUI({  
    similar_to <- input$sightreading
    number_of_examples <- input$sightreadingnumber
    sight_passages(similar_to, number_of_examples)
    filenamer <- unname(unlist(strsplit(similar_to, "tlg001.perseus-grc1:", fixed = TRUE))[2])
    filename <- gsub(".", "_", filenamer, fixed=TRUE)
    markdownfile <- paste("./www/", filename, ".md", sep="", collapse="")
    includeMarkdown(markdownfile)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      similar_to <- input$sightreading
      number_of_examples <- input$sightreadingnumber
      filenamer <- unname(unlist(strsplit(similar_to, "tlg001.perseus-grc1:", fixed = TRUE))[2])
      filename <- gsub(".", "_", filenamer, fixed=TRUE)
      markdownfile <- paste(filename, ".md", sep="", collapse="")
      paste(markdownfile, sep='') },
    content = function(file) {
      similar_to <- input$sightreading
      number_of_examples <- input$sightreadingnumber
      sight_passages(similar_to, number_of_examples)
      filenamer <- unname(unlist(strsplit(similar_to, "tlg001.perseus-grc1:", fixed = TRUE))[2])
      filename <- gsub(".", "_", filenamer, fixed=TRUE)
      markdownfile <- paste("./www/", filename, ".md", sep="", collapse="")
      save_file <- readLines(markdownfile)
      write.table(save_file, file)
    }
  )
  
}


ui <- fluidPage(
  
  titlePanel("Sight Reading Finder based on Topic Modelling Thucydides (Tufts, GRK 0103)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("sightreading", label = "Passage", choices = base),
      sliderInput("sightreadingnumber", label = "Number of Sentences", min = 1, max = 10, value = 5),
      sliderInput("viznumber", label = "Number of Sentences for Similarity Viz", min = 1, max = 5, value = 1),
      submitButton("Submit"),
      downloadButton('downloadData', 'Download')
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Topics in Thucydides", htmlOutput("topicmodels")),
        tabPanel("Sight Readings", htmlOutput("markdownfile")),
        tabPanel("Visualising Similarity", d3heatmapOutput("visualisation"))
      ))))

shinyApp(ui = ui, server = server)

