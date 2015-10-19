library(shiny)
library(LDAvis)
library(XML)
library(RCurl)
library(plyr)
library(tm)
library(lda)

per_page <- 100

server <- function(input, output) function(input, output) {
  
  tm_visualisation <- reactive({
    if(input$collection == 1) {
      
      # Build URL link  
      
      URL <- paste("http://api.digitalnz.org/v3/records.xml?api_key=LUq9-soDzWWhShuy3XhU&text=", input$search_text,"&and[category][]=Images&and[collection][]=New+Zealand+Cartoon+Archive&fields=id,shelf_location,source_url,title,description,collection&and[year]=", input$date,sep = "")
      if(input$date == "") {
        URL <- paste("http://api.digitalnz.org/v3/records.xml?api_key=LUq9-soDzWWhShuy3XhU&text=", input$search_text, "&and[category][]=Images&and[collection][]=New+Zealand+Cartoon+Archive&fields=id,shelf_location,source_url,title,description,collection", sep = "")
      }
      
      # Determine the number of API requests
      
      URLTestcontent <- getURLContent(URL)
      doc <- xmlTreeParse(URLTestcontent, getDTD = F)
      r <- xmlRoot(doc)
      requests_needed <-  ceiling(as.numeric(xmlValue(r[[1]]))/per_page)
      
      # Do all necessary API requests than return unique result
      
      output_list <- list()
      
      for (i in 1:requests_needed ) {
        message("Request", i)
        URL <- paste(URL, "&per_page=", as.character(per_page), "&page=", as.character(i), sep = "")
        
        # &and[year]= , year
        
        URLcontent <- getURLContent(URL)
        # Parse the XML and extract needed information. 
        doc <- xmlTreeParse(URLcontent, getDTD = F)
        rootnode <- xmlRoot(doc)
        results <- xmlChildren(rootnode)[2][[1]]
        catalogue_temp <- xmlSApply(results, function(x) xmlSApply(x, xmlValue))
        catalogue_temp.df <- t(data.frame(catalogue_temp))
        output_list[[i]] <- catalogue_temp.df
      }
      
      catalogue <- do.call("rbind",output_list) #combine all vectors into a matrix
      catalogue <- unique(catalogue) # returns the unique rows of catalogue.
      catalogue[, "description"] <- sapply(strsplit(catalogue[, "description"], split='Quantity:', fixed=TRUE), function(x) (x[1]))
      
      # Save API result in CSV file
      
      dir.create("www/catalogue")
      
      write.table(catalogue, file = 'www/catalogue/catalogue.csv', append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE)
      
      # read in some stopwords:
      stop_words1 <- stopwords("SMART")
      
      stop_words2 <- c("new", "zealand", "context", "shows", "cartoon", "reads", "text", "depicts", "labelled", "man", "woman", "year", "frame", "frames", "socalled", "day", "week", "month", "reading", "representing", "refers")
      
      stop_words3 <- c("acknowledges", "addresses", "advises", "advocates", "aims", "album", "alerts", "analyses", "announces", "answers", "anticipates", "argues", "asks", "asserts", "assesses", "backgrounds", "biographical", "book", "briefly", "celebrates", "chats", "chronicles", "comments", "compares", "concerns", "considers", "continues", "contributes", "covers", "deals", "describes", "details", "discusses", "documents", "draws", "emphasises", "evaluates", "examines", "explains", "explores", "features", "focuses", "follows", "gives", "highlights", "identifies", "illustrates", "informs", "interviews", "introduces", "investigates", "ka", "listens", "lists", "looks", "meets", "notes", "obituary.", "observes", "offers", "outlines", "overviews", "pays", "poem.", "poems", "points", "ponders", "presents", "previews", "profiles", "provides", "questions", "recalls", "recaps", "recognises", "recommends", "recounts", "refers", "reflects", "relates", "relays", "reports", "reveals", "reviews", "revisits", "sets", "shares", "short", "showcases", "speaks", "spotlights", "suggests", "summarises", "sums", "takes", "talks", "tells", "tests", "the", "three", "traces", "two", "updates", "uses", "visits", "warns", "witnesses")
      
      user_stopwords <- unlist(strsplit(input$add_stopwords, " "))
      
      # Decide which set of stopwords
      
      stop_words <- c(stop_words1, stop_words2, user_stopwords)
      # stop_words <- c(stop_words1, stop_words3) # choose for uncertain INNZ/Cartoon data
      # stop_words <- c(stop_words1, stop_words2, stop_words3) # choose for EQ-INNZ/Cartoon data
      
      # Build research_corpus from API result
      
      research_corpus <- catalogue[,"description"]
      # research_corpus <- sapply(strsplit(research_corpus, split='Quantity:', fixed=TRUE), function(x) (x[1]))
      research_corpus <- factor(research_corpus)
      output_names <- factor (catalogue[,"shelf-location"])
      
      # research_corpus <- read.csv("CartoonSep2013/CartoonSep2013.csv")$description
      # output_names <- read.csv("CartoonSep2013/CartoonSep2013.csv")$identifier 
      
      # pre-processing:
      
      research_corpus <- tolower(research_corpus)  # force to lowercase
      research_corpus <- gsub("'", " ", research_corpus)  # remove apostrophes
      research_corpus <- gsub("-", "", research_corpus)  # remove hyphens, create composita
      
      # build better composita (optional)
      
      research_corpus <- gsub("all black", "allblack", research_corpus)
      research_corpus <- gsub("dalai lama", "dalailama", research_corpus)
      research_corpus <- gsub("national list", "nationallist", research_corpus)
      research_corpus <- gsub("labour list", "labourlist", research_corpus)
      research_corpus <- gsub("finance minister", "financeminister", research_corpus)
      research_corpus <- gsub("recovery act", "recoveryact", research_corpus)
      research_corpus <- gsub("recovery minister", "recoveryminister", research_corpus)
      research_corpus <- gsub("commonwealth game", "commonwealthgame", research_corpus)
      research_corpus <- gsub("red zone", "redzone", research_corpus)
      research_corpus <- gsub("red-zone", "redzone", research_corpus)
      research_corpus <- gsub("john key", "johnkey", research_corpus)
      research_corpus <- gsub("johnkey", "key", research_corpus)
      research_corpus <- gsub("gerry brownlee", "gerrybrownlee", research_corpus)
      research_corpus <- gsub("gerrybrownlee", "brownlee", research_corpus)
      research_corpus <- gsub("bob parker", "parker", research_corpus)
      research_corpus <- gsub("world cup", "worldcup", research_corpus)
      research_corpus <- gsub("rugby worldcup", "rugbyworldcup", research_corpus)
      research_corpus <- gsub("bill english", "billenglish", research_corpus)
      research_corpus <- gsub("portaloos", "portaloo", research_corpus)
      research_corpus <- gsub("national party", "natparty", research_corpus)
      research_corpus <- gsub("labour party", "labourparty", research_corpus)
      research_corpus <- gsub("chch", "christchurch", research_corpus)
      research_corpus <- gsub("prime minister", "primeminister", research_corpus)
      research_corpus <- gsub("pm key", "primeminister key", research_corpus)
      research_corpus <- gsub("south island", "southisland", research_corpus)
      research_corpus <- gsub("north island", "northisland", research_corpus)
      research_corpus <- gsub("roger sutton", "rogersutton", research_corpus)
      research_corpus <- gsub("ami insurance", "amiinsurance", research_corpus)
      research_corpus <- gsub("jim anderton", "anderton", research_corpus)
      research_corpus <- gsub("canterbury earthquake recovery authority", "cera", research_corpus)
      research_corpus <- gsub("christchurch city council", "citycouncil", research_corpus)
      research_corpus <- gsub("lianne dalziel", "dalziel", research_corpus)
      research_corpus <- gsub("phil goff", "goff", research_corpus)
      research_corpus <- gsub("hone harawira", "harawira", research_corpus)
      research_corpus <- gsub("rodney hide", "rodneyhide", research_corpus)
      research_corpus <- gsub("tony marryatt", "marryatt", research_corpus)
      research_corpus <- gsub("earthquake commission", "eqcommission", research_corpus)
      research_corpus <- gsub("pike river", "pikeriver", research_corpus)
      research_corpus <- gsub("reserve bank", "reservebank", research_corpus)
      research_corpus <- gsub("ken ring", "kenring", research_corpus)
      research_corpus <- gsub("south canterbury finance", "southcanterburyfinance", research_corpus)
      research_corpus <- gsub("infrastructure rebuild team", "infrastructurerebuildteam", research_corpus)
      
      # Continiue pre-Processing
      research_corpus <- gsub("[[:punct:]]", " ", research_corpus)  # replace punctuation with space
      research_corpus <- gsub("[[:cntrl:]]", " ", research_corpus)  # replace control characters with space
      research_corpus <- gsub("^[[:space:]]+", "", research_corpus) # remove whitespace at beginning of documents
      research_corpus <- gsub("[[:space:]]+$", "", research_corpus) # remove whitespace at end of documents
      research_corpus <- gsub("[0-9]", "", research_corpus) #remove numbers
      
      # tokenize on space and output as a list:
      doc.list <- strsplit(research_corpus, "[[:space:]]+")
      
      # compute the table of terms:
      term.table <- table(unlist(doc.list))
      term.table <- sort(term.table, decreasing = TRUE)
      
      # remove terms that are stop words or occur fewer than 3 times:
      del <- names(term.table) %in% stop_words | term.table < 3
      term.table <- term.table[!del]
      vocab <- names(term.table)
      
      # now put the documents into the format required by the lda package:
      get.terms <- function(x) {
        index <- match(x, vocab)
        index <- index[!is.na(index)]
        rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
      }
      documents <- lapply(doc.list, get.terms)
      
      # Compute some statistics related to the data set:
      D <- length(documents)  # number of documents (2,000)
      W <- length(vocab)  # number of terms in the vocab (14,568)
      doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
      N <- sum(doc.length)  # total number of tokens in the data (546,827)
      term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]
      
      # MCMC and model tuning parameters:
      K <- input$number_topics
      G <- input$iterations
      alpha <- 0.02
      eta <- 0.02
      
      # Fit the model:
      set.seed(357)
      t1 <- Sys.time()
      fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                         num.iterations = G, alpha = alpha, 
                                         eta = eta, initial = NULL, burnin = 0,
                                         compute.log.likelihood = TRUE)
      t2 <- Sys.time()
      t2 - t1
      
      # Build Visualisation
      theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
      phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))
      
      
      
      research_corpusAbstracts <- list(phi = phi,
                                       theta = theta,
                                       doc.length = doc.length,
                                       vocab = vocab,
                                       term.frequency = term.frequency)
      
      
      # get the tables
      
      dir.create("www/tables")
      
      #names(head(sort(phi.frame[,1], decreasing = TRUE)))
      
      # get document-topic distributions and export as csv
      theta.frame <- data.frame(theta)
      colnames(theta.frame) <- paste("topic", as.character(1:K-1), sep="")
      colnames(theta.frame)[1] <- "identifier"
      rownames(theta.frame) <- output_names
      write.table(theta.frame, file = 'www/tables/theta.csv', append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE)
      
      # get topic-term distributions and export as csv
      phi.frame <- t(data.frame(phi))
      colnames(phi.frame) <- paste("topic", as.character(1:K), sep="")
      colnames(phi.frame)[1] <- "term"
      write.table(phi.frame, file = 'www/tables/phi.csv', append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE)
      
      # create the JSON object to feed the visualization:
      
      json <- createJSON(phi = research_corpusAbstracts$phi, 
                         theta = research_corpusAbstracts$theta, 
                         doc.length = research_corpusAbstracts$doc.length, 
                         vocab = research_corpusAbstracts$vocab, 
                         term.frequency = research_corpusAbstracts$term.frequency,
                         R = input$number_terms)
      serVis(json, out.dir = 'www/vis', open.browser = FALSE)
      
    }
  }
  )
  
  output$topicmodels <- renderUI({  
    getPage<-function() {
      return(tags$iframe(src = "/vis/index.html"
                         , style="width:100%;",  frameborder="0"
                         ,id="iframe"
                         , height = "700px"))
    }
    tm_visualisation()
    getPage()
    
  })
  
  output$catalogue <- renderTable({
    counties <- read.csv("tmApp/www/catalogue/catalogue.csv")
  })
  
}

ui <- fluidPage(
  
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

shinyApp(ui = ui, server = server)