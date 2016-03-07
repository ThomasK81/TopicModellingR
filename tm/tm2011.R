setwd("~/OneDrive/GithubProjects/TopicModellingR/tm")

library(shiny)
library(LDAvis)
library(XML)
library(RCurl)
library(plyr)
library(tm)
library(lda)
library(ngram)

per_page <- 100

  
# Build URL link  
      
URL <- paste("http://api.digitalnz.org/v3/records.xml?api_key=LUq9-soDzWWhShuy3XhU", "","&and[category][]=Images&and[collection][]=New+Zealand+Cartoon+Archive&fields=id,shelf_location,source_url,title,description,collection&and[year]=2011", "",sep = "")
      
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
stop_words <- stopwords("SMART")
      
# Build research_corpus from API result
      
research_corpus <- catalogue[,"description"]
research_corpus <- factor(research_corpus)
output_names <- factor (catalogue[,"shelf-location"])

# pre-processing:
      
research_corpus <- tolower(research_corpus)  # force to lowercase
research_corpus <- gsub("'", " ", research_corpus)  # remove apostrophes
research_corpus <- gsub("-", "", research_corpus)  # remove hyphens, create composita
research_corpus <- gsub("[[:punct:]]", " ", research_corpus)  # replace punctuation with space
research_corpus <- gsub("[[:cntrl:]]", " ", research_corpus)  # replace control characters with space
research_corpus <- gsub("^[[:space:]]+", "", research_corpus) # remove whitespace at beginning of documents
research_corpus <- gsub("[[:space:]]+$", "", research_corpus) # remove whitespace at end of documents
research_corpus <- gsub("[0-9]", "", research_corpus) #remove numbers
research_corpus <- gsub("^ *|(?<= ) | *$", "", research_corpus, perl = TRUE) # replace multiple spaces with single space

# prepare ngram_corpus

first_element <- function(x){
  first_element <- head(x, n=1)
  return(first_element)}

last_element <- function(x){
  last_element <- tail(x, n=1)
  return(last_element)}
  
ngram_corpus <- paste(research_corpus, sep="", collapse=" ")
ngram_corpus <- strsplit(ngram_corpus, "[[:space:]]+")
del <- unlist(ngram_corpus) %in% stop_words
ngram_corpus2 <- unlist(ngram_corpus)[!del]
ngram_corpus2 <- paste(unlist(ngram_corpus2), sep="", collapse=" ")

ng <- ngram(ngram_corpus2, n=2)
ngrams <- get.ngrams(ng)
ngram.df <- data.frame(ngrams = ngrams)
ngram.df$vector <- lapply(strsplit(as.character(ngram.df$ngrams), "[[:space:]]+"), "[")

ng3 <- ngram(ngram_corpus2, n=3)
ngrams3 <- get.ngrams(ng3)
ngram3.df <- data.frame(ngrams = ngrams3)
ngram3.df$vector <- lapply(strsplit(as.character(ngram.df$ngrams), "[[:space:]]+"), "[")

ng4 <- ngram(ngram_corpus2, n=4)
ngrams4 <- get.ngrams(ng4)
ngram4.df <- data.frame(ngrams = ngrams4)
ngram4.df$vector <- lapply(strsplit(as.character(ngram.df$ngrams), "[[:space:]]+"), "[")

# tokenize on space and output as a list:
doc.list <- strsplit(research_corpus, "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)
      
# remove terms that are stop words 
del <- names(term.table) %in% stop_words
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
D <- length(documents)  # number of documents
W <- length(vocab)  # number of terms in the vocab
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document
N <- sum(doc.length)  # total number of tokens in the data
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus
      
# MCMC and model tuning parameters:
K <- 15
G <- 500
alpha <- 0.02
eta <- 0.02
      
# Fit the model:
set.seed(173)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                         num.iterations = G, alpha = alpha, 
                                         eta = eta, initial = NULL, burnin = 0,
                                         compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1

# find 2-gram compounds

phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))
phi.t.df <- as.data.frame(t(phi))

is_compound <- function(x) {
  check <- all.equal(phi.t.df[first_element(unlist(x)),], phi.t.df[last_element(unlist(x)),])
  if(length(check) == 1) {result <- 0
                    return(result)}
  result <- mean(as.numeric(sub(".*?difference: (.*?)", "\\1", check)[2:length(check)])) 
  return(result)
}

is_similar <- function(x) {
  checker <- x
  check_function <- function(y) {
    checks_needed <- row.names(theta.frame) [! row.names(theta.frame) %in% checker]
    result <- 
    return(result)
  }
    check <- all.equal(theta.frame[row.names(theta.frame)[1],], theta.frame[row.names(theta.frame)[12],])
  result <- mean(as.numeric(sub(".*?difference: (.*?)", "\\1", check)[2:length(check)]))}
  return(result)
}

ngram.df$compound_score <- lapply(ngram.df$vector, is_compound)
ngram.df$compound_score <- unlist(ngram.df$compound_score)
ngram.df.ordered <- ngram.df[ order(ngram.df[,3], na.last=FALSE), ]

compounds <- ngram.df.ordered[ngram.df.ordered$compound_score < 1, ]
replace_compounds <- function(x) {
  if(first_element(x) == last_element(x)){
    research_corpus <- research_corpus
    return(research_corpus)}
  sequence <- paste(x, sep = "", collapse = " ")
  replacement <- paste(x, sep = "", collapse = "_")
  research_corpus <- gsub(sequence, replacement, research_corpus)
  return(research_corpus)
}

research_corpus <- apply(compounds$vector, replace_compounds)

# Build Visualisation

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))      
      
      
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
                         R = 30)
      serVis(json, out.dir = 'www/vis', open.browser = FALSE)
   
  