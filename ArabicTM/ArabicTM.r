setwd("~/Dropbox/ArabicTM")
# libraries needed

library(tm)
library(XML)
library(RCurl)
library(plyr)
library(lda)
library(LDAvis)

## User settings:
K <- 37
G <- 5000
alpha <- 0.02
eta <- 0.02
seed <- 12
terms_shown <- 40
swLatin <- TRUE
swEnglish <- FALSE
swGreek <- FALSE
swAdditional <- TRUE
language <- "Arabic" # (Persian, Greek, Latin)

# requestURN <- "urn:cts:latinLit:phi0448.phi001"
requestURN <- "urn:cts:greekLit:tlg0003.tlg001"
capabilities_URL <- "http://www.perseus.tufts.edu/hopper/CTS?request=GetCapabilities"
baseURL <- "http://www.perseus.tufts.edu/hopper/CTS?request=GetPassage&urn="
reffURL <- "http://www.perseus.tufts.edu/hopper/CTS?request=GetValidReff&urn="
morpheusURL1 <- "http://www.perseus.tufts.edu/hopper/xmlmorph?lang=lat&lookup="
morpheusURL2 <- "https://services.perseids.org/bsp/morphologyservice/analysis/word?word="

searchterms <- ""

# Read corpus

base_corpus <- read.table("Shamela_0035100.csv", sep="\t", header=FALSE)
research_corpus <- as.character(base_corpus[["V2"]])
output_names <- as.character(base_corpus[["V1"]])

## reduce sample

research_corpus <- research_corpus[1:10]

# pre-processing goes here

# tokenize on space and output as a list:
doc.list <- strsplit(research_corpus, "[[:space:]]+")

# produce dictionary for stemming:

t1 <- Sys.time()

all_words <- unlist(doc.list)
corpus_words <- unique(all_words)
corpus_words <- sort(corpus_words)

#function for stemming goes here

#stemming

parsing2 <- function(x){
  URL <- paste(morpheusURL2, x, "&lang=ara&engine=aramorph", sep = "")
  message("Accessing ", URL)
  
  XMLpassage <-function(xdata){
    miner <- function(x){
      xname <- xmlName(x)
      xattrs <- xmlAttrs(x)
      c(sapply(xmlChildren(x), xmlValue), name = xname, xattrs)}
    result <- xmlParse(xdata)
    temp.df <- as.data.frame(t(xpathSApply(result, "//*/hdwd", miner)), stringsAsFactors = FALSE)
    as.vector(temp.df[['text']])}
  
  URLcontent <- tryCatch({
    getURLContent(URL)}, 
    error = function(err)
    {message(x, " -query caused server error. Return original value.")
     content <- "ServerError"
     return(content)})
  if (URLcontent == "ServerError") {lemma <- x
                                    return(lemma)}
  
  lemma <- if (is.null(XMLpassage(URLcontent)) == TRUE) {
    lemma <- x
    return(lemma)}
  else {tryCatch({XMLpassage(URLcontent)},
                 error = function(err) {
                   message(x, " not found. Return original value.")
                   lemma <- "NotFound1"
                   return(lemma)})}
  
  lemma <- gsub("[0-9]", "", lemma)
  lemma <- tolower(lemma)
  lemma <- unique(lemma)
  # lemma <- paste(lemma, sep="", collapse="_")
  if (nchar(lemma) == 0) lemma <- x
  message(x, " is ", lemma)
  return(lemma)}

## translating

translating <- function(x){
  URL <- paste(morpheusURL2, x, "&lang=ara&engine=aramorph", sep = "")
  message("Accessing ", URL)
  
  XMLpassage <-function(xdata){
    miner <- function(x){
      xname <- xmlName(x)
      xattrs <- xmlAttrs(x)
      c(sapply(xmlChildren(x), xmlValue), name = xname, xattrs)}
    result <- xmlParse(xdata)
    temp.df <- as.data.frame(t(xpathSApply(result, "//*/mean", miner)), stringsAsFactors = FALSE)
    as.vector(temp.df[['text']])}
  
  URLcontent <- tryCatch({
    getURLContent(URL)}, 
    error = function(err)
    {message(x, " -query caused server error. Return original value.")
     content <- "ServerError"
     return(content)})
  if (URLcontent == "ServerError") {lemma <- x
                                    return(lemma)}
  
  lemma <- if (is.null(XMLpassage(URLcontent)) == TRUE) {
    lemma <- x
    return(lemma)}
  else {tryCatch({XMLpassage(URLcontent)},
                 error = function(err) {
                   message(x, " not found. Return original value.")
                   lemma <- "NotFound1"
                   return(lemma)})}
  
  lemma <- gsub("[0-9]", "", lemma)
  lemma <- tolower(lemma)
  lemma <- unique(lemma)
  # lemma <- paste(lemma, sep="", collapse="_")
  if (nchar(lemma) == 0) lemma <- x
  message(x, " is ", lemma)
  return(lemma)}

## lemmatiser function

lemmatiser <- function(x){
  lemmatised <- stem_dictionary[[x]]
  return(lemmatised)}

## stemming
t1 <- Sys.time()
stem_dictionary <- sapply(corpus_words, parsing2)

NumberOfForms <- max(unique(sapply(stem_dictionary, length)))
number_lemmata <- sapply(stem_dictionary, length)

t2 <- Sys.time()
stemming_time <- t2 - t1

## correcting

t1 <- Sys.time()

temp <- strsplit(research_corpus, " ")
temp_correct <- list()
for (i in 1:length(temp)) {
  temp_correct[[i]] <- sapply(temp[[i]], lemmatiser) 
}
NumberOccurrences <- table(unlist(temp_correct))

choose_lemma <- function(x){
  lemma <- names(which(NumberOccurrences[x]==max(NumberOccurrences[x])))
  if (length(lemma)==1)
    return(lemma)}

corrected_corpus <- list()
for (n in 1:length(temp_correct)) {
  temp_corrected <- list()
  counter <- n
  for (i in 1:length(temp_correct[[counter]])) {
    temp_corrected[[i]] <- choose_lemma(temp_correct[[counter]][[i]])  
  }  
  corrected_corpus[[n]] <- temp_corrected
}

for (i in 1:length(corrected_corpus)) {
  corrected_corpus[[i]] <- paste(unlist(corrected_corpus[[i]]), collapse=" ")
}

research_corpus <- unlist(corrected_corpus)

doc.list <- strsplit(research_corpus, "[[:space:]]+")
t2 <- Sys.time()
correcting_time <- t2 - t1

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# determing stopwords

stop_words <- as.data.frame(term.table)
stop_words <- row.names(as.data.frame(stop_words[1:30,]))

# remove terms that are stop words or occur fewer than "occurenses" times:
occurences <- 3
del <- names(term.table) %in% stop_words | term.table < occurences
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

# Fit the model:
set.seed(seed)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
modelling_time <- t2 - t1

# Visualize
theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))



research_corpusAbstracts <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)
			  

# create the JSON object to feed the visualization:
json <- createJSON(phi = research_corpusAbstracts$phi, 
                   theta = research_corpusAbstracts$theta, 
                   doc.length = research_corpusAbstracts$doc.length, 
                   vocab = research_corpusAbstracts$vocab, 
                   term.frequency = research_corpusAbstracts$term.frequency,
                   R=terms_shown)
			 
#Visulise and start browser
serVis(json, out.dir = 'Arabic_vis', open.browser = FALSE)

# get the tables

dir.create("Arabic_tab")

#names(head(sort(phi.frame[,1], decreasing = TRUE)))

# get topic-term distributions and export as csv
phi.frame <- t(data.frame(phi))
colnames(phi.frame) <- paste("topic", as.character(1:K), sep="")
colnames(phi.frame)[1] <- "term"
write.table(phi.frame, file = 'Arabic_tab/phi.csv', append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE)

# get document-topic distributions and export as csv
theta.frame <- data.frame(theta)
colnames(theta.frame) <- paste("topic", as.character(1:K-1), sep="")
colnames(theta.frame)[1] <- "identifier"
rownames(theta.frame) <- output_names
write.table(theta.frame, file = 'Arabic_tab/theta.csv', append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE)
