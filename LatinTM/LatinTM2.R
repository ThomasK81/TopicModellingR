## setwd

setwd("~/OneDrive/LatinTM")

## libraries needed

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
language <- "Latin" # (Persian, Arabic, Greek)


requestURN <- "urn:cts:latinLit:phi0448.phi001"
# requestURN <- "urn:cts:greekLit:tlg0003.tlg001"
capabilities_URL <- "http://www.perseus.tufts.edu/hopper/CTS?request=GetCapabilities"
baseURL <- "http://www.perseus.tufts.edu/hopper/CTS?request=GetPassage&urn="
reffURL <- "http://www.perseus.tufts.edu/hopper/CTS?request=GetValidReff&urn="
morpheusURL1 <- "http://www.perseus.tufts.edu/hopper/xmlmorph?lang=lat&lookup="
morpheusURL2 <- "https://services.perseids.org/bsp/morphologyservice/analysis/word?word="

searchterms <- ""

## read in some stopwords:

stopwords_english <- stopwords("SMART")
stopwords_latin <- c("ab", "ac", "ad", "adhic", "aliqui", "aliquis", "an", "ante", "apud", "at", "atque", "aut", "autem", "cum", "cur", "de", "deinde", "dum", "ego", "enim", "ergo", "es", "est", "et", "etiam", "etsi", "ex", "fio", "haud", "hic", "iam", "idem", "igitur", "ille", "in", "infra", "inter", "interim", "ipse", "is", "ita", "magis", "modo", "mox", "nam", "ne", "nec", "necque", "neque", "nisi", "non", "nos", "o", "ob", "per", "possum", "post", "pro", "quae", "quam", "quare", "qui", "quia", "quicumque", "quidem", "quilibet", "quis", "quisnam", "quisquam", "quisque", "quisquis", "quo", "quoniam", "sed", "si", "sic", "sive", "sub", "sui", "sum", "super", "suus", "tam", "tamen", "trans", "tu", "tum", "ubi", "uel", "uero", "ut", "t", "cos2", "coepio", "sum", "edo")
stopwords_greek <- c("μή", "ἑαυτοῦ", "ἄν", "ἀλλ’", "ἀλλά", "ἄλλοσ", "ἀπό", "ἄρα", "αὐτόσ", "δ’", "δέ", "δή", "διά", "δαί", "δαίσ", "ἔτι", "ἐγώ", "ἐκ", "ἐμόσ", "ἐν", "ἐπί", "εἰ", "εἰμί", "εἴμι", "εἰσ", "γάρ", "γε", "γα^", "ἡ", "ἤ", "καί", "κατά", "μέν", "μετά", "μή", "ὁ", "ὅδε", "ὅσ", "ὅστισ", "ὅτι", "οὕτωσ", "οὗτοσ", "οὔτε", "οὖν", "οὐδείσ", "οἱ", "οὐ", "οὐδέ", "οὐκ", "περί", "πρόσ", "σύ", "σύν", "τά", "τε", "τήν", "τῆσ", "τῇ", "τι", "τί", "τισ", "τίσ", "τό", "τοί", "τοιοῦτοσ", "τόν", "τούσ", "τοῦ", "τῶν", "τῷ", "ὑμόσ", "ὑπέρ", "ὑπό", "ὡσ", "ὦ", "ὥστε", "ἐάν", "παρά", "σόσ")

## Decide which set of stopwords

stop_words <- stopwords_latin

## See which authors are in CTS repository

URL <- capabilities_URL

URLcontent <- getURLContent(URL)
XMLpassage <-function(xdata){
  dumFun <- function(x){
    xname <- xmlName(x)
    xattrs <- xmlAttrs(x)
    c(sapply(xmlChildren(x), xmlValue), name = xname, xattrs)}
  dum <- xmlParse(xdata)
  as.data.frame(t(xpathSApply(dum, "//*/work", dumFun)), stringsAsFactors = FALSE)}
output_list <- XMLpassage(URLcontent)

### Import corpus from CTS repository

## Fetch Reffs for CTS Repository

t1 <- Sys.time()

message("Retrieve Reffs for ", requestURN)
URL <- paste(reffURL, requestURN, sep = "")
URLcontent <- getURLContent(URL)
reffs <- unlist(strsplit(URLcontent, split="<urn>|</urn>"))
reffs <- reffs[2:length(reffs)]
reffs <- reffs[seq(1, length(reffs), 2)]

t2 <- Sys.time()
Reff_time <- t2 - t1

## Fetch Text from CTS Repository

t1 <- Sys.time()

output_list <- list()
error_log <- list()

for (i in reffs[1:10]) {
  message("Retrieve section ", i)
  URL <- paste(baseURL, i, sep = "")
  message("Fetching ", URL)
  URLcontent <- getURLContent(URL)
  # Parse the XML and extract needed information. 
  XMLpassage <-function(xdata){
    dumFun <- function(x){
      xname <- xmlName(x)
      xattrs <- xmlAttrs(x)
      c(sapply(xmlChildren(x), xmlValue), name = xname, xattrs)}
    dum <- xmlParse(xdata)
    as.data.frame(t(xpathSApply(dum, "//*/tei:body", dumFun)), stringsAsFactors = FALSE)}
  output_list[[i]] <- tryCatch({
    XMLpassage(URLcontent)},
    error = function(err)
    {message(i, " -retrieval failed. Put in log.")
     error_log[[i]] <- i
     return("fehler")}
    )
  message("---------------------------------------")}

t2 <- Sys.time()
Fetch_time <- t2 - t1

## Build corpus

corpus <- do.call("rbind",output_list) #combine all vectors into a matrix
corpus <- unique(corpus) # returns the unique rows of catalogue.

## Save corpus to disk

write.table(corpus, file = 'corpus.csv', append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE)

## Build base for topic modelling

research_corpus <- corpus[,"div"]
research_corpus <- factor(research_corpus)
output_names <- rownames(corpus)

### pre-processing:

research_corpus <- tolower(research_corpus)  # force to lowercase
research_corpus <- gsub("'", " ", research_corpus)  # remove apostrophes
research_corpus <- gsub("-", "", research_corpus)  # remove hyphens, create composita
research_corpus <- gsub("v", "u", research_corpus) # normalise to 'u'
research_corpus <- gsub("j", "i", research_corpus) # normalise to 'i'

research_corpus <- gsub("[[:punct:]]", " ", research_corpus)  # replace punctuation with space
research_corpus <- gsub("[[:cntrl:]]", " ", research_corpus)  # replace control characters with space
research_corpus <- gsub("^[[:space:]]+", "", research_corpus) # remove whitespace at beginning of documents
research_corpus <- gsub("[[:space:]]+$", "", research_corpus) # remove whitespace at end of documents
research_corpus <- gsub("[0-9]", "", research_corpus) #remove numbers

## produce dictionary for stemming:
t1 <- Sys.time()

## tokenize on space and output as a list:
doc.list <- strsplit(research_corpus, "[[:space:]]+")

corpus_words <- unique(unlist(doc.list))
corpus_words <- sort(corpus_words)

## function for stemming

parsing2 <- function(x){
  URL <- paste(morpheusURL2, x, "&lang=lat&engine=morpheuslat", sep = "")
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

parsing <- function(x){
  URL <- paste(morpheusURL1, x, sep = "")
  message("Accessing ", URL)
  
  XMLpassage <-function(xdata){
    miner <- function(x){
      xname <- xmlName(x)
      xattrs <- xmlAttrs(x)
      c(sapply(xmlChildren(x), xmlValue), name = xname, xattrs)}
    result <- xmlParse(xdata)
    temp.df <- as.data.frame(t(xpathSApply(result, "//*/lemma", miner)), stringsAsFactors = FALSE)
    as.vector(temp.df[['text']])}
  
  URLcontent <- tryCatch({
    getURLContent(URL)}, 
    error = function(err)
    {message(x, " -query caused server error. Return original value.")
     content <- "ServerError"
     return(content)})
  if (URLcontent == "ServerError") {lemma <- parsing2(x)
                                    return(lemma)}
  
  lemma <- if (is.null(XMLpassage(URLcontent)) == TRUE) {
    lemma <- parsing2(x)
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

## correcting function

correcting <- function(x){
  
}

## stemming

stem_dictionary <- sapply(corpus_words, parsing)

NumberOfForms <- max(unique(sapply(stem_dictionary, length)))
number_lemmata <- sapply(stem_dictionary, length)

## correcting

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

### Prepare Topic-modelling 

## compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

## compute additional stop_words:
add_stop_words <- as.data.frame(term.table)
add_stop_words <- row.names(as.data.frame(add_stop_words[1:1,]))

stop_words <- c(stop_words, add_stop_words)
stop_words <- unique(stop_words)

## remove terms that are stop words or occur fewer than "occurenses" times:
occurences <- 5
del <- names(term.table) %in% stop_words | term.table < occurences
term.table <- term.table[!del]
vocab <- names(term.table)

## now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))}
documents <- lapply(doc.list, get.terms)

## Compute some statistics related to the data set:
D <- length(documents)  # number of documents
W <- length(vocab)  # number of terms in the vocab
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document
N <- sum(doc.length)  # total number of tokens in the data
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus

## Fit the model:
set.seed(123)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
modelling_time <- t2 - t1

## Visualize
theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

research_corpusAbstracts <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)
			  
## create the JSON object to feed the visualization:
json <- createJSON(phi = research_corpusAbstracts$phi, 
                   theta = research_corpusAbstracts$theta, 
                   doc.length = research_corpusAbstracts$doc.length, 
                   vocab = research_corpusAbstracts$vocab, 
                   term.frequency = research_corpusAbstracts$term.frequency,
                   R = terms_shown)
			 
## Visualise and start browser
serVis(json, out.dir = 'Latin_vis', open.browser = FALSE)

## get the tables

dir.create("Latin_tab")

# names(head(sort(phi.frame[,1], decreasing = TRUE)))

## get topic-term distributions and export as csv
phi.frame <- t(data.frame(phi))
colnames(phi.frame) <- paste("topic", as.character(1:K), sep="")
colnames(phi.frame)[1] <- "term"
write.table(phi.frame, file = 'Latin_tab/phi.csv', append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE)

## get document-topic distributions and export as csv
theta.frame <- data.frame(theta)
colnames(theta.frame) <- paste("topic", as.character(1:K-1), sep="")
colnames(theta.frame)[1] <- "identifier"
rownames(theta.frame) <- output_names
write.table(theta.frame, file = 'Latin_tab/theta.csv', append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE)
