## setwd, modify according to your needs

setwd("~/OneDrive/TopicModellingR/CartoonTM")

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

## User settings:
K <- 17
G <- 5000
alpha <- 0.02
eta <- 0.02
seed <- 37
terms_shown <- 25

## read in some stopwords:

stopwords_english <- stopwords("SMART")

# Enable JIT-compiling

enableJIT(3)

### Functions:

### building test matrix to compare a sentence with all other sentences in the corpus

build_test <- function(x){
  test_cases <- output_names [! output_names %in% x]
  first_column <- rep(x, length(test_cases))
  test_matrix <- matrix(nrow=length(test_cases), ncol = 2)
  test_matrix[,1] <- first_column
  test_matrix[,2] <- test_cases
  return(test_matrix)
}

passage.topic.value <- function(x) {
  max_score <- max(theta.frame[which(theta.frame[,1] == x),2:13])
  topic <- which(theta.frame[which(theta.frame[,1] == x),2:13] == max_score)
  result <- topic + max_score
  result <- result[1]
  return(result)
}

just.topic.value <- function(x) {
  max_score <- max(theta.frame[which(theta.frame[,1] == x),2:13])
  result <- max_score[1]
  return(result)
}

find_passages <- function(x) {
  positions <- which(grepl(x,theta.frame[,1], fixed=TRUE) == TRUE)
  return(theta.frame[,1][positions])
}

is_compound <- function(x) {
  check <- all.equal(phi.t.df[first_element(unlist(x)),], phi.t.df[last_element(unlist(x)),])
  check2 <- all.equal(phi.t.df[last_element(unlist(x)),], phi.t.df[first_element(unlist(x)),])
  if (length(check) == 1) {if (check == "Attributes: < Component “row.names”: 1 string mismatch >") {
    result <- 0
    return(result)
  }}
  result1 <- mean(as.numeric(sub(".*?difference: (.*?)", "\\1", check)[2:length(check)]))
  result2 <- mean(as.numeric(sub(".*?difference: (.*?)", "\\1", check2)[2:length(check2)]))
  result <- (result1 + result2) / 2
  return(result)}

is_similar2 <- function(x,y) {
  check <- all.equal(theta.frame[which(theta.frame[,1] == x),], theta.frame[which(theta.frame[,1] == y),]) # comparing with all.equal
  check2 <- all.equal(theta.frame[which(theta.frame[,1] == y),], theta.frame[which(theta.frame[,1] == x),]) # comparing with all.equal
  if (length(check) == 1) {if (check == TRUE) {
    result <- 0
    return(result)
  }}
  result1 <- mean(as.numeric(sub(".*?difference: (.*?)", "\\1", check)[3:length(check)]))
  result2 <- mean(as.numeric(sub(".*?difference: (.*?)", "\\1", check2)[3:length(check2)]))
  result <- (result1 + result2) / 2
  return(result)}

first_element <- function(x){
  first_element <- head(x, n=1)
  return(first_element)}

last_element <- function(x){
  last_element <- tail(x, n=1)
  return(last_element)}

# Read corpus

base_corpus <- read.table("EQ-Cartoons-No-Doubles.csv", sep=",", header=TRUE)
base_corpus.date <- base_corpus[order(as.Date(base_corpus[,6], format="%d/%m/%Y")),]

output_names <- c(as.character(base_corpus.date[,"identifier"]))

publisher_names <- c(as.character(base_corpus.date[,"Publisher"]))
publisher_names <- gsub('\"', '', publisher_names, fixed=TRUE)
publisher_names <- strsplit(publisher_names, ";", fixed=TRUE)
publisher_names <- unlist(publisher_names)
publisher_names <- gsub("^[[:space:]]+", "", publisher_names)
publisher_names <- gsub("[[:space:]]+$", "", publisher_names)
publisher_names <- unique(publisher_names)

research_corpus <- c(as.character(base_corpus.date[,"description"]))

# base_corpus <- read.table("sanai.csv", sep="\t", header=FALSE)

# research_corpus <- as.character(base_corpus[,7])
# output_names <- as.character(base_corpus[,1])

research_corpus <- tolower(research_corpus)  # force to lowercase
# research_corpus <- gsub("'", " ", research_corpus)  # remove apostrophes
# research_corpus <- gsub("-", "", research_corpus)  # remove hyphens, create composita
# research_corpus <- gsub("v", "u", research_corpus) # normalise to 'u'
# research_corpus <- gsub("j", "i", research_corpus) # normalise to 'i'

research_corpus <- gsub("[[:punct:]]", " ", research_corpus)  # replace punctuation with space
research_corpus <- gsub("[[:cntrl:]]", " ", research_corpus)  # replace control characters with space
research_corpus <- gsub("^[[:space:]]+", "", research_corpus) # remove whitespace at beginning of documents
research_corpus <- gsub("[[:space:]]+$", "", research_corpus) # remove whitespace at end of documents
research_corpus <- gsub("[0-9]", "", research_corpus) #remove numbers
research_corpus <- gsub("[[:space:]]+", " ", research_corpus) # remove multiple whitespace
research_corpus <- trimws(research_corpus)

# tokenize on space and output as a list:
doc.list <- strsplit(research_corpus, "[[:space:]]+")

# produce dictionary for stemming:

t1 <- Sys.time()

all_words <- unlist(doc.list)
corpus_words <- unique(all_words)
corpus_words <- sort(corpus_words)

## stemming

# corpus_words_sample <- sample(1:length(corpus_words), 100)
# stem_dictionary <- sapply(corpus_words[corpus_words_sample], parsing)

# NumberOfForms <- max(unique(sapply(stem_dictionary, length)))
# number_lemmata <- sapply(stem_dictionary, length)

t2 <- Sys.time()
time_stemming <- t2 - t1

# compute the table of terms:
term.table <- table(all_words)
term.table <- sort(term.table, decreasing = TRUE)

# determing stopwords

stop_words2 <- as.data.frame(term.table)
stop_words2 <- row.names(stop_words2[1:100,])
keep <- c("gerry", "house", "second", 
          "people", "buildings", "brownlee", 
          "john", "insurance", "red", 
          "damage", "key", "world", 
          "council", "mayor", "large", 
          "cup", "government", "aftershocks", "first", "over", 
          "zone", "parker", "city", "bob", "rugby")
stop_words2 <- stop_words2 [! stop_words2 %in% keep]
stop_words <- c(stopwords_english, stop_words2)

# prepare ngram_corpus

ngram_corpus <- paste(research_corpus, sep="", collapse=" ")
ngram_corpus <- strsplit(ngram_corpus, "[[:space:]]+")
del <- unlist(ngram_corpus) %in% stop_words
ngram_corpus2 <- unlist(ngram_corpus)[!del]
ngram_corpus2 <- paste(unlist(ngram_corpus2), sep="", collapse=" ")
ng <- ngram(ngram_corpus2)
ngrams <- get.ngrams(ng)
ngram.df <- data.frame(ngrams = ngrams)
ngram.df$vector <- lapply(strsplit(as.character(ngram.df$ngrams), "[[:space:]]+"), "[")

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
serVis(json, out.dir = 'Cartoon_vis', open.browser = FALSE)

## get the tables

dir.create("Cartoon_tab")

# names(head(sort(phi.frame[,1], decreasing = TRUE)))

## get topic-term distributions and export as csv
phi.t <- t(phi)
phi.t.df <- data.frame(matrix(nrow=length(phi.t[, 1]), ncol = K+1))
phi.t.df[, 1] <- names(phi.t[,1])
for (i in 1:K){
  phi.t.df[, i+1] <- phi.t[, i]
}
phicolnames <- vector(mode="character", length=K+1)
phicolnames[1] <- "term"
for (i in 1:K){
  phicolnames[i+1] <- paste(head(phi.t.df[order(phi.t.df[,i+1],decreasing=TRUE),], n=7)[,1], sep="", collapse="_")
}
colnames(phi.t.df) <- phicolnames
write.table(phi.t.df, file = 'Cartoon_tab/phi.csv', append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

## get document-topic distributions and export as csv
theta.frame <- data.frame(matrix(nrow=length(theta[,1]), ncol = K+1))
theta.frame[, 1] <- output_names
for (i in 1:K){
  theta.frame[, i+1] <- theta[, i]
}
thetacolnames <- phicolnames
thetacolnames[1] <- "identifier"
colnames(theta.frame) <- thetacolnames
write.table(theta.frame, file = 'Cartoon_tab/theta.csv', append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

# find 2-gram compounds

phi.t.df <- as.data.frame(t(phi))

ngram.df$compound_score <- lapply(ngram.df$vector, is_compound)
ngram.df$compound_score <- unlist(ngram.df$compound_score)
ngram.df.ordered <- ngram.df[ order(ngram.df[,3], na.last=FALSE), ]
ngram.df.ordered <- ngram.df.ordered[-which(is.na(ngram.df.ordered[,"compound_score"]) == TRUE),]
find_importance <- function(x) {
  result <- length(grep(x, research_corpus))
  return(result)
}
ngram.df.ordered$importance <- sapply(ngram.df.ordered[,"ngrams"], find_importance)
ngram.df.ordered <- ngram.df.ordered[-which(ngram.df.ordered[,"importance"] < 6),]
ngram.first_class <- ngram.df.ordered[which(ngram.df.ordered[,"compound_score"] < 100),]

remove_ngrams <- vector()
counter <- 0
for (i in 1:length(ngram.first_class[,1])) {
  for (j in 2:length(ngram.first_class[,1])) {
    x <- unlist(ngram.first_class[i,2])
    y <- unlist(ngram.first_class[j,2])
    test <- x %in% y
    if (length(grep(TRUE, test)) != 0) {
      if (ngram.first_class[i,3] < ngram.first_class[j,3]) {
        counter <- counter + 1
        remove_ngrams[counter] <- paste(y, collapse=" ")
      } else if (ngram.first_class[i,3] < ngram.first_class[j,3]) {
        counter <- counter + 1
        remove_ngrams[counter] <- paste(x, collapse=" ")
      } else if (ngram.first_class[i,3] < ngram.first_class[j,3]) {
        if (ngram.first_class[i,4] < ngram.first_class[j,4]) {
          counter <- counter + 1
          remove_ngrams[counter] <- paste(x, collapse=" ")
        }
        else if (ngram.first_class[i,3] > ngram.first_class[j,3]) {
          counter <- counter + 1
          remove_ngrams[counter] <- paste(y, collapse=" ")
        }
      }
    } 
  }
}

remove_ngrams <- c(unique(remove_ngrams), "mayor bob")
ngram.first_class <- ngram.first_class[-which(ngram.first_class[,"ngrams"] %in% remove_ngrams),]


### Run Again it again with first set of compounds normalised

compound_normaliser1 <- as.character(ngram.first_class[,1])
for (i in compound_normaliser1) {
  replacement <- gsub("[[:space:]]", "_", i)
  research_corpus <- gsub(i, replacement, research_corpus)
  rm(replacement)
}

# tokenize on space and output as a list:
doc.list <- strsplit(research_corpus, "[[:space:]]+")

# produce dictionary for stemming:

t1 <- Sys.time()

all_words <- unlist(doc.list)
corpus_words <- unique(all_words)
corpus_words <- sort(corpus_words)

## stemming

# corpus_words_sample <- sample(1:length(corpus_words), 100)
# stem_dictionary <- sapply(corpus_words[corpus_words_sample], parsing)

# NumberOfForms <- max(unique(sapply(stem_dictionary, length)))
# number_lemmata <- sapply(stem_dictionary, length)

t2 <- Sys.time()
time_stemming <- t2 - t1

# compute the table of terms:
term.table <- table(all_words)
term.table <- sort(term.table, decreasing = TRUE)

# determing stopwords

stop_words2 <- as.data.frame(term.table)
stop_words2 <- row.names(stop_words2[1:100,])
keep <- c("gerry", "house", "second", 
          "people", "buildings", "brownlee", 
          "john", "insurance", "red", 
          "damage", "key", "world", 
          "council", "mayor", "large", 
          "cup", "government", "aftershocks", "first", "over", 
          "zone", "parker", "city", "bob", "rugby")
stop_words2 <- stop_words2 [! stop_words2 %in% keep]
stop_words <- c(stopwords_english, stop_words2)

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
serVis(json, out.dir = 'CartoonCompsRemoved_vis', open.browser = FALSE)

## get the tables

dir.create("CartoonCompsRemoved_tab")

# names(head(sort(phi.frame[,1], decreasing = TRUE)))

## get topic-term distributions and export as csv
phi.t <- t(phi)
phi.t.df <- data.frame(matrix(nrow=length(phi.t[, 1]), ncol = K+1))
phi.t.df[, 1] <- names(phi.t[,1])
for (i in 1:K){
  phi.t.df[, i+1] <- phi.t[, i]
}
phicolnames <- vector(mode="character", length=K+1)
phicolnames[1] <- "term"
for (i in 1:K){
  phicolnames[i+1] <- paste(head(phi.t.df[order(phi.t.df[,i+1],decreasing=TRUE),], n=7)[,1], sep="", collapse="_")
}
colnames(phi.t.df) <- phicolnames
write.table(phi.t.df, file = 'CartoonCompsRemoved_tab/phi.csv', append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

## get document-topic distributions and export as csv
theta.frame <- data.frame(matrix(nrow=length(theta[,1]), ncol = K+1))
theta.frame[, 1] <- output_names
for (i in 1:K){
  theta.frame[, i+1] <- theta[, i]
}
thetacolnames <- phicolnames
thetacolnames[1] <- "identifier"
colnames(theta.frame) <- thetacolnames
write.table(theta.frame, file = 'CartoonCompsRemoved_tab/theta.csv', append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

total <- merge(base_corpus, theta.frame, by="identifier")


write.table(total, file = 'CartoonCompsRemoved_tab/total.csv', append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

### topic-corpus visualisation

passage.topic.value <- function(x) {
  max_score <- max(theta.frame[which(theta.frame[,1] == x),2:18])
  topic <- which(theta.frame[which(theta.frame[,1] == x),2:18] == max_score)
  result <- topic + max_score
  result <- result[1]
  return(result)
}

just.topic.value <- function(x) {
  max_score <- max(theta.frame[which(theta.frame[,1] == x),2:18])
  result <- max_score[1]
  return(result)
}

find_passages <- function(x) {
  positions <- which(grepl(x,theta.frame[,1], fixed=TRUE) == TRUE)
  return(theta.frame[,1][positions])
}

colourise2 <- function(x) {
  topic_column <- floor(x) + 1
  string <- colnames(theta.frame)[topic_column]
  return(string)
}

cartoons_all <- as.character(total[,"identifier"])

topics_cartoons_all.df <- data.frame(matrix(NA, nrow=length(cartoons_all), ncol=4))
topics_cartoons_all.df[,1] <- cartoons_all
topics_cartoons_all.df[,2] <- sapply(cartoons_all, passage.topic.value)
topics_cartoons_all.df[,3] <- sapply(topics_cartoons_all.df[,2], colourise2)
topics_cartoons_all.df[,4] <- sapply(cartoons_all, just.topic.value)
colnames(topics_cartoons_all.df) <- c("identifier", "TopicValue2", "Topic", "TopicValue")

total <- merge(topics_cartoons_all.df, total, by="identifier")
write.table(total, file = 'CartoonCompsRemoved_tab/total.csv', append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

total_bydate <- total[order(as.Date(total$Date, format="%d/%m/%Y")),]

chart_allcart <- rPlot(TopicValue ~ Date | Topic,
                       data = total_bydate,
                       color = "Topic",
                       type = 'bar',,
                       width = 800,
                       height = 2400)

fusionsave <- total[-c(5,6,7,10,13,14,15,16,17)]
write.table(fusionsave, file = 'CartoonCompsRemoved_tab/fusionsave.csv', append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

total_bydate$Date <- as.Date(total_bydate[,"Date"], "%d/%m/%Y")
CartoonData <- total_bydate

saveRDS(CartoonData, file="CartoonData.Rda")