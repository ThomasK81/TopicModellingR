## setwd, modify according to your needs

setwd("~/OneDrive/GithubProjects/TopicModellingR/CartoonTM")

## libraries needed

library(tm)
library(XML)
library(RCurl)
library(plyr)
library(lda)
library(LDAvis)
library(compiler)

## User settings:
K <- 12
G <- 500
alpha <- 0.02
eta <- 0.02
seed <- 37
terms_shown <- 25

## read in some stopwords:

stopwords_english <- stopwords("SMART")

# Enable JIT-compiling

enableJIT(3)

# Read corpus

base_corpus <- read.table("INNZ-Metadata-2010.csv", sep=",", header=TRUE)
# base_corpus.date <- base_corpus[order(as.Date(base_corpus[,5], format="%d/%m/%Y")),]

output_names <- c(as.character(base_corpus[,"identifier"]))

# publisher_names <- c(as.character(base_corpus.date[,"Publisher"]))
# publisher_names <- gsub('\"', '', publisher_names, fixed=TRUE)
# publisher_names <- strsplit(publisher_names, ";", fixed=TRUE)
# publisher_names <- unlist(publisher_names)
# publisher_names <- gsub("^[[:space:]]+", "", publisher_names)
# publisher_names <- gsub("[[:space:]]+$", "", publisher_names)
# publisher_names <- unique(publisher_names)

research_corpus <- c(as.character(base_corpus[,"text"]))

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
research_corpus <- gsub('"', '', research_corpus)  # replace punctuation with space
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

stop_words <- stopwords_english

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
serVis(json, out.dir = 'KM_INNZ_vis', open.browser = FALSE)

## get the tables

dir.create("KM_INNZ_tab")

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
write.table(phi.t.df, file = 'KM_INNZ_tab/phi.csv', append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

## get document-topic distributions and export as csv
theta.frame <- data.frame(matrix(nrow=length(theta[,1]), ncol = K+1))
theta.frame[, 1] <- output_names
for (i in 1:K){
  theta.frame[, i+1] <- theta[, i]
}
thetacolnames <- phicolnames
thetacolnames[1] <- "identifier"
colnames(theta.frame) <- thetacolnames
write.table(theta.frame, file = 'KM_INNZ_tab/theta.csv', append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
