setwd("~/Dropbox/ArabicTM")
# libraries needed

library(tm)
library(plyr)
library(lda)
library(LDAvis)

# Read corpus

base_corpus <- read.table("Shamela_0035100.csv", sep="\t", header=FALSE)
research_corpus <- as.character(base_corpus[["V2"]])
output_names <- as.character(base_corpus[["V1"]])

# pre-processing goes here

# tokenize on space and output as a list:
doc.list <- strsplit(research_corpus, "[[:space:]]+")

# produce dictionary for stemming:

all_words <- unlist(doc.list)
corpus_words <- unique(all_words)
corpus_words <- sort(all_words)

#function for stemming goes here

#stemming

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# determing stopwords

stop_words <- as.data.frame(term.table)
stop_words <- row.names(as.data.frame(stop_words[1:100,]))

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

# MCMC and model tuning parameters:
K <- 20
G <- 500
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
                   R=50)
			 
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
