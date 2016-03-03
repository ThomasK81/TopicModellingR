## setwd, modify according to your needs

setwd("~/OneDrive/TopicModellingR/PersianTM")

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


## User settings:
K <- 7
G <- 5000
alpha <- 0.02
eta <- 0.02
seed <- 37
terms_shown <- 15
swLatin <- TRUE
swEnglish <- FALSE
swGreek <- FALSE
swAdditional <- TRUE
language <- "Greek" # (Persian, Arabic, Latin)
requestURN <- "urn:cts:greekLit:tlg0003.tlg001" # urn:cts:latinLit:phi0448.phi001
capabilities_URL <- "http://www.perseus.tufts.edu/hopper/CTS?request=GetCapabilities"
baseURL <- "http://www.perseus.tufts.edu/hopper/CTS?request=GetPassage&urn="
reffURL <- "http://www.perseus.tufts.edu/hopper/CTS?request=GetValidReff&urn="
morpheusURL <- "https://services.perseids.org/bsp/morphologyservice/analysis/word?word="
searchterms <- ""

## read in some stopwords:

stopwords_english <- stopwords("SMART")
stopwords_latin <- c("ab", "ac", "ad", "adhic", "aliqui", "aliquis", "an", "ante", "apud", "at", "atque", "aut", "autem", "cum", "cur", "de", "deinde", "dum", "ego", "enim", "ergo", "es", "est", "et", "etiam", "etsi", "ex", "fio", "haud", "hic", "iam", "idem", "igitur", "ille", "in", "infra", "inter", "interim", "ipse", "is", "ita", "magis", "modo", "mox", "nam", "ne", "nec", "necque", "neque", "nisi", "non", "nos", "o", "ob", "per", "possum", "post", "pro", "quae", "quam", "quare", "qui", "quia", "quicumque", "quidem", "quilibet", "quis", "quisnam", "quisquam", "quisque", "quisquis", "quo", "quoniam", "sed", "si", "sic", "sive", "sub", "sui", "sum", "super", "suus", "tam", "tamen", "trans", "tu", "tum", "ubi", "uel", "uero", "ut", "t", "cos2", "coepio", "sum", "edo")
stopwords_greek <- c("μή", "ἑαυτοῦ", "ἄν", "ἀλλ’", "ἀλλά", "ἄλλοσ", "ἀπό", "ἄρα", "αὐτόσ", "δ’", "δέ", "δή", "διά", "δαί", "δαίσ", "ἔτι", "ἐγώ", "ἐκ", "ἐμόσ", "ἐν", "ἐπί", "εἰ", "εἰμί", "εἴμι", "εἰσ", "γάρ", "γε", "γα^", "ἡ", "ἤ", "καί", "κατά", "μέν", "μετά", "μή", "ὁ", "ὅδε", "ὅσ", "ὅστισ", "ὅτι", "οὕτωσ", "οὗτοσ", "οὔτε", "οὖν", "οὐδείσ", "οἱ", "οὐ", "οὐδέ", "οὐκ", "περί", "πρόσ", "σύ", "σύν", "τά", "τε", "τήν", "τῆσ", "τῇ", "τι", "τί", "τισ", "τίσ", "τό", "τοί", "τοιοῦτοσ", "τόν", "τούσ", "τοῦ", "τῶν", "τῷ", "ὑμόσ", "ὑπέρ", "ὑπό", "ὡσ", "ὦ", "ὥστε", "ἐάν", "παρά", "σόσ")
# stopwords_arabic and stopwords_persian are currently based on frequency only. I welcome pointers to stopword lists for Classical Arabic and Persian

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

# Read corpus

base_corpus <- read.table("dayofquotes_full.csv", sep=",", header=FALSE)

research_corpus <- as.character(base_corpus[,1])
output_names <- as.character(base_corpus[,2])

research_corpus <- tolower(research_corpus)  # force to lowercase
research_corpus <- gsub("'", " ", research_corpus)  # remove apostrophes
research_corpus <- gsub("-", "", research_corpus)  # remove hyphens, create composita
# research_corpus <- gsub("v", "u", research_corpus) # normalise to 'u'
# research_corpus <- gsub("j", "i", research_corpus) # normalise to 'i'

research_corpus <- gsub("[[:punct:]]", " ", research_corpus)  # replace punctuation with space
research_corpus <- gsub("[[:cntrl:]]", " ", research_corpus)  # replace control characters with space
research_corpus <- gsub("^[[:space:]]+", "", research_corpus) # remove whitespace at beginning of documents
research_corpus <- gsub("[[:space:]]+$", "", research_corpus) # remove whitespace at end of documents
research_corpus <- gsub("[0-9]", "", research_corpus) #remove numbers


# tokenize on space and output as a list:
doc.list <- strsplit(research_corpus, "[[:space:]]+")

# produce dictionary for stemming:

t1 <- Sys.time()

all_words <- unlist(doc.list)
corpus_words <- unique(all_words)
corpus_words <- sort(corpus_words)

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
serVis(json, out.dir = 'DH_vis', open.browser = FALSE)
