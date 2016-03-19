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
K <- 15
G <- 5000
alpha <- 0.02
eta <- 0.02
seed <- 37
terms_shown <- 40
# swLatin <- FALSE
# swEnglish <- FALSE
# swGreek <- FALSE
# swAdditional <- TRUE
# language <- "Persian" # (Persian, Arabic, Latin)
# requestURN <- "urn:cts:greekLit:tlg0003.tlg001" # urn:cts:latinLit:phi0448.phi001
# capabilities_URL <- "http://www.perseus.tufts.edu/hopper/CTS?request=GetCapabilities"
# baseURL <- "http://www.perseus.tufts.edu/hopper/CTS?request=GetPassage&urn="
# reffURL <- "http://www.perseus.tufts.edu/hopper/CTS?request=GetValidReff&urn="
morpheusURL <- "http://services.perseids.org/pysvc/morphologyservice/analysis/word?word="
# searchterms <- ""

## read in some stopwords:

# stopwords_english <- stopwords("SMART")
# stopwords_latin <- c("ab", "ac", "ad", "adhic", "aliqui", "aliquis", "an", "ante", "apud", "at", "atque", "aut", "autem", "cum", "cur", "de", "deinde", "dum", "ego", "enim", "ergo", "es", "est", "et", "etiam", "etsi", "ex", "fio", "haud", "hic", "iam", "idem", "igitur", "ille", "in", "infra", "inter", "interim", "ipse", "is", "ita", "magis", "modo", "mox", "nam", "ne", "nec", "necque", "neque", "nisi", "non", "nos", "o", "ob", "per", "possum", "post", "pro", "quae", "quam", "quare", "qui", "quia", "quicumque", "quidem", "quilibet", "quis", "quisnam", "quisquam", "quisque", "quisquis", "quo", "quoniam", "sed", "si", "sic", "sive", "sub", "sui", "sum", "super", "suus", "tam", "tamen", "trans", "tu", "tum", "ubi", "uel", "uero", "ut", "t", "cos2", "coepio", "sum", "edo")
# stopwords_greek <- c("μή", "ἑαυτοῦ", "ἄν", "ἀλλ’", "ἀλλά", "ἄλλοσ", "ἀπό", "ἄρα", "αὐτόσ", "δ’", "δέ", "δή", "διά", "δαί", "δαίσ", "ἔτι", "ἐγώ", "ἐκ", "ἐμόσ", "ἐν", "ἐπί", "εἰ", "εἰμί", "εἴμι", "εἰσ", "γάρ", "γε", "γα^", "ἡ", "ἤ", "καί", "κατά", "μέν", "μετά", "μή", "ὁ", "ὅδε", "ὅσ", "ὅστισ", "ὅτι", "οὕτωσ", "οὗτοσ", "οὔτε", "οὖν", "οὐδείσ", "οἱ", "οὐ", "οὐδέ", "οὐκ", "περί", "πρόσ", "σύ", "σύν", "τά", "τε", "τήν", "τῆσ", "τῇ", "τι", "τί", "τισ", "τίσ", "τό", "τοί", "τοιοῦτοσ", "τόν", "τούσ", "τοῦ", "τῶν", "τῷ", "ὑμόσ", "ὑπέρ", "ὑπό", "ὡσ", "ὦ", "ὥστε", "ἐάν", "παρά", "σόσ")
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

XMLpassage2 <-function(xdata){
  result <- xmlParse(xdata)
  temp.df <- as.data.frame(t(xpathSApply(result, "//*/hdwd", XMLminer)), stringsAsFactors = FALSE)
  as.vector(temp.df[['text']])}

parsing <- function(x){
  word_form <- x
  URL <- paste(morpheusURL, word_form, "&lang=per&engine=hazm", sep = "")
  message(round((match(word_form, corpus_words)-1)/length(corpus_words)*100, digits=2), "% processed. Checking ", x," now.")
  
  URLcontent <- tryCatch({
    getURLContent(URL, httpheader = c(Accept="application/xml"))}, 
    error = function(err)
    {tryCatch({
      Sys.sleep(0.1)
      message("Try once more")
      getURLContent(URL)},
      error = function(err)
      {message("Return original value: ", word_form)
       return(word_form)
      })
    })
  if (URLcontent == "ServerError") {
    lemma <- x
    message(x, " is ", lemma)
    return(lemma)}
  else {
    lemma <- if (is.null(XMLpassage2(URLcontent)) == TRUE) {
      lemma <- x
      message(x, " is ", lemma)
      return(lemma)}
    else {lemma <- tryCatch({XMLpassage2(URLcontent)},
                            error = function(err) {
                              message(x, " not found. Return original value.")
                              lemma <- "NotFound1"
                              message(x, " is ", lemma)
                              return(lemma)})
          
          lemma <- gsub("[0-9]", "", lemma)
          lemma <- tolower(lemma)
          lemma <- unique(lemma)
          if (nchar(lemma) == 0) {
            lemma <- x
            message(x, " is ", lemma)
            return(lemma)}
          else {
            message(x, " is ", lemma)
            return(lemma)
          }
    }
  }
}

# Read corpus

# base_corpus <- read.table("sanai.csv", sep="\t", header=FALSE)
first_corpus <- read.table("sanai.csv", sep="\t", header=FALSE)
second_corpus <- read.table("anvari.csv", sep="\t", header=FALSE)
third_corpus <- read.table("attar.csv", sep="\t", header=FALSE)
fourth_corpus <- read.table("khaghani.csv", sep="\t", header=FALSE)

output_names <- c(as.character(first_corpus[,1]),
                  as.character(second_corpus[,1]),
                  as.character(third_corpus[,1]),
                  as.character(fourth_corpus[,1]))

research_corpus <- c(as.character(first_corpus[,7]),
                     as.character(second_corpus[,7]),
                     as.character(third_corpus[,7]),
                     as.character(fourth_corpus[,7]))

# base_corpus <- read.table("sanai.csv", sep="\t", header=FALSE)

# research_corpus <- as.character(base_corpus[,7])
# output_names <- as.character(base_corpus[,1])

# research_corpus <- tolower(research_corpus)  # force to lowercase
# research_corpus <- gsub("'", " ", research_corpus)  # remove apostrophes
# research_corpus <- gsub("-", "", research_corpus)  # remove hyphens, create composita
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

## stemming

corpus_words_sample <- sample(1:length(corpus_words), 100)
stem_dictionary <- sapply(corpus_words[corpus_words_sample], parsing)

NumberOfForms <- max(unique(sapply(stem_dictionary, length)))
number_lemmata <- sapply(stem_dictionary, length)

t2 <- Sys.time()
time_stemming <- t2 - t1

# compute the table of terms:
term.table <- table(all_words)
term.table <- sort(term.table, decreasing = TRUE)

# determing stopwords

stopword_corpus <- read.table("corpus.csv", sep="\t", header=FALSE)

stopword_corpus <- c(as.character(stopword_corpus[,7]))
stopword_corpus <- gsub("[[:punct:]]", " ", stopword_corpus)  # replace punctuation with space
stopword_corpus <- gsub("[[:cntrl:]]", " ", stopword_corpus)  # replace control characters with space
stopword_corpus <- gsub("^[[:space:]]+", "", stopword_corpus) # remove whitespace at beginning of documents
stopword_corpus <- gsub("[[:space:]]+$", "", stopword_corpus) # remove whitespace at end of documents
stopword_corpus <- gsub("[0-9]", "", stopword_corpus) #remove numbers

# tokenize stopword_corpus on space and output as a list:
doc.list2 <- strsplit(stopword_corpus, "[[:space:]]+")

# compute the table of stop_words:
all_for_stop_words <- unlist(doc.list2)
term.table2 <- table(all_for_stop_words)
term.table2 <- sort(term.table2, decreasing = TRUE)

stop_words <- as.data.frame(term.table2)
rm(term.table2)
stop_words <- row.names(as.data.frame(stop_words[1:200,]))
remove_from_sw <- c(stop_words[37], stop_words[41], stop_words[63], stop_words[65], stop_words[67], stop_words[79], stop_words[86], stop_words[91], stop_words[92], stop_words[94], stop_words[96], stop_words[101], stop_words[116], stop_words[117], stop_words[118], stop_words[120], stop_words[121], stop_words[123], stop_words[128], stop_words[138], stop_words[141], stop_words[142], stop_words[159], stop_words[160], stop_words[164], stop_words[165], stop_words[166], stop_words[171], stop_words[174], stop_words[180], stop_words[182], stop_words[183], stop_words[186], stop_words[187], stop_words[188], stop_words[191], stop_words[192])
stop_words <- stop_words [! stop_words %in% remove_from_sw]

# remove terms that are stop words or occur fewer than "occurenses" times:
occurences <- 5
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
serVis(json, out.dir = 'PERSIAN_vis', open.browser = FALSE)

## get the tables

dir.create("Persian_tab")

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
write.table(phi.t.df, file = 'Persian_tab/phi.csv', append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

## get document-topic distributions and export as csv
theta.frame <- data.frame(matrix(nrow=length(theta[,1]), ncol = K+1))
theta.frame[, 1] <- output_names
for (i in 1:K){
  theta.frame[, i+1] <- theta[, i]
}
thetacolnames <- phicolnames
thetacolnames[1] <- "identifier"
colnames(theta.frame) <- thetacolnames
write.table(theta.frame, file = 'Persian_tab/theta.csv', append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

### more experiments // very experimental and not made for speedy processing

test_matrix <- matrix(nrow=length(corpus[,1]), ncol = length(corpus[,1]))
rownames(test_matrix) <- corpus[,1]
colnames(test_matrix) <- corpus[,1]

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

### topic-corpus visualisation

passage.topic.value <- function(x) {
  max_score <- max(theta.frame[which(theta.frame[,1] == x),2:16])
  topic <- which(theta.frame[which(theta.frame[,1] == x),2:16] == max_score)
  result <- topic + max_score
  result <- result[1]
  return(result)
}

just.topic.value <- function(x) {
  max_score <- max(theta.frame[which(theta.frame[,1] == x),2:16])
  result <- max_score[1]
  return(result)
}

find_passages <- function(x) {
  positions <- which(grepl(x,theta.frame[,1], fixed=TRUE) == TRUE)
  return(theta.frame[,1][positions])
}

sanaee <- find_passages("urn:cts:perslit:sanaee")
anvari <- find_passages("urn:cts:perslit:anvari")
attar <- find_passages("urn:cts:perslit:attar")
AllPers <- c(sanaee, anvari, attar)

colourise2 <- function(x) {
  string <- paste("Topic ", as.character(floor(x)), sep = "")
  return(string)
}

topics_AllPers.df <- data.frame(matrix(NA, nrow=length(AllPers), ncol=4))
topics_AllPers.df[,1] <- AllPers
topics_AllPers.df[,2] <- sapply(AllPers, passage.topic.value)
topics_AllPers.df[,3] <- sapply(topics_AllPers.df[,2], colourise2)
topics_AllPers.df[,4] <- sapply(AllPers, just.topic.value)
colnames(topics_AllPers.df) <- c("Passage", "TopicValue2", "Topic", "TopicValue")

chart1_1 <- rPlot(
  x = "Passage",
  y = "TopicValue",
  data = topics_AllPers.df,
  type = "bar",
  color = "Topic")

chart_sanaee <- rPlot(TopicValue ~ Passage | Topic,
                  data = topics_AllPers.df,
                  color = "Topic",
                  type = 'bar',
                  size = list(const = 3),
                  width = 600,
                  height = 1800)

chart_sanaee$save('TopicsAllPers.html', standalone = TRUE)

topics_AllPers.df.sorted <- topics_AllPers.df[with(topics_AllPers.df, order(-TopicValue2)), ]

chart_topics_sorted <- rPlot(TopicValue ~ Passage | Topic,
                      data = topics_AllPers.df.sorted,
                      color = "Topic",
                      type = 'bar',
                      size = list(const = 3),
                      width = 600,
                      height = 1800)

chart_topics_sorted$save('TopicsAllPersSorted.html', standalone = TRUE)

