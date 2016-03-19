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

#### Read in tables

CartoonWDoublesData <- readRDS(file="CartoonWDoublesData.Rda")
CartoonData <- readRDS(file="CartoonData.Rda")
CarINNZData <- readRDS(file="CarINNZData.Rda")

#### Visualise Cartoons published in total vs. Cartoons published in Press

Press <- grep("Press (Christchurch", as.character(CartoonWDoublesData[,"Publisher"]), fixed=TRUE)
CartoonsPublished <- data.frame(table(CartoonWDoublesData$Month))
CartoonsPublished_Press <- data.frame(table(CartoonWDoublesData[Press,]$Month))
CartoonsPublished_exclPress <- data.frame(table(CartoonWDoublesData[-Press,]$Month))
colnames(CartoonsPublished) <- c("Month", "CartoonsPublished")
colnames(CartoonsPublished_Press) <- c("Month", "CartoonsPublished_Press")
colnames(CartoonsPublished_exclPress) <- c("Month", "CartoonsPublished_exclPress")
overview <- merge(CartoonsPublished, CartoonsPublished_Press, by="Month")
overview <- merge(overview, CartoonsPublished_exclPress, by="Month")
overview2 <- merge(CartoonsPublished_exclPress, CartoonsPublished_Press, by="Month")

PressCartoons <- gvisColumnChart(overview)
plot(PressCartoons)

PressCartoons2 <- gvisColumnChart(overview2)
plot(PressCartoons2)

CartoonWDoublesData$Press <- grepl("Press (Christchurch", as.character(CartoonWDoublesData[,"Publisher"]), fixed=TRUE)
colnames(overview2) <- c("Month", "Other", "Press")
PressCartoons2 <- gvisSteppedAreaChart(overview2, xvar="Month", yvar=c("Other", "Press"), options=list(isStacked=TRUE, title="Cartoons Published in Press compared to Cartoons Published Elsewhere", width=600, height=600, vAxis="{title:'Cartoons Published'}", hAxis="{title:'Months'}", legend="{position:'top'}"))
plot(PressCartoons2)

#### Total number of cartoons published by month
CartoonsPublished <- data.frame(table(CartoonData$Month))
colnames(CartoonsPublished) <- c("Month", "Cartoons")
CartoonsPublishedVis <- gvisSteppedAreaChart(CartoonsPublished, xvar="Month", yvar="Cartoons", options=list(isStacked=TRUE, title="Total Number of Different Cartoons Published", width=600, height=600, vAxis="{title:'Cartoons Published'}", hAxis="{title:'Months'}", legend="{position:'none'}"))
plot(CartoonsPublishedVis)

#### Bouncy Balls
### Determining Published Articles by Month

INNZ <- grep("INNZ", as.character(CarINNZData[,"identifier"]), , fixed=TRUE)
EQArticles <- CarINNZData[INNZ,]
ArticlesPublished <- data.frame(table(EQArticles$Month))
colnames(ArticlesPublished) <- c("Month", "Articles")

ArticlesPublishedVis <- gvisSteppedAreaChart(ArticlesPublished, xvar="Month", yvar="Articles", options=list(isStacked=TRUE, title="Total Number of Articles Published", width=600, height=600, vAxis="{title:'Articles Published'}", hAxis="{title:'Months'}", legend="{position:'none'}"))
plot(ArticlesPublishedVis)

### See Press vs Others

Press <- grep("Press (Christchurch", as.character(EQArticles[,"Publisher"]), fixed=TRUE)
ArticlesPublished_Press <- data.frame(table(EQArticles[Press,]$Month))
ArticlesPublished_exclPress <- data.frame(table(EQArticles[-Press,]$Month))
colnames(ArticlesPublished_Press) <- c("Month", "Press")
colnames(ArticlesPublished_exclPress) <- c("Month", "Other")
overview2 <- merge(ArticlesPublished_exclPress, ArticlesPublished_Press, by="Month")
PressArticles <- gvisSteppedAreaChart(overview2, xvar="Month", yvar=c("Other", "Press"), options=list(isStacked=TRUE, title="Articles Published in Press compared to Cartoons Published Elsewhere", width=600, height=600, vAxis="{title:'Articles Published'}", hAxis="{title:'Months'}", legend="{position:'top'}"))
plot(PressArticles)

###
EQCartoons <- CarINNZData[-INNZ,]
CartoonsPublished <- data.frame(table(EQCartoons$Month))
colnames(CartoonsPublished) <- c("Month", "Cartoons")


### Topic Modelling Combined ####

topic <- 8
themen <- 10 + topic
test <- EQArticles[c(1,5,themen)]
TopicsINNZ.df <-aggregate(list(test[,3]), list(month = test$Month), sum)
colnames(TopicsINNZ.df) <- colnames(EQArticles[c(5,themen)])

test <- EQCartoons[c(1,5,themen)]
TopicsCartoons.df <-aggregate(list(test[,3]), list(month = test$Month), sum)
colnames(TopicsCartoons.df) <- colnames(EQCartoons[c(5,themen)])

TopicsINNZ <- gvisSteppedAreaChart(TopicsINNZ.df, 
                                       xvar="Month", 
                                       yvar=colnames(TopicsINNZ.df)[2:length(colnames(TopicsINNZ.df))], 
                                       options=list(
                                         isStacked=FALSE, 
                                         title="",
                                         width=600, height=600, 
                                         vAxes="[{viewWindowMode:'explicit', viewWindow:{min:0, max:10}}]", 
                                         hAxis="{title:''}", 
                                         legend="{position:'top'}"))
plot(TopicsINNZ)

TopicsCartoons <- gvisSteppedAreaChart(TopicsCartoons.df, 
                                   xvar="Month", 
                                   yvar=colnames(TopicsINNZ.df)[2:length(colnames(TopicsCartoons.df))], 
                                   options=list(
                                     isStacked=FALSE, 
                                     title="",
                                     width=600, height=600, 
                                     vAxes="[{viewWindowMode:'explicit', viewWindow:{min:0, max:10}}]", 
                                     hAxis="{title:''}", 
                                     legend="{position:'top'}"))
plot(TopicsCartoons)