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

URL <-"http://api.digitalnz.org/v3/records.xml?api_key=LUq9-soDzWWhShuy3XhU&fields=collection"
URLTestcontent <- getURLContent(URL)
doc <- xmlTreeParse(URLTestcontent, getDTD = F)
r <- xmlRoot(doc)
per_page = 100
requests_needed <-  ceiling(as.numeric(xmlValue(r[[1]]))/per_page)
output_list <- list()

for (i in 1:requests_needed ) {
  message("Request", i)
  URL <- paste(URL, "&per_page=", as.character(per_page), "&page=", as.character(i), sep = "")
  URLcontent <- getURLContent(URL)
  # Parse the XML and extract needed information. 
  doc <- xmlTreeParse(URLcontent, getDTD = F)
  rootnode <- xmlRoot(doc)
  results <- xmlChildren(rootnode)[2][[1]]
  catalogue_temp <- xmlSApply(results, function(x) xmlSApply(x, xmlValue))
  catalogue_temp.df <- t(data.frame(catalogue_temp))
  output_list[[i]] <- catalogue_temp.df
}