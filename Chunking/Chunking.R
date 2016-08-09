load("~/OneDrive/GithubProjects/TopicModellingR/GreekTM/backup.RData")

lengthofcorpus <- length(theta.frame[,1])
result <- vector()
result2 <- vector()
for (i in 1:lengthofcorpus) {
  j <- i + 1
  k <- i - 1
  if (j > lengthofcorpus) {
    j <- i
    }
  if (k == 0) {
    k <- i
  }
  value <- dist(rbind(theta.frame[i,],theta.frame[j,]))
  value2 <- dist(rbind(theta.frame[i,],theta.frame[k,]))
  result[i] <- value
  result2[i] <- value2
}
identifier <- theta.frame[,1]
result.frame <- data.frame(identifier, result, result2)

searcher <- tail(sort(result2),100)
string_name <- identifier[which(result2 %in% searcher)]
table(substring(string_name, nchar(string_name)) == "1")["TRUE"] / length(string_name)