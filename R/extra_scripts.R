# extra scripts 
data <- read.csv2('data/test10.csv', sep = '\t')
data$rri <- as.numeric(data$rri)

set.seed(777)
for (i in 2:10) {
  prob <- i / (10+2)
  print(prob)
  annot <- sample(c(0, 1), length(data$rri), prob = c(1 - prob, prob), replace = TRUE)
  new_data <- data.frame(rri = data$rri, annot = annot)
  # write.table(new_data, file = paste0("test", i + 10, ".csv"), sep = '\t', quote = FALSE, row.names = FALSE)
  new_result <- hrvhra::hrvhra(new_data$rri, new_data$annot)
  n <- length(new_data$rri[new_data$annot == 0]) # filtered length
  SDNN3 <- sd(new_data$rri[new_data$annot == 0]) * sqrt((n-1)/n)
  new_result["SDNN"] <- SDNN3
  # write.table(new_result, file = paste0("test_result", i + 10, ".csv"), quote = FALSE) 
}

