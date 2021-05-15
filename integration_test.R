test_data <- read.csv("integration_test_sample.csv")

test_result <- by(test_data, test_data$plantdb, function(x)
  {
  plantdb <- x[1, 5]
  tested <- read.csv(paste("tested_", plantdb, ".csv", sep = ""))
  expected <- x[1:3]
  observed <- tested[c(2, 4, 3)]
  eo_comparison <- as.data.frame(expected == observed)
  eo_comparison$score <- rowSums(eo_comparison)
  result <- cbind(expected, observed, eo_comparison["score"])
  result[result$score != 3, ]
})
