test_data <- read.csv("testing/integration_test_sample.csv")

# Should result to 0 in gbif, lcvp and tpl, 2 in wcvp and 1 in wfo (consider these 3 mismatches as known issues :)
test_result <- by(test_data, test_data$plantdb, function(x)
  {
  plantdb <- x[1, 5]
  # Load manually prepared results
  tested <- read.csv(paste("testing/tested_", plantdb, ".csv", sep = ""))
  expected <- x[1:3]
  observed <- tested[c(2, 4, 3)]
  observed[2] <- sapply(observed[2], trimws)
  eo_comparison <- as.data.frame(expected[-1] == observed[-1])
  eo_comparison$score <- rowSums(eo_comparison)
  result <- cbind(expected, observed, eo_comparison["score"])
  wrong <- result[result$score != 2, ]
  n_wrong <- nrow(wrong)
  return(list(n_wrong, wrong))
})
