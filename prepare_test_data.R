library(RSQLite)

connection <- dbConnect(SQLite(), "species.db")

get_sample <- function(plantdb_name)
{
  sample_size = 60
  query = paste0("SELECT * FROM ", plantdb_name)
  data <- dbGetQuery(connection, query)
  sampled_data <- data[sample(1:nrow(data), sample_size), ]
  rm(data)
  sampled_data
}

plantdb_names <- c("plantlist", "wfo", "wcvp", "gbif", "lcvp")
data_samples <- lapply(plantdb_names, get_sample)

dbDisconnect(connection)

data_sample <- do.call(rbind, data_samples)

data_sample[is.na.data.frame(data_sample)] <- ""

data_sample$input <- trimws(mapply(paste, data_sample$name, data_sample$author, sep = " "))
data_sample$output <- trimws(mapply(paste, data_sample$accepted_name, data_sample$accepted_author, sep = " "))
sample_to_save <- data_sample[c("input", "output", "status")]

# write.csv(sample_to_save, "integration_test_sample.csv", row.names = FALSE)
