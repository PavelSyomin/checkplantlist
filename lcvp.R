library(LCVP)
library(RSQLite)
library(data.table)

data <- tab_lcvp

data <- as.data.table(data)

table(data$Status)
table(data$PL.comparison)
table(data$PL.alternative)

names_splitter <- function(name)
{
  parts <- strsplit(name, " ")[[1]]
  is_capital_letter <- grepl("^[(A-Z]", parts)
  caps <- which(is_capital_letter)
  if (length(caps) > 1)
  {
    author_start <- caps[2]
    name_end <- author_start - 1
    name <- paste(parts[1:name_end], sep = "", collapse = " ")
    author <- paste(parts[author_start:length(parts)], sep = "", collapse = " ")
  }
  else
  {
    author <- ""
  }
  return(c(name, author))
}

input_split <- lapply(data$Input.Taxon, names_splitter)
output_split <- lapply(data$Output.Taxon, names_splitter)

data$name <- sapply(input_split, function(x) x[1])
data$author <- sapply(input_split, function(x) x[2])
data$accepted_name <- sapply(output_split, function(x) x[1])
data$accepted_author <- sapply(output_split, function(x) x[2])

lcvp <- data[, .(
  id = "",
  name, author,
  status = Status,
  accepted_name, accepted_author,
  accepted_id = ""
)]

table(sapply(lcvp$name, function(x) length(strsplit(x, " ")[[1]])))

lcvp$search_string <- sapply(lcvp$name, function (x) {
  parts <- strsplit(x, " ", fixed = TRUE)[[1]]
  if (length(parts) == 1)
    parts[2] <- ""
  paste0(parts[1:2], collapse = "")
}, USE.NAMES = FALSE)

conn <- dbConnect(SQLite(), "species.db")

dbWriteTable(conn, "lcvp", lcvp)

dbExecute(conn, "CREATE INDEX lcvp_search_string_index ON lcvp(search_string)")

dbDisconnect(conn)
