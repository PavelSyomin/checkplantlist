library(data.table)

data <- fread("wcvp_v4_mar_2021.txt", sep = "|")
str(data)
data2 <- data[rank %in% c("Form",
                          "InfraspecificName",
                          "SPECIES",
                          "Subform",
                          "SUBSPECIES",
                          "Subvariety",
                          "VARIETY")]
table(data2$taxonomic_status)

wcvp <- data[, .(name = taxon_name,
                 author = authors,
                 status = taxonomic_status,
                 id = kew_id,
                 accepted_name,
                 accepted_author = accepted_authors,
                 accepted_id = accepted_kew_id)]

wcvp[status == "Synonym"][1:3]

wcvp[grepl("×", name)][1:10]

wcvp$l <- sapply(wcvp$name, function(x) length(strsplit(x, " ")[[1]]))
wcvp[l == 1][1:50]

wcvp$search_string <- sapply(wcvp$name, function (x) {
  parts <- strsplit(x, " ", fixed = TRUE)[[1]]
  if (length(parts) == 1)
    parts[2] <- ""
  paste0(parts[1:2], collapse = "")
}, USE.NAMES = FALSE)

wcvp <- rbind(
  wcvp[status %in% c("Accepted", "Unplaced", "", "Artificial Hybrid"), .(id,
                                                                         name,
                                                                         author,
                                                                         status,
                                                                         accepted_id = id,
                                                                         accepted_author = author,
                                                                         accepted_name = name,
                                                                         search_string
  )],
  wcvp[status %in% c("Synonym", "Homotypic_Synonym")]
)

conn <- dbConnect(SQLite(), "species.db")

dbWriteTable(conn, "wcvp", wcvp)

dbExecute(conn, "CREATE INDEX wcvp_search_string_index ON wcvp(search_string)")

dbDisconnect(conn)

rm(data, data2, wcvp, conn)
