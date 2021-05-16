library(plantlist)
library(RSQLite)

accepted <- acc_dat[-2]
synonyms <- syn_dat[-2]

synonyms_merged <- merge(synonyms, accepted,
                         by.x = "ACCEPTED_ID",
                         by.y = "ID",
                         all.x = TRUE,
                         suffixes = c("_syn", "_acc"))
synonyms_merged <- synonyms_merged[c(3:5, 2, 6, 7, 1)]
colnames(synonyms_merged) <- c("name", "author", "status", "id",
                               "accepted_name", "accepted_author", "accepted_id")

accepted$accepted_name <- accepted$SCIENTIFIC_NAME
accepted$accepted_author <- accepted$AUTHOR
accepted$accepted_id <- accepted$ID
accepted <- accepted[c(2:4, 1, 5:7)]
colnames(accepted) <- c("name", "author", "status", "id",
                        "accepted_name", "accepted_author", "accepted_id")

tpl <- rbind(accepted, synonyms_merged)

tpl$search_string <- sapply(tpl$name, function (x) {
  paste0(strsplit(x, " ", fixed = TRUE)[[1]][1:2], collapse = "")
}, USE.NAMES = FALSE)

connection <- dbConnect(SQLite(), "species.db")

if ("tpl" %in% dbListTables(connection))
  dbRemoveTable(connection, "tpl")

dbWriteTable(connection, "tpl", tpl)

dbExecute(connection, "CREATE INDEX tpl_index ON tpl(search_string)")

dbDisconnect(connection)

rm(accepted, synonyms, synonyms_merged, tpl)
