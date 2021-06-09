library(data.table)

message("===World Flora Online===")
message("Processing data…")
data <- fread("raw_data/wfo.txt", sep = "\t", quote = "\"", header = TRUE)

data2 <- data[, c("taxonID",
                "scientificName",
                "taxonRank",
                "scientificNameAuthorship",
                "taxonomicStatus",
                "acceptedNameUsageID")]

data2$taxonRank <- toupper(data2$taxonRank)
table(data2$taxonRank)

table(data2$taxonomicStatus)

data3 <- data2[taxonRank %in% c("FORM", "FORMA", "SPECIES", "SUBFORM", "SUBSPECIES", "SUBVARIETY", "VARIETY")]
table(data3$taxonRank)

data2[taxonRank == "NOTHOSPECIES"]

table(data3$taxonomicStatus)

data3[taxonomicStatus == "homotypicSynonym"][1:10]

nrow(data3[taxonomicStatus == "Unchecked" & acceptedNameUsageID != ""])
nrow(data3[taxonomicStatus == "Synonym" & acceptedNameUsageID == ""])
nrow(data3[taxonomicStatus == "homotypicSynonym" & acceptedNameUsageID == ""])
nrow(data3[taxonomicStatus == "heterotypicSynonym" & acceptedNameUsageID == ""])

wfo_accepted <- data3[taxonomicStatus %in% c("Accepted", "Unchecked")]
wfo_synonyms <- data3[taxonomicStatus %in% c("heterotypicSynonym", "homotypicSynonym", "Synonym")]
nrow(wfo_accepted) + nrow(wfo_synonyms) == nrow(data3)

wfo_accepted <- wfo_accepted[, .(name = scientificName,
                                 author = scientificNameAuthorship,
                                 status = taxonomicStatus,
                                 id = taxonID,
                                 accepted_name = scientificName,
                                 accepted_author = scientificNameAuthorship,
                                 accepted_id = taxonID
                                 )]

wfo_synonyms <- wfo_synonyms[, .(name = scientificName,
                                 author = scientificNameAuthorship,
                                 status = taxonomicStatus,
                                 id = taxonID,
                                 accepted_id = acceptedNameUsageID
                                 )]
wfo_synonyms <- merge(wfo_synonyms, wfo_accepted,
                      by.x = "accepted_id", by.y = "id",
                      all.x = TRUE,
                      suffixes = c("_syn", "_acc"))

wfo_synonyms <- wfo_synonyms[, .(name = name_syn,
                                 author = author_syn,
                                 status = status_syn,
                                 id,
                                 accepted_name = name_acc,
                                 accepted_author = author_acc,
                                 accepted_id)]

wfo <- rbind(wfo_accepted, wfo_synonyms)

table(sapply(wfo$name, function(x) length(strsplit(x, " ")[[1]])))

wfo$search_string <- sapply(wfo$name, function (x) {
  paste0(strsplit(x, " ", fixed = TRUE)[[1]][1:2], collapse = "")
}, USE.NAMES = FALSE)

message("Writing to database…")
if ("wfo" %in% dbListTables(connection))
  dbRemoveTable(connection, "wfo")

dbWriteTable(connection, "wfo", wfo)

dbExecute(connection, "CREATE INDEX wfo_index ON wfo(search_string)")

rm(data, data2, data3, wfo, wfo_accepted, wfo_synonyms)

message("Done.\n")