library(data.table)

message("===GBIF Backbone===")
message("Processing data…")
data <- fread("raw_data/gbif.tsv", sep = "\t")
table(data$kingdom)
data2 <- data[kingdom == "Plantae",
              .(name = switch(taxonRank,
                                                    "subspecies" = paste(genericName, specificEpithet, "subsp.",  infraspecificEpithet, sep = " ", collapse = ""),
                                                    "variety" = paste(genericName, specificEpithet, "var.",  infraspecificEpithet, sep = " ", collapse = ""),
                                                    "form" = paste(genericName, specificEpithet, "f.",  infraspecificEpithet, sep = " ", collapse = ""),
                                                    canonicalName),
                                      author = scientificNameAuthorship,
                                      status = taxonomicStatus,
                                      id = taxonID,
                                      accepted_name = NA,
                                      accepted_author = NA,
                                      accepted_id = acceptedNameUsageID,
                                      taxonRank  
                                      ),
              by = taxonID
              ]

table(data2$taxonRank)

data3 <- data2[taxonRank %in% c("form",
                                "species",
                                "subspecies",
                                "variety"
                                )]

table(data3$taxonRank)
table(data3$status)

data3$taxonRank <- NULL

data3[status == "accepted" | status == "doubtful", sum(!is.na(accepted_id))]
data3[status %in% c("heterotypic synonym", "homotypic synonym", "misapplied",  "proparte synonym", "synonym"), sum(is.na(accepted_id))]

gbif_accepted <- data3[status == "accepted" | status == "doubtful",
                       .(name, id, author, status,
                         accepted_name = name,
                         accepted_author = author,
                         accepted_id = id)]
gbif_synonyms <- merge(
  data3[status %in% c("heterotypic synonym", "homotypic synonym", "misapplied",  "proparte synonym", "synonym")][, c("accepted_name", "accepted_author") := NULL],
  data3[, .(accepted_name = name,
            accepted_author = author, 
            accepted_id = id)],
  by = "accepted_id",
  all.x = TRUE,
)

gbif_synonyms[is.na(accepted_name), .N]

gbif_synonyms$taxonID <- NULL

gbif <- rbind(gbif_accepted, gbif_synonyms)

gbif <- gbif[name != ""]

table(sapply(gbif$name, function(x) length(strsplit(x, " ")[[1]])))

gbif$search_string <- sapply(gbif$name, function (x) {
  parts <- strsplit(x, " ", fixed = TRUE)[[1]]
  if (length(parts) == 1)
    parts[2] <- ""
  paste0(parts[1:2], collapse = "")
}, USE.NAMES = FALSE)

message("Writing to database…")
if ("gbif" %in% dbListTables(connection))
  dbRemoveTable(connection, "gbif")

dbWriteTable(connection, "gbif", gbif)

dbExecute(connection, "CREATE INDEX gbif_index ON gbif(search_string)")

rm(data, data2, data3, gbif_accepted, gbif_synonyms, gbif)

message("Done.\n")