library(RSQLite)

db_filename <- "species3.db"

connection <- dbConnect(SQLite(), db_filename)
source("database_generation/tpl.R")
source("database_generation/wfo.R")
source("database_generation/wcvp.R")
source("database_generation/gbif.R")
source("database_generation/lcvp.R")
dbDisconnect(connection)

