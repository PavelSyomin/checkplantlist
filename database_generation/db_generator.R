library(RSQLite)

db_filename <- "species2.db"

if (file.exists(db_filename))
{
  stop("Database already exists")
} else
{
  connection <- dbConnect(SQLite(), db_filename)
  source("database_generation/tpl.R")
  # source("database_generation/wfo.R")
  # source("database_generation/wcvp")
  # source("database_generation/tpl.R")
  # source("database_generation/lcvp")
  dbDisconnect(connection)
}
