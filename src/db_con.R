library(odbc)
library(DBI)
con <- dbConnect(odbc::odbc(), "SQLite", timeout = 10)
dbWriteTable(con, "ed_migraine", data, overwrite = TRUE)