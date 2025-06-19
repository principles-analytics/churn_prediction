save_to_db <- function(data, db_path, tbl_name) {
  on.exit(DBI::dbDisconnect(con))
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbWriteTable(con, tbl_name, data, overwrite = TRUE)
}