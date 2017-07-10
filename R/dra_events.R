
#' @import RMySQL
#'
#' @importFrom readr read_csv

get_events_data <- function(year_id) {
  db.connection.info = readr::read_csv('D:/one-offs/mlb/makeHOFData/secret.txt')
  conn = RMySQL::dbConnect(MySQL(),
                   user=db.connection.info$user,
                   password=db.connection.info$password,
                   dbname=db.connection.info$dbname)

  query1 <- paste("select * from retrosheet.events where "
                  ," (event_cd<=3 or event_cd>=20 or (event_cd>=14 and event_cd<=16)) "
                  ," and year_id=", year_id)

  rs <- RMySQL::dbSendQuery(conn, query1)
  res1 = RMySQL::dbFetch(rs, n=-1)
}

export_events_data <- function(year_id, .data=NULL) {
  if (is.null(.data)) {
    .data <- get_events_data(year_id)
  }
  df_name <- sprintf("dra_events_%d", year_id)
  ofile <- sprintf("inst/extdata/%s.rds", df_name)
  assign(df_name, .data)
  saveRDS(get(df_name), file=ofile)
}

#' @export
load_events_data <- function(year_id) {
  df_name <- sprintf("dra_events_%d", year_id)
  ofile <- sprintf("%s/BProDRA/extdata/%s.rds", .libPaths()[[1]], df_name)
  tmp <- readRDS(ofile)
  tmp$season <- as.integer(stringr::str_replace(tmp[1,]$GAME_ID, '^[A-Z]{3}([0-9]{4}).+', '\\1'))
  tmp

}

