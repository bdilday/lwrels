#' @export
get_all_metrics <- function() {
  c("1B_IF", "1B_OF", "2B", "3B", "CF_PO", "Catcher_PO", "First_PO",
    "HBP", "HR", "IBB", "LF_PO", "PO", "Pitcher_PO", "RF_PO", "SO",
    "Second_PO", "Short_PO", "Third_PO", "UIBB")
}

#' @export
get_batter_metrics <- function() {
  c("HR", "3B", "2B", "1B_OF", "1B_IF", "SO", "UIBB", "IBB", "HBP")
}
