
#' @export
export_dra_model_simplified <- function(.data, metric, year, nagc=0, overwrite=FALSE) {
  ofile = sprintf('./inst/extdata/smplfy%d/glmer_mod_%s_%d.rds', year, metric, year)
  if (!file.exists(ofile) || overwrite) {
    glmer_mod <- fit_dra_value_model(.data, metric, nagc=nagc, generic=TRUE)
    saveRDS(glmer_mod, file=ofile)
  }
}

#' @export
loop_export_dra_model_simplified <- function(year, metrics=NULL, nagc=0) {
  ev <- load_events_data(year)
  if (is.null(metrics)) {
    metrics <- get_simplified_metrics()
  }
  for (metric in metrics) {
    print(paste0("metric: ", metric))
    export_dra_model_simplified(ev, metric, year, nagc)
  }
}

#' @export
get_simplified_metrics <- function() {
  c("HR", "BB", "SO", "BIP_OUT", "BIP_HIT")
}
