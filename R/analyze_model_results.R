
#' @importFrom magrittr %$%
#' @import dplyr
#' @import lme4
#' @export
ranef_to_df <- function(glmer_mod, ranef_name) {
  rr = lme4::ranef(glmer_mod)[[ranef_name]]
  dplyr::data_frame(var_id=rownames(rr), value=rr[,1])
}


#' @export
get_all_batter_runs <- function(event_data, mods) {
  batter_ranef <- extract_batter_ranef(mods)
  bat_ids <- unique(ev$BAT_ID)

  nbat <- length(bat_ids)
  ll <- lapply(1:nbat, function(idx) {
    s <- bat_ids[[idx]]
    print(sprintf("%04d %04d %s", idx, nbat, s))
    tmp <- get_dra_batter_runs(ev, mods, s, batter_ranef_df = batter_ranef)
    tmp$bat_id <- s
    tmp
  }
  )
  dra_runs <- purrr::reduce(ll, rbind.data.frame)

  }

#' @export
export_dra_results <- function(year, npit=NULL, metrics=NULL) {
  print('loading events...')
  ev <- load_events_data(year)
  pit_ids <- unique(ev$PIT_ID)
  print('loading mods...')
  if (is.null(metrics)) {
    metrics <- get_all_metrics()
  }
  mods <- load_fitted_dra_models(year, metrics=metrics)
  print('getting pit ranef...')
  pit_ranef <- extract_pitcher_ranef(mods)

  if (is.null(npit)) {
    npit <- length(pit_ids)
  }

  ll <- lapply(1:npit, function(idx) {
    s <- pit_ids[[idx]]
    print(sprintf("%04d %04d %s", idx, npit, s))
    tmp <- get_dra_runs(ev, mods, s, pitcher_ranef_df = pit_ranef)
    tmp$pit_id <- s
    tmp
    }
  )
  dra_runs <- purrr::reduce(ll, rbind.data.frame)

  print('getting summaries...')
  model_names <- names(mods)

  ll <- lapply(1:npit, function(pit_id_idx) {
    pit_id <- pit_ids[[pit_id_idx]]
    print(sprintf("%04d %04d %s", pit_id_idx, npit, pit_id))
    ll <- lapply(1:length(model_names),
                 function(model_name_idx) {
                   model_name <- model_names[[model_name_idx]]
                   tmp <-summarise_ranef(mods[[model_name_idx]], pit_id)
                   tmp$model_name <- model_name
                   tmp
                 })
    purrr::reduce(ll, rbind.data.frame)
  })


  model_ranef <- purrr::reduce(ll, rbind.data.frame)
  tmp <- list(pit_ranef=pit_ranef, dra_runs=dra_runs, model_ranef=model_ranef)
  saveRDS(tmp, file=sprintf('./inst/extdata/dra_results_%d.rds', year))
  tmp
}

#' @export
average_ranef <-function(mod, pit_id, ranef_name) {
  rrb = ranef_to_df(mod, ranef_name)
  cc = as.character(mod@frame$pitcher) == as.character(pit_id)
  x = sum(cc)
  if (ranef_name %in% names(mod@frame[cc,])) {
    tmp <- mod@frame[cc,] %>% merge(rrb, by.x=ranef_name, by.y="var_id")
    mean(tmp$value)
  } else {
   0
  }

}

#' @export
summarise_ranef <- function(mod, pit_id) {
  df1 <- mod@frame
#  cat(names(df1))
  cc = df1$pitcher == pit_id
  ranef_name <- names(lme4::ranef(mod))
  data_frame(pit_id=pit_id,
             ranef_name=ranef_name,
             mean_value=sapply(ranef_name, function(r) {
               average_ranef(mod, pit_id, r)}
               )
             )
}

#' @export
load_comparison_metrics <- function() {
  readRDS(sprintf('%s/BProDRA/inst/extdata/pitching_metric_throwdown.rds', .libPaths()[[1]]))
}

#' @export
load_fitted_dra_models <- function(year, metrics=NULL, file_path=NULL) {
  if (is.null(metrics)) {
    metrics <- get_all_metrics()
  }
  ans <- list()
  if (is.null(file_path)) {
    fs <- Sys.glob(sprintf('%s/BProDRA/extdata/glmer*%d*rds', .libPaths()[[1]], year))
  } else {
    fs <- Sys.glob(sprintf('%s/glmer*%d*rds', file_path, year))
  }

  for (f in fs) {
    metric <- stringr::str_replace(f, sprintf(".+glmer_mod_(.+)_%s.rds", year), "\\1")
    if (metric %in% metrics) {
      ans[[metric]] <- readRDS(f)
    }
  }
  ans
}

#' @export
extract_pitcher_ranef <- function(mods) {
  purrr::reduce(
    lapply(names(mods),
           function(mod_key) {
             tmp<-ranef_to_df(mods[[mod_key]], "pitcher");
             tmp$model_name <- mod_key;
             return(tmp)
             }
           ),
    rbind.data.frame
    )
}

#' @export
extract_batter_ranef <- function(mods) {
  mods <- mods[get_batter_metrics()]
  purrr::reduce(
    lapply(names(mods),
           function(mod_key) {
             tmp<-ranef_to_df(mods[[mod_key]], "batter");
             tmp$model_name <- mod_key;
             return(tmp)
           }
    ),
    rbind.data.frame
  )
}

#' @export
pool_predictions <- function(event_data, mods, predict_type='response') {
  vv = get_dra_model_data(event_data, "HR")
  purrr::reduce(lapply(names(mods), function(mod_key) {
   # print(mod_key);
    predict(mods[[mod_key]], vv, type=predict_type, allow.new.levels=TRUE)}), cbind)

}

#' @export
get_linear_weights <- function() {

  woba_weights <- list(
    HBP=0.7,
    BB=0.7,
    x1B=0.9,
    x2B=1.25,
    x3B=1.6,
    x4B=2.0
  )
  woba_scale = 1.25
  woba_scale_ = 1/woba_scale
  out_val = -0.28

  lw <- lapply(lapply(woba_weights, `+`, out_val), `*`, woba_scale_)
  data_frame(`1B_IF`=lw$x1B,
       `1B_OF`=lw$x1B,
       `2B`=lw$x2B,
       `3B`=lw$x3B,
       "HR"=lw$x4B,
       "Catcher_PO"=out_val,
       "CF_PO"=out_val,
       "DP"=out_val*2,
       "First_PO"=out_val,
       "HBP"=lw$HBP,
       "IBB"=lw$BB,
       "LF_PO"=out_val,
       "PO"=out_val,
       "Pitcher_PO"=out_val,
       "SO"=out_val,
       "UIBB"=lw$BB,
       "RF_PO"=out_val,
       "Second_PO"=out_val,
       "Short_PO"=out_val,
       "Third_PO"=out_val,
       "BIP_OUT"=out_val,
       "BIP_HIT"=0.15*lw$x1B + 0.045*lw$x2B + 0.005*lw$x3B
       )

}

#' @export
logit_fun <- function(x) {
  exp(x)/(1+exp(x))
}

#' @export
get_delta_probs_batter <- function(event_data, mods, bat_id, batter_ranef_df=NULL) {
  if(is.null(batter_ranef_df)){
    batter_ranef_df <- extract_batter_ranef(mods)
  }

  cc = which(event_data$BAT_ID == bat_id)
  pp <- pool_predictions(event_data[cc,], mods, predict_type = 'link')
  zz <- batter_ranef_df %>% subset(var_id==bat_id)

  w = pp
  w0 = t(t(pp) - zz$value)

  p = logit_fun(w)
  p0 = logit_fun(w0)
  p - p0

}

#' @export
get_delta_probs <- function(event_data, mods, pit_id, pitcher_ranef_df=NULL) {
  if(is.null(pitcher_ranef_df)){
    pitcher_ranef_df <- extract_pitcher_ranef(mods)
  }

  cc = which(event_data$PIT_ID == pit_id)
  pp <- pool_predictions(event_data[cc,], mods, predict_type = 'link')
  zz <- pitcher_ranef_df %>% subset(var_id==pit_id)

  w = pp
  w0 = t(t(pp) - zz$value)

  p = logit_fun(w)
  p0 = logit_fun(w0)
  p - p0

}

#' @export
get_dra_batter_runs <- function(event_data, mods, bat_id, batter_ranef_df=NULL, delta_probs=NULL) {
  if (is.null(delta_probs)) {
    delta_probs <- get_delta_probs_batter(event_data, mods, bat_id, batter_ranef_df)
  }

  lw <- get_linear_weights()
  tt = colSums(delta_probs) * c(data.matrix(lw[,names(mods)]))

  data_frame(event_type=names(mods), dra_runs=tt)

}

#' @export
get_dra_runs <- function(event_data, mods, pit_id, pitcher_ranef_df=NULL, delta_probs=NULL) {
   if (is.null(delta_probs)) {
    delta_probs <- get_delta_probs(event_data, mods, pit_id, pitcher_ranef_df)
  }

  lw <- get_linear_weights()
  tt = colSums(delta_probs) * c(data.matrix(lw[,names(mods)]))

  data_frame(event_type=names(mods), dra_runs=tt)

}

#' @export
dra_components_boxplot <- function(.data) {

  gg <- .data %>%
    dplyr::mutate(p=exp(value)/(1+exp(value))) %>%
    ggplot(aes(x=model_name, y=p, group=model_name)) +
    geom_boxplot() + theme_minimal() + coord_flip() +
    labs(title='DRA Components', x='DRA Component', y='prediction')
  gg <- gg + theme(axis.text = element_text(size=12, face='bold'), axis.title = element_text(face='bold', size=14))

  }

#' @export
log_likelihood <- function(mod, ranef_name, ranef_key, offset=0) {
  tmp <- mod@frame
  cc <- tmp[[ranef_name]] == ranef_key
  tmp <- tmp[cc,]
  pp <- logit_fun(predict(mod, newdata=tmp) + offset)
  sum(tmp$outcome * log(pp) + (1-tmp$outcome) * log(1-pp))
}

#' @export
ranef_uncertainity <- function(mod, ranef_name, ranef_key) {
  rr <- ranef_to_df(mod, ranef_name)
  ranef_baseline_value <- rr[rr$var_id == ranef_key,]$value
  idx <- which(names(mod@cnms) == ranef_name)
  ranef_sigma <- mod@theta[[idx]]
  ranef_sigma_inv_half = 0.5/ranef_sigma^2
  baseline_value <- log_likelihood(mod, ranef_name, ranef_key, offset=0) +
    - ranef_baseline_value^2 * ranef_sigma_inv_half

  dx <- seq(-1, 1, 0.05)
  dy <- sapply(dx, function(x){
    log_likelihood(mod, ranef_name, ranef_key, offset=x) -
      (x - ranef_baseline_value)^2 * ranef_sigma_inv_half})

  list(dx=dx, dy=dy, baseline_value=baseline_value)
}

#' @export
lw_uncertainity <-function (mods) {
  metrics <- names(mods)

}

#' @export
run_value_uncertainty <- function(mods, metric, ranef_name, ranef_key) {
  lw <- get_linear_weights()
  ds <- ranef_uncertainity_parabolic_approx(mods[[metric]], ranef_name, ranef_key)
  mod <- mods[[metric]]
  tmp <- mod@frame
  # TODO: handle case where selection is empty
  cc <- tmp[[ranef_name]] == ranef_key
  tmp <- tmp[cc,]
  eta0 <- predict(mod, newdata=tmp, type='link')
  etaP <- eta0 + ds
  etaM <- eta0 - ds
  dP <- 0.5 * (logit_fun(etaP) + logit_fun(etaM) - 2 * logit_fun(eta0))
  sum(dP * lw[[metric]])
  }

#' @export
ranef_uncertainity_parabolic_approx <- function(mod, ranef_name, ranef_key) {
  rr <- ranef_to_df(mod, ranef_name)
  ranef_baseline_value <- rr[rr$var_id == ranef_key,]$value
  idx <- which(names(mod@cnms) == ranef_name)
  ranef_sigma <- mod@theta[[idx]]

  ds2 <- -2 * second_derivative(mod, ranef_name, ranef_key) + 2 / ranef_sigma^2
  if (ds2 <= 0) {
    stop('second derivative is not positive.')
  }
  1 / sqrt(ds2)
}

#' @export
second_derivative <- function(mod, ranef_name, ranef_key) {
  h <- 1e-6
  f0 <- log_likelihood(mod, ranef_name, ranef_key)
  f1p <- log_likelihood(mod, ranef_name, ranef_key, offset=h)
  f1m <- log_likelihood(mod, ranef_name, ranef_key, offset=-h)
  (f1m + f1p - 2*f0) / h^2
}
