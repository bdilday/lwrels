
#' @export
generate_event_data <- function(nlim = NULL, rseed=102, year=2016) {
  ev <- load_events_data(year)

  # rm pitchers as hitters
  ev %<>% filter(BAT_FLD_CD != 1)

  # anyone with less than 20 PA hitting is generic
  lowpa_batters <- ev %>% group_by(BAT_ID) %>% summarise(PA=n()) %>% filter(PA<=20) %$% BAT_ID

  # anyone with less than 20 PA hitting is generic
  lowpa_pitchers <- ev %>% group_by(PIT_ID) %>% summarise(PA=n()) %>% filter(PA<=20) %$% PIT_ID

  if (length(lowpa_batters) > 0) {
    cc <- which(ev$BAT_ID %in% lowpa_batters)
    ev[cc,]$BAT_ID <- "xxxxb001"
  }

  if (length(lowpa_pitchers) > 0) {
    cc <- which(ev$PIT_ID %in% lowpa_pitchers)
    ev[cc,]$PIT_ID <- "xxxxp001"
  }

  if (!is.null(nlim)) {
    idx <- sample(1:nrow(ev), nlim)
    ev <- ev[idx,]
  }

  cc_hr <- which(ev$EVENT_CD == 23)
  cc_so <- which(ev$EVENT_CD == 3)
  cc_bip0 <- which(ev$EVENT_CD == 2)
  cc_bip1 <- which(ev$EVENT_CD >= 20 & ev$EVENT_CD <= 22)
  cc_bb <- which(ev$EVENT_CD >= 14 & ev$EVENT_CD <= 16)

  ev$outcome <- NA

  ev[cc_bip0,]$outcome <- 1
  ev[cc_bip1,]$outcome <- 2
  ev[cc_hr,]$outcome <- 3
  ev[cc_so,]$outcome <- 4
  ev[cc_bb,]$outcome <- 5

  assertthat::are_equal(sum(is.na(ev$outcome)), 0)
  ev %<>% mutate(bid=as.integer(as.factor(BAT_ID)))
  ev %<>% mutate(pid=as.integer(as.factor(PIT_ID)))
  ev %<>% mutate(sid=as.integer(as.factor(HOME_TEAM_ID)))
  ev %<>% mutate(pid = pid+max(bid))
  ev %<>% mutate(sid = sid+max(pid))
  ev %<>% select(GAME_ID, EVENT_ID, EVENT_CD, BAT_ID, PIT_ID, HOME_TEAM_ID, bid, pid, sid, outcome)

  ev
}

#' @export
generate_model_df <- function(event_data=NULL,
                              nlim = NULL, rseed=102, year=2016) {
  if (is.null(event_data)) {
    event_data <- generate_event_data(nlim=nlim, rseed=rseed, year=year)
  }

  bat_ids <- unique(event_data$BAT_ID)
  pit_ids <- unique(event_data$PIT_ID)
  stad_ids <- unique(event_data$HOME_TEAM_ID)

  xx <- model.matrix(outcome ~ bid + pid + sid, data=event_data)[,-1]
  max_levels <- max(xx)
  LEVELS <- c(length(bat_ids), length(pit_ids), length(stad_ids))
  sum_levels = sum(LEVELS)
  ans <- list(N=dim(xx)[[1]],
              D=dim(xx)[[2]],
              K=length(unique(event_data$outcome)),
              LEVELS=LEVELS,
              x=xx,
              y=as.integer(as.character(event_data$outcome)),
              MAX_LEVEL=max_levels,
              SUM_LEVELS=sum_levels,
              ev=event_data)
}

#' @export
initialize_with_lme4 <- function(model_df,
                                 frm=as.formula('outcome ~ (1|bid) + (1|pid) + (1|sid)')) {

  mods <- list()
  unique_outcomes <- unique(model_df$outcome) %>% sort()
  for (u in unique_outcomes) {
    cat(sprintf('fitting model for outcome: %d \n', u))
    cc <- which(model_df$outcome == u)
    stopifnot(length(cc) > 0)
    tmp <- model_df
    tmp[cc,]$outcome <- 1
    tmp[-cc,]$outcome <- 0
    glmer_mod <- glmer(frm, data=tmp,
                       nAGQ = 0,
                       family = binomial,
                       control=glmerControl(optimizer = "nloptwrap")
    )
    mods[[as.character(u)]] <- glmer_mod
  }
  mods

}

#' @export
update_ans <- function(ans, mods) {
  nl <- length(names(mods))

  ans$RANEF_SIGMA <- matrix(rep(0, ans$D * ans$K), ncol=ans$D)
  for (i in seq_along(names(mods))) {
    ichar <- names(mods)[[i]]
    mod <- mods[[ichar]]
    thetas <- sapply(mod@theta, max, 1e-6)
    ans$RANEF_SIGMA[i,] <- thetas
  }

  rr <- matrix(rep(0, ans$K * ans$SUM_LEVELS), ncol=ans$SUM_LEVELS)
  for (idx in seq_along(names(mods))) {
    ichar <- names(mods)[[idx]]
    mod <- mods[[ichar]]
    tmp <- matrix(t(rbind(ranef(mod)$bid,ranef(mod)$pid,ranef(mod)$sid)), nrow=1)
    rr[idx,] <- tmp
  }
  ans$rr <- rr
  cc <- ans$RANEF_SIGMA < 1e-2
  ans$RANEF_SIGMA[cc] <- 0.1
  ans
}

#' @export
get_init_fun <- function(ans) {
  rr <- ans$rr
  k <- ans$K

  function(chain_id=NULL) {
      list(ALPHAX=rr[1:(k-1),], CX=rep(-1, k-1))
  }
}

#' @export
do_stan_fit <- function(model_df, warmup=100, iter=500, seed=10101) {
  init_fun <- get_init_fun(model_df)
    stan(file='inst/extdata/multinom_ravel.stan',
         model_name="multinom_iden",
         data=model_df,
         iter=iter,
         warmup=warmup,
         init=init_fun,
         seed=seed,
         cores=4, chains=4)
}

#' @export
predict_from_stan_X <- function(stan_mod, ans) {
  ee <- rstan::extract(stan_mod)

  alpha_m <- purrr::reduce(lapply(1:5, function(i) {ee$ALPHA[,i,] %>% colMeans()}), cbind)
  c_m <- colMeans(ee$C)

  #  cm[166,] + cm[1061,] + cm[1230,] + colMeans(ee$C)

  pp_mu <- t(t(alpha_m[ans$ev$bid,] + alpha_m[ans$ev$pid,] + alpha_m[ans$ev$sid,]) + c_m)

  nl <- dim(pp_mu)[[1]]
  ll <- lapply(1:nl, function(i) {exp(pp_mu[i,]) / (sum(exp(pp_mu[i,1:5])))})
  pp <- t(matrix(unlist(ll), nrow=5))
  pp
}


#' @export
runs_from_stan <- function(ans, stan_mod, ranef_name, ranef_key, ee=NULL) {
  if (is.null(ee)) {
    ee <- rstan::extract(stan_mod)
  }

  if (ranef_name == 'bid') {
    ndf_cc <- which(ans$ev$BAT_ID == ranef_key)
    id <- ans$ev[ndf_cc,]$bid[[1]]
    offset <- ee$ALPHA[,,]
  } else if (ranef_name == 'pid') {
    ndf_cc <- which(ans$ev$PIT_ID == ranef_key)
    id <- ans$ev[ndf_cc,]$pid[[1]]
  } else if (ranef_name == 'sid') {
    ndf_cc <- which(ans$ev$HOME_TEAM_ID == ranef_key)
    id <- ans$ev[ndf_cc,]$sid[[1]]
  } else {
    stop('random effect ', ranef_name, ' not supported')
  }


  player_events <- ans$ev[ndf_cc,]
  offset <- list()
  for (i in 1:4) {
    offset[[i]] <- ee$ALPHA[,i,id]
  }

  pp_baseline <- predict_from_stan(stan_mod, player_events, ee=ee, offset=offset)
  pp_player <- predict_from_stan(stan_mod, player_events, ee=ee, offset=0)
  runs_from_predictions(pp_player - pp_baseline)

}


#' @export
runs_from_predictions <- function(prediction_array, lw = c(-0.28, 0.573, 1.376, -0.28, 0.336)) {
  dd <- dim(prediction_array)
  tmp <- array(rep(lw, each=dd[[1]] * dd[[2]]), dim=dd)

  (tmp * prediction_array )  %>% apply(1, sum)
}

#' @export
predict_from_stan <- function(stan_mod, ev, ee=NULL, offset = 0) {
  if (is.null(ee)) {
    ee <- rstan::extract(stan_mod)
  }

  etas_key_e <- list()

  if (length(offset) == 1) {
    offset <- rep(offset, 4)
  }

  denom <- 1
  for (i in 1:4) {
    etas_key_e[[i]] <-
      exp(t(t(ee$ALPHA[,i,ev$bid] + ee$ALPHA[,i,ev$pid] + ee$ALPHA[,i,ev$sid] - offset[[i]]) + ee$C[,i]))
    denom <- denom + etas_key_e[[i]]
  }
  etas_key_e[[5]] <- 1

  ll <- lapply(1:5, function(i) {etas_key_e[[i]] / denom})
  ppA <- array(unlist(ll), dim=c(dim(ll[[1]]), length(ll)))

}
