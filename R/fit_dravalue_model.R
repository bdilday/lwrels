
#' @import dplyr
#' @import lme4
#' @import magrittr
#' @export
fit_dra_value_model <- function(.data, metric, nagc=0, generic=FALSE) {
  mod_df <- get_dra_model_data(.data, metric)
  if (generic) {
    cc <- mod_df$pitcher_hitting
    mod_df[cc,]$batter<- 'pitashittr'
    frm <- get_dra_model_frm("GENERIC")
  } else {
    frm <- get_dra_model_frm(metric)

    glmer_mod <- lme4::glmer(frm, data=mod_df,
                             nAGQ = nagc,
                             family = binomial,
                             control=glmerControl(optimizer = "nloptwrap")
    )
  }
}


loop_export <- function(year, metrics=NULL, nagc=0) {
  ev <- load_events_data(year)
  if (is.null(metrics)) {
    metrics <- readRDS('./inst/extdata/all_metrics.rds')
  }
  for (metric in metrics) {
    print(paste0("metric: ", metric))
    export_dra_model(ev, metric, year, nagc)
  }
}

#' @export
export_dra_model <- function(.data, metric, year, nagc=0) {
  ofile = sprintf('./inst/extdata/glmer_mod_%s_%d.rds', metric, year)
  glmer_mod <- fit_dra_value_model(.data, metric, nagc=nagc)
  saveRDS(glmer_mod, file=ofile)
}

#' @export
get_dra_model_data <- function(.data, metric="HR") {

  .data$base_outs <- sprintf("%d%d%d%d",
                             1-as.integer(.data$BASE1_RUN_ID==''),
                             1-as.integer(.data$BASE2_RUN_ID==''),
                             1-as.integer(.data$BASE3_RUN_ID==''),
                             .data$OUTS_CT
  )

  tmp <- .data %>% group_by(GAME_ID, PIT_ID) %>%
    mutate(min_pa=min(GAME_PA_CT), PIT_PA_CT=GAME_PA_CT-min_pa, TTO=(PIT_PA_CT %/% 9) + 1 ) %>%
    ungroup()

  if (metric =='HR') {
    tmp$outcome = ifelse(tmp$EVENT_CD==23, 1, 0)
  } else if (metric == '3B') {
    tmp$outcome = ifelse(tmp$EVENT_CD==22, 1, 0)
  } else if (metric == '2B') {
    tmp$outcome = ifelse(tmp$EVENT_CD==21, 1, 0)
  } else if (metric == '1B_IF') {
    tmp$outcome = ifelse(
      (tmp$EVENT_CD==20) & (grepl('^S[1-6]{1}', tmp$EVENT_TX)) , 1, 0)
  } else if (metric == '1B_OF') {
    tmp$outcome = ifelse(
      (tmp$EVENT_CD==20) & (grepl('^S[7-9]{1}', tmp$EVENT_TX)), 1, 0)
  } else if (metric == 'UIBB') {
    tmp$outcome = ifelse(tmp$EVENT_CD==14, 1, 0)
  } else if (metric == 'IBB') {
    tmp$outcome = ifelse(tmp$EVENT_CD==15, 1, 0)
  } else if (metric == 'HBP') {
    tmp$outcome = ifelse(tmp$EVENT_CD==16, 1, 0)
  } else if (metric == 'SO') {
    tmp$outcome = ifelse(tmp$EVENT_CD==3, 1, 0)

  }  else if (metric == 'Pitcher_PO') {
    tmp <- tmp %>% filter(EVENT_OUTS_CT <= 1)
    tmp$outcome = ifelse( (tmp$PO1_FLD_CD == 1) & (tmp$EVENT_CD == 2), 1, 0)

  }  else if (metric == 'Catcher_PO') {
    tmp <- tmp %>% filter(EVENT_OUTS_CT <= 1)
    tmp$outcome = ifelse( (tmp$PO1_FLD_CD == 2) & (tmp$EVENT_CD == 2), 1, 0)

  }  else if (metric == 'First_PO') {
    tmp <- tmp %>% filter(EVENT_OUTS_CT <= 1)
    tmp$outcome = ifelse( (tmp$PO1_FLD_CD == 3) & (tmp$EVENT_CD == 2), 1, 0)

  }  else if (metric == 'Second_PO') {
    tmp <- tmp %>% filter(EVENT_OUTS_CT <= 1)
    tmp$outcome = ifelse( (tmp$PO1_FLD_CD == 4) & (tmp$EVENT_CD == 2), 1, 0)

  }  else if (metric == 'Third_PO') {
    tmp <- tmp %>% filter(EVENT_OUTS_CT <= 1)
    tmp$outcome = ifelse( (tmp$PO1_FLD_CD == 5) & (tmp$EVENT_CD == 2), 1, 0)

  }  else if (metric == 'Short_PO') {
    tmp <- tmp %>% filter(EVENT_OUTS_CT <= 1)
    tmp$outcome = ifelse( (tmp$PO1_FLD_CD == 6) & (tmp$EVENT_CD == 2), 1, 0)

  }  else if (metric == 'LF_PO') {
    tmp <- tmp %>% filter(EVENT_OUTS_CT <= 1)
    tmp$outcome = ifelse( (tmp$PO1_FLD_CD == 7) & (tmp$EVENT_CD == 2), 1, 0)

  }  else if (metric == 'CF_PO') {
    tmp <- tmp %>% filter(EVENT_OUTS_CT <= 1)
    tmp$outcome = ifelse( (tmp$PO1_FLD_CD == 8) & (tmp$EVENT_CD == 2), 1, 0)

  }  else if (metric == 'RF_PO') {
    tmp <- tmp %>% filter(EVENT_OUTS_CT <= 1)
    tmp$outcome = ifelse( (tmp$PO1_FLD_CD == 9) & (tmp$EVENT_CD == 2), 1, 0)

  } else if (metric == "PO") {
    tmp$outcome = ifelse( (tmp$EVENT_CD == 2) & (tmp$EVENT_OUTS_CT <= 1), 1, 0)


  }  else if (metric == 'Pitcher_DP') {
    tmp$outcome = ifelse( (tmp$ASS1_FLD_CD == 1) & (tmp$EVENT_OUTS_CT == 2) & (tmp$EVENT_CD == 2), 1, 0)
  }  else if (metric == 'Catcher_DP') {
    tmp$outcome = ifelse( (tmp$ASS1_FLD_CD == 2) & (tmp$EVENT_OUTS_CT == 2) & (tmp$EVENT_CD == 2), 1, 0)
  }  else if (metric == 'First_DP') {
    tmp$outcome = ifelse( (tmp$ASS1_FLD_CD == 3) & (tmp$EVENT_OUTS_CT == 2) & (tmp$EVENT_CD == 2), 1, 0)
  }  else if (metric == 'Second_DP') {
    tmp$outcome = ifelse( (tmp$ASS1_FLD_CD == 4) & (tmp$EVENT_OUTS_CT == 2) & (tmp$EVENT_CD == 2), 1, 0)
  }  else if (metric == 'Third_DP') {
    tmp$outcome = ifelse( (tmp$ASS1_FLD_CD == 5) & (tmp$EVENT_OUTS_CT == 2) & (tmp$EVENT_CD == 2), 1, 0)
  }  else if (metric == 'Short_DP') {
    tmp$outcome = ifelse( (tmp$ASS1_FLD_CD == 6) & (tmp$EVENT_OUTS_CT == 2) & (tmp$EVENT_CD == 2), 1, 0)

  } else if (metric == "DP") {
    tmp$outcome = ifelse( (tmp$EVENT_CD == 2) & (tmp$EVENT_OUTS_CT >= 2), 1, 0)

  } else if (metric == "BIP_OUT") {
    tmp$outcome = ifelse( tmp$EVENT_CD %in% c(2), 1, 0)

  } else if (metric == "BIP_HIT") {
    tmp$outcome = ifelse( tmp$EVENT_CD %in% c(20, 21, 22), 1, 0)

  } else if (metric == "BIP") {
    tmp$outcome = ifelse( tmp$EVENT_CD %in% c(2, 20, 21, 22), 1, 0)

  } else if (metric == "BB") {
    tmp$outcome = ifelse( tmp$EVENT_CD %in% c(14, 15, 16), 1, 0)

  } else if (metric == "GENERIC_HR"){
    tmp$outcome = ifelse( (tmp$EVENT_CD == 23) & (tmp$EVENT_OUTS_CT >= 2), 1, 0)

  } else {
    stop(sprintf("unknown metric: %s", metric))
  }

  tmp <- tmp %>% transmute(season=season,
                           GAME_ID=GAME_ID,
                           EVENT_ID=EVENT_ID,
                           EVENT_CD=EVENT_CD,
                           outcome=outcome,
                           pitcher_hitting=ifelse(BAT_FLD_CD==1, TRUE, FALSE),
                           role=PIT_START_FL,
                           bats=BAT_HAND_CD, throws=PIT_HAND_CD,
                           pitcher=PIT_ID,
                           batter=BAT_ID,
                           catcher=POS2_FLD_ID,
                           stadium=HOME_TEAM_ID,
                           stadium_hand=paste(stadium, BAT_HAND_CD, sep=''),
                           defense=AWAY_TEAM_ID,
                           Pos_2=POS2_FLD_ID,
                           Pos_3=POS3_FLD_ID,
                           Pos_4=POS4_FLD_ID,
                           Pos_5=POS5_FLD_ID,
                           Pos_6=POS6_FLD_ID,
                           Pos_7=POS7_FLD_ID,
                           Pos_8=POS8_FLD_ID,
                           Pos_9=POS9_FLD_ID,
                           inning_10=ifelse(INN_CT>9, 1, 0),
                           score_diff = ifelse(BAT_HOME_ID==1,
                                               HOME_SCORE_CT-AWAY_SCORE_CT,
                                               -HOME_SCORE_CT+AWAY_SCORE_CT),
                           open_1B_outs = as.factor(
                             ifelse(tmp$BASE1_RUN_ID=='',
                                    paste0('1', tmp$OUTS_CT),
                                    paste0('0', tmp$OUTS_CT))
                           ),
                           base1_run_id=BASE1_RUN_ID,
                           base2_run_id=BASE2_RUN_ID,
                           home_team=as.factor(BAT_HOME_ID),
                           base_outs = as.factor(base_outs),
                           fld_team = ifelse(BAT_HOME_ID==1, AWAY_TEAM_ID, HOME_TEAM_ID),
                           TTO=TTO,
                           platoon_advantage=ifelse(BAT_HAND_CD==PIT_HAND_CD, 'XX', 'XO'),
                           assist=as.factor(ASS1_FLD_CD)
  )

  master_simple <- Lahman::Master %>% select(retroID, birthYear)
  tmp %<>% merge(master_simple, by.x='pitcher', by.y='retroID', all.x=TRUE)
  tmp %<>% mutate(pitcher_age=season-birthYear) %>% select(-birthYear)

  tmp %<>% merge(master_simple, by.x='batter', by.y='retroID', all.x=TRUE)
  tmp %<>% mutate(batter_age=season-birthYear) %>% select(-birthYear)

  tmp %<>% merge(master_simple, by.x='catcher', by.y='retroID', all.x=TRUE)
  tmp %<>% mutate(catcher_age=season-birthYear) %>% select(-birthYear)
  tmp
}


#' @export
get_dra_model_frm <-function(metric) {
  if (metric == 'HR') {
    #TODO: add temperature, framing
    outcome ~ role + bats + throws + (1|pitcher) + (1|stadium) + (1|pitcher_hitting) + (1|batter) + (1|catcher)
  } else if (metric == '3B') {
    #TODO: what is IF-fld?
    outcome ~ (1|batter) + (1|stadium:bats) + (1|Pos_3) + (1|Pos_4) + (1|Pos_7) + (1|Pos_8) + (1|pitcher)
  } else if (metric == '2B') {
    #TODO: add IF-fld (what is it?)
    outcome ~ (1|pitcher) + (1|batter) + (1|Pos_4) + (1|Pos_5) + (1|Pos_7) + (1|Pos_8) + (1|stadium:bats) + inning_10
  } else if (metric == '1B_IF') {
    outcome ~ bats + throws + (1|pitcher) + (1|batter) + (1|Pos_3) + (1|Pos_4) + (1|Pos_5) + (1|Pos_6)
  } else if (metric == '1B_OF') {
    outcome ~ bats + throws + (1|pitcher) + (1|batter) + (1|Pos_5) + (1|Pos_7) + (1|Pos_8) + (1|Pos_9)
  } else if (metric == 'UIBB') {
    # TODO: add framing
    outcome ~ (1|batter) + (1|pitcher) + (1|Pos_2) + (1|pitcher_hitting) + (1|base_outs) + bats + throws + TTO + home_team
  } else if (metric == 'IBB') {
    outcome ~ bats + throws + role + inning_10 + score_diff + (1|batter) + (1|pitcher) + (1|open_1B_outs) + (1|Pos_2) + (1|fld_team)
  } else if (metric == 'HBP') {
    outcome ~ bats + throws + role + (1|batter) + (1|pitcher) + (1|base_outs) + (1|fld_team)
  }  else if (metric == 'SO') {
    # TODO: add framing
    outcome ~ (1|batter) + (1|pitcher) + (1|Pos_2) + (1|pitcher_hitting) + (1|base_outs) + bats + throws + TTO + home_team
  }  else if (metric == 'Pitcher_PO') {
    # TODO: what is assist? is it a position cd, or a player id? I think it's probably a player, otherwise making it a random effect doesn't make sense
    # also applie to below
    outcome ~ (1|batter) + (1|pitcher) + (1|Pos_3) + (1|assist) + bats + throws
  }  else if (metric == 'Catcher_PO') {
    outcome ~ (1|batter) + (1|pitcher) + (1|Pos_2) + (1|base_outs) + bats + throws
  }  else if (metric == 'First_PO') {
    # TODO: add bunt
    outcome ~ (1|batter) + (1|pitcher) + (1|Pos_3) + (1|Pos_5) + (1|assist) + (1|base1_run_id) + (1|pitcher_hitting) + (1|base_outs) + bats + throws

  }  else if (metric == 'Second_PO') {
    outcome ~ (1|pitcher) + (1|Pos_4) + (1|base1_run_id) + (1|batter) + (1|base_outs) + (1|assist) + bats + throws

  }  else if (metric == 'Third_PO') {
    outcome ~ (1|pitcher) + (1|Pos_5) + (1|base1_run_id) + (1|batter) + (1|base_outs) + (1|assist) + bats + throws

  }  else if (metric == 'Short_PO') {
    outcome ~ (1|pitcher)  +(1|Pos_6) + (1|batter) + (1|base_outs) + (1|assist) + bats + throws

  }  else if (metric == 'LF_PO') {
    # TODO: add temperature
    outcome ~ (1|batter) + (1|pitcher) + (1|Pos_7) + (1|stadium) + bats + throws

  }  else if (metric == 'CF_PO') {
    outcome ~ (1|batter) + (1|pitcher) + (1|Pos_8) + (1|stadium) + bats + throws

  }  else if (metric == 'RF_PO') {
    outcome ~ (1|batter) + (1|pitcher) + (1|Pos_9) + (1|Pos_8) + (1|stadium) + bats + throws

  }  else if (metric == 'PO') {
    outcome ~ (1|batter) + (1|pitcher) + (1|Pos_2) + (1|Pos_3) +
      (1|Pos_4) + (1|Pos_5) + (1|Pos_6) + (1|Pos_7) + (1|Pos_8) + (1|Pos_9) +
      (1|stadium) + bats + throws

  }  else if (metric == 'Pitcher_DP') {
    outcome ~ (1|pitcher) + (1|batter) + (1|base1_run_id)
  }  else if (metric == 'Catcher_DP') {
    outcome ~ (1|pitcher) + (1|Pos_2) + (1|Pos_3) + (1|Pos_4) + (1|base1_run_id) + throws
  }  else if (metric == 'First_DP') {
    outcome ~ (1|pitcher) + (1|batter) + (1|Pos_3) + (1|Pos_4) + (1|Pos_6) + (1|base1_run_id) + bats + throws
  }  else if (metric == 'Second_DP') {
    outcome ~ (1|pitcher) + (1|batter) + (1|Pos_4) + (1|Pos_6) + (1|base1_run_id) + (1|base2_run_id) + bats
  }  else if (metric == 'Third_DP') {
    outcome ~ (1|pitcher) + (1|batter) + (1|Pos_3) + (1|Pos_5) + (1|stadium) + (1|base1_run_id) + (1|base2_run_id) + bats
  }  else if (metric == 'Short_DP') {
    outcome ~ (1|pitcher) + (1|batter) + (1|Pos_3) + (1|Pos_4) + (1|Pos_6) + (1|stadium) + (1|base1_run_id) + bats + throws

  }  else if (metric == 'DP') {
    outcome ~ (1|batter) + (1|pitcher) + (1|Pos_2) + (1|Pos_3) +
      (1|Pos_4) + (1|Pos_5) + (1|Pos_6) + (1|Pos_7) + (1|Pos_8) + (1|Pos_9) +
      (1|stadium) + bats + throws

  } else if (grepl('GENERIC', metric)) {
    outcome ~ (1|batter) + (1|pitcher) + (1|catcher) + (1|stadium) +
      platoon_advantage + TTO

  } else {
    stop(sprintf("unknown metric: %s", metric))
  }
}

