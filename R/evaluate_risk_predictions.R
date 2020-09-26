##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param risk_predictions
##' @param times
##' @param testing_data
evaluate_risk_predictions <- function(risk_predictions,
                                      times,
                                      testing_data) {


  scores <- risk_predictions %>%
    unite(md_strat, model, col = 'md_strat_mdl', sep = '..') %>%
    select(md_strat_mdl, sprobs) %>%
    filter(map_lgl(sprobs, ~inherits(.x, 'matrix'))) %>%
    deframe() %>%
    Score(
      formula = Surv(time, status) ~ 1,
      data = testing_data,
      times = times,
      summary = 'IPA',
      plots = 'Calibration',
      se.fit = 0,
      contrasts = FALSE
    ) %>%
    tidy_score() %>%
    separate(md_strat_mdl, into = c('md_strat', 'model'), sep = '\\.\\.')

  gnd <- risk_predictions %>%
    unite(md_strat, model, col = 'md_strat_mdl', sep = '..') %>%
    select(md_strat_mdl, sprobs) %>%
    deframe() %>%
    map_dfr(
      .f = GND_test,
      .id = 'md_strat_mdl',
      predict_horizon = times,
      event_time = testing_data$time,
      event_status = testing_data$status
    ) %>%
    separate(md_strat_mdl, into = c('md_strat', 'model'), sep = '\\.\\.')

  nri <- risk_predictions %>%
    group_by(model) %>%
    group_modify(
      .f = ~ score_nri(data = .x,
                       reference = 'meanmode_si',
                       time = testing_data$time,
                       status = testing_data$status,
                       eval_time = times)
    )

  scores %>%
    left_join(nri) %>%
    left_join(gnd)


}
