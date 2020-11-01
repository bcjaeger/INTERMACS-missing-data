##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param im
##' @param resamples
make_risk_evaluation <- function(mc_cv_results, im, resamples, times) {

  risk_eval <- vector(mode = 'list', length = length(resamples))
  names(risk_eval) <- paste(seq(length(resamples)))
  risk_eval <- risk_eval[1:max(mc_cv_results$iteration)]

  for (r in unique(mc_cv_results$iteration)) {

    message("starting run ", r)

    .risk_predictions <- mc_cv_results %>%
      filter(iteration == r,
             map_lgl(sprobs, ~all(!is.na(.x)))) %>%
      group_by(outcome, additional_missing_pct) %>%
      nest() %>%
      rename(risk_prediction = data)

    .testing_data <- list(
      dead = im %>%
          filter(1:n() %in% resamples[[r]]) %>%
          select(time = months_post_implant,
                 status = pt_outcome_dead),
      txpl = im %>%
            filter(1:n() %in% resamples[[r]]) %>%
            select(time = months_post_implant,
                   status = pt_outcome_txpl)
    ) %>%
      enframe(name = 'outcome', value = 'testing_data')

    risk_eval[[r]] <- .risk_predictions %>%
      left_join(.testing_data) %>%
      ungroup() %>%
      transmute(
        outcome,
        additional_missing_pct,
        risk_eval = map2(
          .x = risk_prediction,
          .y = testing_data,
          .f = ~ evaluate_risk_predictions(
            risk_predictions = .x,
            times = times,
            testing_data = .y
          )
        )
      ) %>%
      unnest(cols = risk_eval)

  }

  bind_rows(risk_eval, .id = 'iteration') %>%
    mutate(cal_error = cal_error * 100)

}
