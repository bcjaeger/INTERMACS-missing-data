##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param risk_evaluation
make_bayes_mccv_fit <- function(risk_evaluation) {

  model_data <- risk_evaluation %>%
    mutate(md_strat = fct_relevel(md_strat, 'meanmode_si')) %>%
    select(-bri) %>%
    pivot_longer(cols = c(auc, ipa, cal_error), names_to = 'metric') %>%
    split(
      f = list(
        .$outcome,
        .$model,
        .$metric
        #.$additional_missing_pct
      )
    )

  output <- vector(mode = 'list', length = length(model_data))
  names(output) <- names(model_data)

  for(i in seq_along(output)){

    output[[i]] <- stan_lmer(
      formula = value ~ md_strat + additional_missing_pct + (1 | iteration),
      data = model_data[[i]],
      #prior = R2(0.50),
      iter = 5000
    )

  }

  output

}
