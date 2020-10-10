##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param risk_evaluation
make_bayes_mccv_fit <- function(risk_evaluation) {

  model_data <- risk_evaluation %>%
    mutate(md_strat = fct_relevel(md_strat, 'meanmode_si'))

  stan_lm(
    formula = ipa ~ outcome + md_strat + model + additional_missing_pct,
    #+ (additional_missing_pct | iteration),
    data = model_data,
    prior = R2(0.30)
  )

  # model_fits_ipa <- map(
  #   .x = model_data,
  #   .f = ~ stan_lmer(
  #     formula = ipa ~ md_strat + model + additional_missing_pct +
  #       (additional_missing_pct | iteration),
  #     data = .x
  #   )
  # )


}
