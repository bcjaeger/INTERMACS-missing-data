##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param risk_evaluation
make_bayes_mccv_fit <- function(risk_evaluation) {

  risk_evaluation %>%
    mutate(md_strat = fct_relevel(md_strat, 'meanmode_si')) %>%
    select(-bri) %>%
    pivot_longer(cols = c(auc, ipa), names_to = 'metric') %>%
    split(f = list(.$outcome,
                   .$model,
                   .$metric,
                   .$additional_missing_pct)) %>%
    map(
      ~ stan_lmer(
        formula = value ~ md_strat + (1 | iteration),
        data = .x
      )
    )

}
