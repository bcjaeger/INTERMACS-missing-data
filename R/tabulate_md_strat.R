##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param risk_evaluation
tabulate_md_strat <- function(risk_evaluation,
                              md_method_labels,
                              md_type_labels,
                              model_labels,
                              outcome_labels,
                              additional_missing_labels,
                              rspec) {

  output <- risk_evaluation %>%
    select(-bri) %>%
    pivot_longer(cols = c(auc, ipa, cal_error), names_to = 'metric') %>%
    split(f = list(.$outcome, .$metric)) %>%
    map(
      ~ .x %>%
          select(-outcome, -metric) %>%
          pivot_wider(values_from = value, names_from = md_strat) %>%
          mutate(
            across(
              .cols = c(
                mia,
                ranger_si,
                ranger_mi,
                pmm_mi,
                pmm_si,
                bayesreg_mi,
                bayesreg_si,
                hotdeck_si,
                hotdeck_mi,
                nbrs_mi,
                nbrs_si,
              ),
              .fns = ~ .x - meanmode_si
            )
          ) %>%
          pivot_longer(cols = -(iteration:model), names_to = 'md_strat') %>%
          group_by(model, md_strat, additional_missing_pct) %>%
          summarize(
            est = 100 * median(value, na.rm = TRUE),
            lwr = 100 * quantile(value, probs = 0.25, na.rm = TRUE),
            upr = 100 * quantile(value, probs = 0.75, na.rm = TRUE)
          ) %>%
          ungroup()
    )



}
