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

  pm <- function(x){ifelse(x>0, "+", "")}

  output <- risk_evaluation %>%
    filter(model != 'rf') %>%
    pivot_longer(cols = c(auc:GND_pvalue), names_to = 'metric') %>%
    split(f = list(.$outcome, .$metric)) %>%
    map(
      ~ {

        mult_by <- if(.x$metric[1] == 'GND_chisq') 1 else 100

        if(str_detect(.x$metric[1], '^nri'))
          .x$value[.x$md_strat == 'meanmode_si'] <- 0

        data_tbl <- .x %>%
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
            est = mult_by * median(value, na.rm = TRUE),
            lwr = mult_by * quantile(value, probs = 0.025, na.rm = TRUE),
            upr = mult_by * quantile(value, probs = 0.975, na.rm = TRUE)
          ) %>%
          ungroup() %>%
          transmute(
            model = recode(model, !!!model_labels),
            additional_missing_pct = factor(
              additional_missing_pct,
              levels = additional_missing_labels,
              labels = names(additional_missing_labels)
            ),
            md_strat = str_replace(md_strat, 'mia', 'mia_si'),
            table_val = if_else(
              md_strat == 'meanmode_si',
              true = table_glue("{est} ({lwr}, {upr})", rspec=rspec),
              false = table_glue("{pm(est)}{est} ({lwr}, {upr})", rspec=rspec)
            )
          ) %>%
          separate(md_strat, into = c('md_method', 'md_type')) %>%
          pivot_wider(names_from = md_type, values_from = table_val) %>%
          mutate(
            md_method = factor(md_method,
                               levels = names(md_method_labels),
                               labels = md_method_labels)
          ) %>%
          arrange(md_method) %>%
          filter(!(md_method == 'Missingness as an attribute' &
                     model == 'Proportional hazards')) %>%
          rename(MI = mi, SI = si) %>%
          pivot_wider(names_from = model, values_from = c(SI, MI)) %>%
          select(
            additional_missing_pct,
            md_method,
            `SI_Proportional hazards`,
            `MI_Proportional hazards`,
            `SI_Gradient boosted decision trees`,
            `MI_Gradient boosted decision trees`
          )

        cols <- str_detect(names(data_tbl), pattern = 'MI$|SI$')
        cols <- names(data_tbl)[cols]

        gt(data_tbl,
           groupname_col = 'additional_missing_pct',
           rowname_col = 'md_method') %>%
          tab_stubhead(label = 'Imputation method') %>%
          fmt_missing(columns = everything(),
                      missing_text = '--') %>%
          tab_spanner(label = 'Proportional hazards',
                      columns = c("SI_Proportional hazards",
                                  "MI_Proportional hazards")) %>%
          tab_spanner(label = "Gradient boosted decision trees",
                      columns = c("SI_Gradient boosted decision trees",
                                  "MI_Gradient boosted decision trees")) %>%
          cols_label(
            "MI_Proportional hazards" = 'Multiple Imputation',
            "SI_Proportional hazards" = 'Single Imputation',
            "MI_Gradient boosted decision trees" = 'Multiple Imputation',
            "SI_Gradient boosted decision trees" = 'Single Imputation',
          ) %>%
          cols_align('center')
      }
    )



}
