##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param tbl_md_strat
as_gt <- function(tbl_md_strat,
                  model_labels,
                  additional_missing_labels,
                  md_method_labels,
                  rspec) {

  gt_md_strat <- tbl_md_strat %>%
    map(
      ~ {
        gt_data <- .x %>%
          mutate(
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
          select(-est, -lwr, -upr) %>%
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

        cols <- str_detect(names(gt_data), pattern = 'MI$|SI$')
        cols <- names(gt_data)[cols]

        gt(gt_data,
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

  list(md_strat = gt_md_strat)

}
