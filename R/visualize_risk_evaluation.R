##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param risk_evaluation
visualize_risk_evaluation <- function(risk_evaluation,
                                      md_method_labels,
                                      md_type_labels,
                                      model_labels,
                                      outcome_labels,
                                      additional_missing_labels,
                                      times) {


  rspec <- round_spec() %>%
    round_using_decimal(digits = 2) %>%
    round_half_even()

  ggdata <- risk_evaluation %>%
    group_by(outcome, md_strat, model, additional_missing_pct) %>%
    summarize(across(c(auc,ipa), mean), .groups = 'drop') %>%
    mutate(md_strat = if_else(md_strat == 'mia',
                              true = 'mia_si',
                              false = md_strat)) %>%
    separate(md_strat, into = c('md_method', 'md_type'), sep = '_') %>%
    group_by(outcome, md_method, model) %>%
    mutate(
      across(
        .cols = c(auc, ipa),
        .fns  = list(pct_diff = ~ table_value(100*(max(.x)-min(.x))/min(.x),
                                              rspec = rspec))
      ),
      across(
        .cols = c(auc, ipa),
        .fns = list(lbl = ~table_value(100 * .x, rspec = rspec))
      )
    ) %>%
    mutate(md_method = recode(md_method, !!!md_method_labels),
           md_type   = recode(md_type, !!!md_type_labels),
           model     = recode(model, !!!model_labels),
           outcome   = recode(outcome, !!!outcome_labels),
           additional_missing_pct = factor(additional_missing_pct),
           additional_missing_pct = fct_recode(additional_missing_pct,
                                               !!!additional_missing_labels),
           md_type   = fct_relevel(md_type, 'Single imputation'))

  md_methods_upper_only <- c(
    "Mean or mode",
    "Missingness as an attribute"
  )

  # auc plots ----

  lower_values <- ggdata %>%
    group_by(outcome, md_method, model, additional_missing_pct) %>%
    select(-ipa) %>%
    filter(auc == min(auc)) %>%
    ungroup() %>%
    filter(!(md_method %in% md_methods_upper_only)) %>%
    nest(lwr = -c(model, outcome))

  upper_values <- ggdata %>%
    group_by(outcome, md_method, model, additional_missing_pct) %>%
    select(-ipa) %>%
    filter(auc == max(auc)) %>%
    group_by(model) %>%
    nest(upr = -c(model, outcome))

  output_auc <- ggdata %>%
    group_by(model, outcome) %>%
    nest() %>%
    left_join(lower_values) %>%
    left_join(upper_values) %>%
    mutate(
      data = map(
        .x = data,
        .f = ~ .x %>%
          mutate(md_method = fct_reorder(md_method, .x = auc, .fun = max))
      ),
      plot = pmap(
        .l = list(data, lwr, upr),
        .f = ~ {

          ggplot(..1) +
            aes(x = auc,
                y = md_method,
                fill = md_type) +
            geom_line(aes(group = md_method), color = 'grey') +
            geom_point(size = 3, shape = 21) +
            geom_text(
              data = ..2,
              aes(label = auc_lbl),
              nudge_x = -0.003
            ) +
            geom_text(
              data = ..3,
              aes(label = auc_lbl),
              nudge_x = +0.003
            ) +
            labs(x = glue('Area underneath the ROC curve,',
                          '{times} months post transplant',
                          .sep = ' '),
                 y = '',
                 fill = '') +
            facet_wrap(~additional_missing_pct) +
            theme_bw() +
            scale_x_continuous(
              limits = c(
                min(..1$auc -0.01),
                max(..1$auc +0.01)
              )
            ) +
            scale_fill_manual(values = c("orange", "purple")) +
            theme(legend.position = 'top',
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_line(linetype = 2))
        }
      )
    )

  # ipa plots ----

  lower_values <- ggdata %>%
    group_by(outcome, md_method, model, additional_missing_pct) %>%
    select(-auc) %>%
    filter(ipa == min(ipa)) %>%
    ungroup() %>%
    filter(!(md_method %in% md_methods_upper_only)) %>%
    nest(lwr = -c(model, outcome))

  upper_values <- ggdata %>%
    group_by(outcome, md_method, model, additional_missing_pct) %>%
    select(-auc) %>%
    filter(ipa == max(ipa)) %>%
    group_by(model) %>%
    nest(upr = -c(model, outcome))

  output_ipa <- ggdata %>%
    group_by(model, outcome) %>%
    nest() %>%
    left_join(lower_values) %>%
    left_join(upper_values) %>%
    mutate(
      data = map(
        .x = data,
        .f = ~ .x %>%
          mutate(md_method = fct_reorder(md_method, .x = ipa, .fun = max))
      ),
      plot = pmap(
        .l = list(data, lwr, upr),
        .f = ~ {
          ggplot(..1) +
            aes(x = ipa,
                y = md_method,
                fill = md_type) +
            geom_line(aes(group = md_method), color = 'grey') +
            geom_point(size = 3, shape = 21) +
            geom_text(
              data = ..2,
              aes(label = ipa_lbl),
              nudge_x = -0.003
            ) +
            geom_text(
              data = ..3,
              aes(label = ipa_lbl),
              nudge_x = +0.003
            ) +
            labs(x = glue('Index of prediction accuracy,',
                          '{times} months post transplant',
                          .sep = ' '),
                 y = '',
                 fill = '') +
            facet_wrap(~additional_missing_pct) +
            theme_bw() +
            scale_x_continuous(
              limits = c(
                min(..1$ipa -0.01),
                max(..1$ipa +0.01)
              )
            ) +
            scale_fill_manual(values = c("orange", "purple")) +
            theme(legend.position = 'top',
                  text = element_text(size = 15),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_line(linetype = 2))
        }
      )
    )

  bind_rows(auc = output_auc, ipa = output_ipa, .id = 'metric')


}
