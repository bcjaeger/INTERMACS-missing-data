##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param bayes_mccv_fits
visualize_md_strat_inference <- function(bayes_mccv_fits,
                                         md_type_labels,
                                         md_method_labels,
                                         outcome_labels,
                                         rspec) {

  get_xlevels <- function(posterior) {

    posterior$x@Dimnames[[2]] %>%
      enframe() %>%
      filter(str_detect(value, '^md_strat')) %>%
      pull(value) %>%
      str_remove('md_strat')

  }

  posterior_diffs <- map_dfr(
    .x = bayes_mccv_fits,
    .id = 'id',
    .f = function(posterior) {
      tibble(
        m1 = 'meanmode_si',
        m2 = get_xlevels(posterior)
      ) %>%
        mutate(
          distribution = map2(
            .x = m1,
            .y = m2,
            .f = ~ posterior_predict(
              posterior,
              newdata = data.frame(md_strat = c(.x, .y)),
              re.form = NA
            ) %>%
              apply(1, diff)
          )
        )
    }
  )

  ggdat <- posterior_diffs %>%
    separate(
      id,
      into = c('outcome', 'model', 'metric', 'additional_missing_pct')
    ) %>%
    group_by(m2, metric) %>%
    summarize(distribution = reduce(distribution, `+`) / n()) %>%
    ungroup() %>%
    mutate(
      md_type = if_else(str_detect(m2, '_mi$'), 'mi', 'si'),
      m2 = str_remove(m2, '_mi$|_si$'),
      m2 = fct_reorder(
        .f = m2,
        .x = distribution,
        .fun = function(x) diff(range(x))
      ),
      md_type = recode(md_type, !!!md_type_labels),
      md_type = fct_relevel(md_type, md_type_labels[1]),
      distribution = distribution * 100,
      metric = recode(metric,
                      'auc' = 'Area underneath ROC curve',
                      'ipa' = 'Scaled Brier score')
    ) %>%
    mutate(m2 = fct_reorder(m2, .x = distribution, .fun = max))

  x_mi <- ggdat %>%
    filter(md_type == 'Multiple imputation') %>%
    summarize(value = quantile(distribution, probs = 0.999)) %>%
    pull(value)

  x_si <- ggdat %>%
    filter(md_type == 'Multiple imputation') %>%
    summarize(value = quantile(distribution, probs = 0.975)) %>%
    pull(value)

  x_label <- ggdat %>%
    select(metric, m2, md_type) %>%
    distinct() %>%
    mutate(text = "Pr(Difference > 0)",
           distribution = (x_mi + x_si) / 2)

  temp_rspec <- round_spec() %>%
    round_half_even() %>%
    round_using_decimal(digits = 3)

  posterior_comparison <- ggdat %>%
    group_by(m2, md_type, metric) %>%
    summarize(prob_gt_meanmode = mean(distribution > 0)) %>%
    mutate(
      prob_gt_meanmode = table_glue("{prob_gt_meanmode}",
                                    rspec = temp_rspec),
      distribution = if_else(
        md_type == 'Multiple imputation', x_mi, x_si
      )
    ) %>%
    split(.$md_type)

  plot_labels <- levels(ggdat$m2) %>%
    recode(!!!md_method_labels)

  ggplot(ggdat, aes(x = distribution, y = m2, fill = md_type)) +
    geom_vline(xintercept = 0, color = 'red', linetype = 2) +
    stat_halfeye(alpha = 0.90) +
    scale_y_discrete(breaks = levels(ggdat$m2),
                     labels = plot_labels) +
    theme_bw() +
    facet_grid(~metric) +
    geom_label(data = posterior_comparison$`Multiple imputation`,
               nudge_y = 1/2,
               nudge_x = 1/4,
               alpha = 1/2,
               show.legend = FALSE,
               aes(label = prob_gt_meanmode)) +
    geom_label(data = posterior_comparison$`Single imputation`,
               nudge_y = 1/2,
               nudge_x = 1/4,
               alpha = 1/2,
               show.legend = FALSE,
               aes(label = prob_gt_meanmode)) +
    geom_label(data = x_label,
               nudge_y = 3/4,
               nudge_x = 1/4,
               aes(label = text),
               fill = 'white') +
    theme(panel.grid.major.y = element_line(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position = 'top',
          text = element_text(size = 18)) +
    scale_fill_manual(values = c('orange', 'purple')) +
    labs(fill = '',
         x = 'Difference in model performance versus using imputation to the mean',
         y = '')



}
