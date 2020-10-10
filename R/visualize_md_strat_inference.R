##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param bayes_mccv_fits
visualize_md_strat_inference <- function(bayes_mccv_fits,
                                         md_type_labels,
                                         md_method_labels,
                                         outcome_labels) {


  posterior_diffs <- expand.grid(
    m1 = bayes_mccv_fits$xlevels$md_strat,
    m2 = bayes_mccv_fits$xlevels$md_strat,
    additional_missing_pct = c(0, 20, 40),
    model = c('cph', 'xgb', 'rf'),
    outcome = c('dead', 'txpl'),
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    filter(m1 == 'meanmode_si') %>%
    mutate(
      distribution = pmap(
        .l = list(m1, m2, additional_missing_pct, model, outcome),
        .f = ~ posterior_epred(
          bayes_mccv_fits,
          newdata = data.frame(
            md_strat = c(..1, ..2),
            additional_missing_pct = ..3,
            model = ..4,
            outcome = ..5
          ),
          re.form = NA
        ) %>%
          apply(1, diff)
      )
    )

  ggdat <- posterior_diffs %>%
    group_by(m2) %>%
    summarize(distribution = reduce(distribution , `+`) / n()) %>%
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
      distribution = distribution * 100
    ) %>%
    mutate(m2 = fct_reorder(m2, .x = distribution, .fun = max))

  plot_labels <- levels(ggdat$m2) %>%
    recode(!!!md_method_labels)

  ggplot(ggdat, aes(x = distribution, y = m2, fill = md_type)) +
    geom_vline(xintercept = 0, color = 'red', linetype = 2) +
    stat_halfeye() +
    scale_y_discrete(breaks = levels(ggdat$m2),
                     labels = plot_labels) +
    theme_bw() +
    theme(panel.grid.major.y = element_line(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position = 'top') +
    scale_fill_manual(values = c('orange', 'purple')) +
    labs(fill = '', x = '',
         y = 'Imputation method')



}
