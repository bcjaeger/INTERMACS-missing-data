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

    if(inherits(posterior, 'lmerMod')){

      posterior$x@Dimnames[[2]] %>%
        enframe() %>%
        filter(str_detect(value, '^md_strat')) %>%
        pull(value) %>%
        str_remove('md_strat')

    } else {

      posterior$xlevels$md_strat

    }

  }


  join_on <- expand.grid(
    model = c('cph', 'xgb'),
    md_strat = c(
      "meanmode_si",
      "bayesreg_mi",
      "bayesreg_si",
      "hotdeck_mi",
      "hotdeck_si",
      "mia",
      "nbrs_mi",
      "nbrs_si",
      "pmm_mi",
      "pmm_si",
      "ranger_mi",
      "ranger_si"
    ),
    additional_missing_pct = c(0, 15, 30)
  )

  posterior_diffs <- map_dfr(
    .x = bayes_mccv_fits,
    .id = 'id',
    .f = function(posterior) {
      tibble(
        m1 = 'meanmode_si',
        m2 = setdiff(get_xlevels(posterior), 'meanmode_si')
      ) %>%
        mutate(
          distribution = map2(
            .x = m1,
            .y = m2,
            .f = ~ {

              newdata <- tibble(md_strat = c(.x, .y)) %>%
                left_join(join_on, by = 'md_strat')

              newdata$pred <- posterior_predict(
                posterior,
                newdata = newdata,
                re.form = NA
              ) %>%
                apply(2, list) %>%
                map(~.x[[1]])

              newdata <- newdata %>%
                group_by(md_strat) %>%
                summarize(pred = list(reduce(pred, `+`) / n()),
                          .groups = 'drop')

              newdata$pred[[which(newdata$md_strat != 'meanmode_si')]] -
                newdata$pred[[which(newdata$md_strat == 'meanmode_si')]]

            }
          )
        )
    }
  )

  xlabs <- list(
    auc = 'Difference in concordance index\nversus using imputation to the mean',
    cal_error = 'Difference in calibration error\nversus using imputation to the mean',
    ipa = 'Difference in scaled Brier score\nversus using imputation to the mean'
  )

  output <- posterior_diffs %>%
    separate(
      id,
      into = c('outcome', 'model', 'metric'),
      sep = '\\.'
    ) %>%
    # just show one panel instead of two
    split(f = .$metric) %>%
    map2(
      .y = xlabs,
      .f = ~ {

        md_method_labels <- c(
          meanmode = "Imputation\nto the mean",
          mia = "Missingness\nas an\nattribute",
          hotdeck = "Hot deck",
          nbrs = "K-nearest-\nneighbors",
          pmm = "Predictive\nmean\nmatching",
          ranger = "Random\nforest",
          bayesreg = "Bayesian\nregression"
        )

        ggdat <- group_by(.x, m2, metric) %>%
          summarize(distribution = reduce(distribution, `+`) / n()) %>%
          ungroup() %>%
          mutate(
            md_type = if_else(str_detect(m2, '_mi$'), 'mi', 'si'),
            m2 = str_remove(m2, '_mi$|_si$'),
            # m2 = fct_reorder(
            #   .f = m2,
            #   .x = distribution,
            #   .fun = function(x) diff(range(x))
            # ),
            md_type = recode(md_type, !!!md_type_labels),
            md_type = fct_relevel(md_type, md_type_labels[1]),
            distribution = distribution * 100,
            metric = recode(metric,
                            'auc' = 'Area underneath ROC curve',
                            'ipa' = 'Scaled Brier score')
          ) %>%
          mutate(m2 = factor(m2, levels = rev(names(md_method_labels)[-1])))

        probs_upr <- 0.990
        probs_lwr <- 0.940

        if(str_detect(.y, 'calibration')){
          probs_lwr <- 0.0010
          probs_upr <- 0.000001
        }

        x_mi <- ggdat %>%
          filter(md_type == 'Multiple imputation') %>%
          summarize(value = quantile(distribution, probs = probs_upr)) %>%
          pull(value)

        x_si <- ggdat %>%
          filter(md_type == 'Multiple imputation') %>%
          summarize(value = quantile(distribution, probs = probs_lwr)) %>%
          pull(value)

        x_label <- ggdat %>%
          select(metric, m2, md_type) %>%
          distinct() %>%
          mutate(text = "Pr(Difference > 0)",
                 distribution = (x_mi + x_si) / 2)

        if(str_detect(.y, 'calibration')){
          x_label <- ggdat %>%
            select(metric, m2, md_type) %>%
            distinct() %>%
            mutate(text = "Pr(Difference < 0)",
                   distribution = (x_mi + x_si) / 2)
        }

        temp_rspec <- round_spec() %>%
          round_half_even() %>%
          round_using_decimal(digits = 3)

        posterior_comparison <- ggdat %>%
          group_by(m2, md_type, metric) %>%
          summarize(prob_gt_meanmode = mean(distribution > 0)) %>%
          mutate(
            prob_gt_meanmode = if_else(
              condition = str_detect(.y, 'calibration'),
              true = table_glue("{1-prob_gt_meanmode}",rspec = temp_rspec),
              false = table_glue("{prob_gt_meanmode}",rspec = temp_rspec)
            ),
            distribution = if_else(
              condition = md_type == 'Multiple imputation',
              true = x_mi,
              false = x_si
            )
          ) %>%
          split(.$md_type)

        # these need to be restructured for the figure
        plot_labels <- levels(ggdat$m2) %>%
          recode(!!!md_method_labels)

        fig <- ggplot(ggdat, aes(x = distribution, y = m2, fill = md_type)) +
          geom_vline(xintercept = 0, color = 'red', linetype = 2) +
          stat_halfeye(alpha = 0.90) +
          scale_y_discrete(breaks = levels(ggdat$m2),
                           labels = plot_labels) +
          theme_bw() +
          #facet_grid(~metric) +
          geom_label(data = posterior_comparison$`Multiple imputation`,
                     nudge_y = 1/3,
                     nudge_x = 1/4,
                     alpha = 1/2,
                     show.legend = FALSE,
                     aes(label = prob_gt_meanmode)) +
          geom_label(data = posterior_comparison$`Single imputation`,
                     nudge_y = 1/3,
                     nudge_x = 1/4,
                     alpha = 1/2,
                     show.legend = FALSE,
                     aes(label = prob_gt_meanmode)) +
          geom_label(data = x_label,
                     nudge_y = 7/10,
                     nudge_x = 1/4,
                     aes(label = text),
                     fill = 'white') +
          theme(panel.grid.major.y = element_line(),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                legend.position = 'top',
                text = element_text(size = 13)) +
          scale_fill_manual(values = c('orange', 'purple')) +
          labs(fill = '', x = .y, y = '')

        list(fig = fig, probs = posterior_comparison)

      }
    )

  output

}
