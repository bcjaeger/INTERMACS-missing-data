##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

tabulate_impute_accuracy <- function(mc_cv_results) {

  data_tbl <- mc_cv_results %>%
    filter(additional_missing_pct > 0,
           md_strat != 'mia') %>%
    group_by(
      iteration,
      outcome,
      additional_missing_pct,
      model
    ) %>%
    mutate(
      pe = nominal[md_strat == 'meanmode_si'],
      nominal = (nominal - pe) / (1 - pe)
    ) %>%
    select(md_strat, additional_missing_pct, nominal, numeric) %>%
    group_by(md_strat, additional_missing_pct) %>%
    summarize(
      across(
        .cols = c(nominal, numeric),
        .fns = list(
          est = median,
          lwr = ~quantile(.x, probs = 0.025),
          upr = ~quantile(.x, probs = 0.975)
        )
      )
    )

  data_tbl %>%
    mutate(
      nominal = table_glue(
        "{pm(nominal_est)}{nominal_est}\n({nominal_lwr}, {nominal_upr})"
      ),
      numeric = table_glue(
        "{pm(numeric_est)}{numeric_est}\n({numeric_lwr}, {numeric_upr})"
      )
    ) %>%
    mutate(
      across(
        .cols = c(nominal, numeric),
        .fns = ~str_replace(.x,
                            pattern = fixed('0.00\n(0.00, 0.00)'),
                            replacement = fixed('0 (reference)'))
      )
    ) %>%
    separate(md_strat, into = c('md_method', 'md_type')) %>%
    pivot_wider(values_from = matches("^nominal|^numeric"),
                names_from = md_type) %>%
    select(md_method,
           additional_missing_pct,
           nominal_si,
           nominal_mi,
           numeric_si,
           numeric_mi,
           everything())


}

# resample_ids <- resamples %>%
#   enframe(name = 'iteration', value = 'id')
#
# tmp <- mc_cv_results %>%
#   left_join(resample_ids) %>%
#   unnest(cols = c(sprobs, id)) %>%
#   group_by(md_strat, outcome, additional_missing_pct, model, id) %>%
#   summarize(across(matches("sprob"), mean))
#
# tmp2 <- tmp %>%
#   group_by(md_strat, outcome, additional_missing_pct, model) %>%
#   split(f = .$outcome)
#
# loadd(im)
#
# im_dead <- im %>%
#   select(time = months_post_implant,
#          status = pt_outcome_dead)
#
# im_txpl <- im %>%
#   select(time = months_post_implant,
#          status = pt_outcome_txpl)
#
# tmp3 <- tmp2$txpl %>%
#   ungroup() %>%
#   select(-outcome) %>%
#   filter(md_strat != 'mia',
#          additional_missing_pct == 0) %>%
#   group_by(model, md_strat) %>%
#   nest() %>%
#   unite(col = 'id', md_strat, model) %>%
#   deframe() %>%
#   map(pull, sprobs) %>%
#   Score(
#     formula = Surv(time, status) ~ 1,
#     data = im_txpl,
#     plots = 'Calibration',
#     se.fit = FALSE,
#     times = 6
#   )
#
# pcal <- plotCalibration(tmp3, method = 'q')
#
# pcal$plotFrames %>%
#   bind_rows(.id = 'id') %>%
#   as_tibble() %>%
#   separate(id, into = c('md_method', 'md_strat', 'model')) %>%
#   ggplot(aes(x=Pred, y=Obs, col = md_method)) +
#   geom_point() +
#   geom_line() +
#   geom_abline(slope = 1) +
#   facet_grid(~model)



