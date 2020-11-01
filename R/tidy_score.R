##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param scores
tidy_score <- function(scores) {

  cal_estm <- plotCalibration(scores,
                              plot = FALSE,
                              method = 'q',
                              cens.method="local") %>%
    use_series('plotFrame') %>%
    map_dbl(~mean((.x$Pred - .x$Obs)^2)) %>%
    enframe(name = 'md_strat_mdl', value = 'cal_error')

  auc_estm <- as_tibble(scores$AUC$score) %>%
    select(md_strat_mdl = model, auc = AUC)

  bri_estm <- as_tibble(scores$Brier$score) %>%
    select(md_strat_mdl = model, bri = Brier, ipa = IPA)

  auc_estm %>%
    left_join(bri_estm, by = 'md_strat_mdl') %>%
    left_join(cal_estm, by = 'md_strat_mdl')

}
