##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param scores
tidy_score <- function(scores) {

  auc_estm <- as_tibble(scores$AUC$score) %>%
    select(md_strat_mdl = model, auc = AUC)

  bri_estm <- as_tibble(scores$Brier$score) %>%
    select(md_strat_mdl = model, bri = Brier, ipa = IPA)

  left_join(auc_estm, bri_estm, by = 'md_strat_mdl')

}
