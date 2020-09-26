##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @param new_probs
##' @param ref_probs
##' @param time
##' @param status
##' @param eval_time
##'
##' @title

tidy_nri <- function(new_probs,
                     ref_probs,
                     time,
                     status,
                     eval_time){

  nricens(
    time = time,
    event = status,
    p.std = as.numeric(ref_probs),
    p.new = as.numeric(new_probs),
    t0 = eval_time,
    cut = median(as.numeric(ref_probs)),
    niter = 0
  ) %>%
    getElement("nri") %>%
    as_tibble(rownames = 'metric') %>%
    filter(str_detect(metric, '^NRI')) %>%
    select(metric, Estimate) %>%
    pivot_wider(names_from = metric, values_from = Estimate) %>%
    rename(nri_overall = NRI,
           nri_plus = `NRI+`,
           nri_minus = `NRI-`)

}
