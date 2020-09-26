##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data
##' @param reference
##' @param time
##' @param status
##' @param eval_time
score_nri <- function(data, reference, time, status, eval_time){

  ref_probs <- data %>%
    filter(md_strat == reference) %>%
    pull(sprobs) %>%
    .[[1]]

  new_probs <- data %>%
    filter(md_strat != reference) %>%
    select(md_strat, sprobs) %>%
    deframe()

  map_dfr(
    .x = new_probs,
    .f = tidy_nri,
    ref_probs = ref_probs,
    time = time,
    status = status,
    eval_time = eval_time,
    .id = 'md_strat'
  )


}
