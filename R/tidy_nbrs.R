##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

tidy_nbrs <- function(nbrs_brew){

  nbrs_brew$wort %>%
    as_tibble() %>%
    select(-pars) %>%
    mutate(impute = as.integer(impute))

}
