##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param ranger_mi_train
##' @param ranger_mi_test
tidy_ranger <- function(ranger_mi_train, ranger_mi_test) {

  tidy_train_imputes <- completeData(ranger_mi_train) %>%
    set_names(1:length(.)) %>%
    enframe(name = 'impute', value = 'training') %>%
    mutate(training = map(training, as_tibble))

  tidy_test_imputes <- ranger_mi_test$imputedData %>%
    set_names(1:length(.)) %>%
    enframe(name = 'impute', value = 'testing')

  left_join(tidy_train_imputes,
            tidy_test_imputes,
            by = 'impute') %>%
    mutate(impute = as.integer(impute))

}

tidy_nbrs <- function(nbrs_brew){

  nbrs_brew$wort %>%
    as_tibble() %>%
    select(-pars) %>%
    mutate(impute = as.integer(impute))

}
