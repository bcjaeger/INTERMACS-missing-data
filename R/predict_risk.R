
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param training
##' @param testing
##' @param pipeline
##' @param verbose

predict_risk_cph <- function(training,
                             testing,
                             pipeline,
                             verbose,
                             times) {

  predict_risk(training = training,
               testing = testing,
               pipeline = pipeline,
               verbose = verbose,
               times = times,
               model = 'cph')

}

predict_risk_xgb <- function(training,
                             testing,
                             pipeline,
                             verbose,
                             times) {

  predict_risk(training = training,
               testing = testing,
               pipeline = pipeline,
               verbose = verbose,
               times = times,
               model = 'xgb')

}

predict_risk_rf <- function(training,
                            testing,
                            pipeline,
                            verbose,
                            times) {

  predict_risk(training = training,
               testing = testing,
               pipeline = pipeline,
               verbose = verbose,
               times = times,
               model = 'rf')

}

predict_risk <- function(training,
                         testing,
                         pipeline,
                         verbose,
                         times,
                         model){

  .f <- switch(
    model,
    'cph' = predict_risk_cph_si,
    'xgb' = predict_risk_xgb_si,
    'rf'  = predict_risk_rf_si
  )

  if(is_tibble(training)) {

    output <- .f(training = training,
       testing = testing,
       pipeline = pipeline,
       verbose = verbose,
       times = times)

  } else {

    output <- map2(
      .x = training,
      .y = testing,
      .f = .f,
      pipeline = pipeline,
      verbose  = verbose,
      times    = times
    ) %>%
      reduce(cbind) %>%
      apply(1, median) %>%
      matrix(ncol = 1)

  }

  output


}
