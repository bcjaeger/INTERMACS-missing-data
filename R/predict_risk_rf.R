##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @param training
##' @param testing
##' @param pipeline
##' @param verbose
##' @param times
##'
##' @title

predict_risk_rf_si <- function(training,
                               testing,
                               pipeline,
                               verbose,
                               times) {

  suppressWarnings(
    expr = {
      .rec <- prep(pipeline, training = training)
      .trn <- juice(.rec)
      .tst <- bake(.rec, new_data = testing)
    }
  )


  message("fitting RSF model")
  model <- rfsrc.fast(formula = Surv(time, status) ~ .,
                      data = .trn,
                      ntree = 1000,
                      nodesize = 20,
                      nsplit = 5,
                      forest = TRUE,
                      na.action = 'na.impute')
  message("done")

  predictRisk(model,
              newdata = .tst,
              times = times,
              na.action = 'na.impute')

}
