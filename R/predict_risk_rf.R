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


  message("fitting CIF model")

  # unbiased random forest using pec and party packages.
  model <- cforest(
    formula = Surv(time,status) ~ .,
    data = .trn,
    controls = cforest_unbiased(maxsurrogate = 3)
  )

  message("done")
  message("computing CIF predictions")

  predictions <- treeresponse(model, newdata = .tst, OOB = TRUE) %>%
    map_dbl(.f = predictRisk,
        newdata = .tst[1, , drop = FALSE], # this is arbitrary
        times = times) %>%
    matrix(ncol = 1)

  message("done")

  predictions

  # a fast randomforest using Ishwaran's package
  # model <- rfsrc.fast(formula = Surv(time, status) ~ .,
  #                     data = .trn,
  #                     ntree = 1000,
  #                     nodesize = 20,
  #                     nsplit = 5,
  #                     forest = TRUE,
  #                     na.action = 'na.impute')
  # predictRisk(model,
  #             newdata = .tst,
  #             times = times)

}
