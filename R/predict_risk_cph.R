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

predict_risk_cph_si <- function(training,
                                testing,
                                pipeline,
                                verbose,
                                times) {

  # prevent hard error when MIA is used.
  if(any(is.na(training)) | any(is.na(testing))) return(NULL)

  suppressWarnings(
    expr = {
      .rec <- prep(pipeline, training = training)
      .trn <- juice(.rec)
      .tst <- bake(.rec, new_data = testing)
    }
  )

  formula_null <- Surv(time, status) ~ 1

  all_predictors <- names(.trn) %>%
    setdiff(c('time', 'status')) %>%
    glue_collapse(sep = " + ")

  formula_full <- as.formula(glue("Surv(time, status) ~ {all_predictors}"))

  scope = list(
    lower = formula_null,
    upper = formula_full
  )

  message("fitting CPH model")

  model <- stepAIC(
    coxph(Surv(time, status) ~ 1, data = .trn, x = TRUE),
    scope = scope,
    direction = 'both',
    trace = verbose,
    k = log(nrow(.trn)), # uses BIC
    steps = ncol(.trn) - 2 # subtract 2 outcome columns
  )

  if( any(is.na(model$coefficients)) ){

    formula <- Surv(time, status) ~ .
    data_refit <- .trn[, all.vars(model$formula)]

    while ( any(is.na(model$coefficients)) ) {

      message("Found a coefficient dependency")

      na_index <- which(is.na(model$coefficients))
      to_drop <- names(model$coefficients)[na_index]
      data_refit[, to_drop] <- NULL
      model <- coxph(formula = formula, data = data_refit, x = TRUE)

    }

  }

  message("done")

  predictRisk(object = model, newdata = .tst, times = times)

}
