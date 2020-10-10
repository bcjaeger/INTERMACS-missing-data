##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param training
##' @param testing
##' @param pipeline
##' @param verbose


predict_risk_xgb_si <- function(training,
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

  xgb_data <- as_sgb_data(.trn, time = time, status = status)

  xgb_cv_folds <- xgb_folds(data = .trn, nfolds = 10, strata = status)

  xgb_cv_tune <- expand.grid(
    max_depth = c(2,4),
    min_child_weight = c(3),
    colsample_bynode = 1/2,
    subsample = 1/2,
    eta = 0.03,
    eval_metric = 'cox-nloglik',
    objective = 'survival:cox'
  ) %>%
    apply(1, as.list) %>%
    enframe(value = 'params') %>%
    mutate(
      fit = map(
        .x = params,
        .f = xgb.cv,
        data = xgb_data$data,
        label = xgb_data$label,
        nrounds = 25000,
        folds = xgb_cv_folds,
        early_stopping_rounds = 50,
        print_every_n = 25000
      )
    )

  xgb_params_tuned <- xgb_cv_tune %>%
    mutate(
      nrounds = map_int(fit, ~.x$best_iteration),
      score = map_dbl(
        .x = fit,
        .f = ~min(.x$evaluation_log$test_cox_nloglik_mean)
      )
    ) %>%
    arrange(score) %>%
    slice(1)

  model <- sgb_fit(params = xgb_params_tuned$params[[1]],
                   nrounds = xgb_params_tuned$nrounds,
                   sgb_df = xgb_data)

  predictors <- setdiff(names(.tst), c('time', 'status'))

  1 - predict(model,
              new_data = as.matrix(.tst[, predictors]),
              eval_times = times)


}
