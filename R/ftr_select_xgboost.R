##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data
ftr_select_xgboost <- function(data, n_features) {

  xgb_data <- data %>%
    as_one_hot() %>%
    as_sgb_data(time = time, status = status)

  params <- list(
    max_depth = c(2),
    min_child_weight = c(1.5),
    colsample_bynode = 1/2,
    subsample = 1/2,
    eta = 0.03,
    eval_metric = 'cox-nloglik',
    objective = 'survival:cox'
  )

  model <- sgb_fit(params = params,
                   nrounds = 300,
                   sgb_df = xgb_data)

  output <- xgb.importance(model = model$fit) %>%
    as_tibble() %>%
    slice(1:n_features) %>%
    pull(Feature) %>%
    str_remove_all('\\.\\..+') %>%
    unique()

  ## check
  if(!all(output %in% names(data)))
    warning("a string coding error occurred")

  output[output %in% names(data)]

}
