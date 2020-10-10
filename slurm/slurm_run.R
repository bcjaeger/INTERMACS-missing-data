

source("../packages.R")
R.utils::sourceDirectory("../R")

.rslurm_func <- function(iteration,
                         md_strat,
                         outcome,
                         additional_missing_pct = 0,
                         n_features = 50,
                         n_impute_mi = 5,
                         times = 6,
                         maxtime = 8) {

  set.seed(iteration)

  im = read_csv("../data/intermacs_clean.csv", guess_max = 20000) %>%
    rename(time = months_post_implant,
           status = paste0('pt_outcome_', outcome)) %>%
    select(-starts_with('pt_outcome_')) %>%
  # if you want to truncate event times
  mutate(
    status = if_else(time > maxtime & status == 1, 0, status),
    time = pmin(time, maxtime)
  )

  testing_index <- read_rds('../data/resamples.rds')[[iteration]]

  # train/test split -----

  im_train_raw <- im[-testing_index, ] #%>% slice_sample(prop = 1/10)
  im_test_raw  <- im[ testing_index, ] #%>% slice_sample(prop = 1/2)

  too_many_missing <- im_train_raw %>%
    miss_var_summary() %>%
    filter(pct_miss > 50) %>%
    pull(variable)

  im_train_raw[, too_many_missing] <- NULL
  im_test_raw[, too_many_missing] <- NULL

  # filter columns using xgboost (MIA) -----
  features <- ftr_select_xgboost(im_train_raw, n_features = n_features)

  analysis_variables <- c('time', 'status', features)

  im_train_raw %<>% select(all_of(analysis_variables))
  im_test_raw %<>% select(all_of(analysis_variables))

  prop_miss_train <- prop_miss(im_train_raw)
  prop_miss_test <- prop_miss(im_test_raw)

  message("% missing in training data: ", 100 * prop_miss_train)

  # amputation (if additional_missing_pct > 0) ----

  amputed_values <- vector(mode = 'list', length = length(features)) %>%
    set_names(features)

  # to induce MAR
  age_cats <- cut(im_train_raw$demo_age,
                  breaks = c(0, 40, 65, Inf),
                  include.lowest = TRUE)

  age_probs <- rep(0, length(age_cats))
  age_probs[age_cats == '[0,40]'] <- 0.40
  age_probs[age_cats == '(40,65]'] <- 0.10
  age_probs[age_cats == '(65,Inf]'] <- 0.40

  if(additional_missing_pct > 0){

    for(i in features){

      trn_n_observed <- sum(!is.na(im_train_raw[[i]]))
      trn_pct_observed <- 100 * trn_n_observed / nrow(im_train_raw)

      trn_n_to_ampute <- (additional_missing_pct/100) * nrow(im_train_raw)
      trn_n_to_ampute <- round(trn_n_to_ampute)

      if(trn_n_observed < trn_n_to_ampute){

        # ampute nothing, and remove feature from amputed value
        amputed_values[[i]] <- NULL

      } else {

        # make sure there are enough observed values in training data
        observed_index <- which(!is.na(im_train_raw[[i]]))

        ampute_index <- sample(x = observed_index,
                               size = trn_n_to_ampute,
                               replace = FALSE,
                               prob = age_probs[observed_index])

        amputed_values[[i]] <- list(
          index = ampute_index,
          values = im_train_raw[[i]][ampute_index]
        )

        im_train_raw[[i]][ampute_index] <- NA

      }

    }

  }

  prop_miss_train <- prop_miss(im_train_raw)

  # save memory
  rm(im, testing_index)

  message(
    glue(
      "training sample: {nrow(im_train_raw)} ({sum(im_train_raw$status)})",
      " observations ({outcome} events) \n",
      "testing sample: {nrow(im_test_raw)} ({sum(im_test_raw$status)})",
      " observations ({outcome} events) \n",
      "{ncol(im_train_raw)-2} initial predictors"
    )
  )

  # create a pre-processing pipeline to be applied before imputation

  recipe_pre_impute <- recipe(
    formula = time + status ~ .,
    data = im_train_raw
  ) %>%
    #step_corr(all_numeric(), threshold = 0.875) %>%
    step_nzv(all_predictors()) %>%
    step_other(all_nominal(), threshold = 0.025, other = 'Other_value')

  pipe_pre_impute <- prep(recipe_pre_impute)

  im_train_pre_impute <- juice(pipe_pre_impute)
  im_test_pre_impute <- bake(pipe_pre_impute, new_data = im_test_raw)

  # redefine features, since some features may have been dropped by step_nzv
  features <- setdiff(names(im_train_pre_impute), c('time', 'status'))

  # redefine amputed values to account for dropped features
  for(i in names(amputed_values)){
    if(!(i %in% features)) amputed_values[[i]] <- NULL
  }

  # missing value summary
  mv_summary <- miss_var_summary(im_train_pre_impute)

  message("Missing variables in training data")
  print(mv_summary, n = n_features + 2)

  # variables with missing values that miceranger will impute
  miceranger_var_names <- mv_summary %>%
    filter(n_miss > 0) %>%
    pull(variable)

  n_miss_vars <- length(miceranger_var_names)

  miceranger_vars <- vector(mode = 'list', length = n_miss_vars)

  for(i in seq_along(miceranger_vars)){

    names(miceranger_vars)[i] <- miceranger_var_names[i]
    # dont use the outcome to impute a predictor
    miceranger_vars[[i]] <- setdiff(features, miceranger_var_names[i])

  }

  # imputation -----

  pipe_meanmode_impute <- recipe_pre_impute %>%
    step_meanimpute(all_numeric()) %>%
    step_modeimpute(all_nominal()) %>%
    prep()

  # initialize all but fill only the imputations designated by md_strat
  mia <- meanmode_si <-
    pmm_mi <- pmm_si <-
    bayesreg_mi <- bayesreg_si <-
    hotdeck_mi <- hotdeck_si <-
    nbrs_mi <- nbrs_si <-
    ranger_mi <- ranger_si <- NULL

  if('mia' %in% md_strat){

    mia <- tibble(
      impute = 1,
      training = list(im_train_pre_impute),
      testing = list(im_test_pre_impute)
    )

  }
  if('meanmode_si' %in% md_strat){

    meanmode_si <- tibble(
      impute = 1,
      training = list(juice(pipe_meanmode_impute)),
      testing = list(bake(pipe_meanmode_impute, new_data = im_test_raw))
    )

  }
  if('pmm' %in% md_strat) {

    qp = im_train_pre_impute %>%
      select(-time, -status) %>%
      quickpred(mincor = 0.10, minpuc = 0.05)

    pmm_trn <- im_train_pre_impute %>%
      select(-time, -status) %>%
      mice(pred = qp,
           m = n_impute_mi,
           maxit = 10,
           method = 'pmm')

    pmm_tst <- im_test_pre_impute %>%
      select(-time, -status) %>%
      mice(pred = qp,
           m = n_impute_mi,
           maxit = 10,
           method = "pmm")

    pmm_mi <- tibble(
      impute = 1:n_impute_mi,
      training = map(complete(pmm_trn, 'all'), as_tibble),
      testing = map(complete(pmm_tst, 'all'), as_tibble)
    ) %>%
      # bind the outcomes back into the imputed data
      mutate(
        training = map(
          training,
          ~bind_cols(time = im_train_pre_impute$time,
                     status = im_train_pre_impute$status,
                     .x)
        ),
        testing = map(
          testing,
          ~bind_cols(time = im_test_pre_impute$time,
                     status = im_test_pre_impute$status,
                     .x)
        )
      )

    pmm_si <- slice(pmm_mi, 1)

  }
  if('bayesreg' %in% md_strat){

    qp = im_train_pre_impute %>%
      select(-time, -status) %>%
      quickpred(mincor = 0.10, minpuc = 0.05)

    bayesreg_trn <- im_train_pre_impute %>%
      select(-time, -status) %>%
      mice(pred = qp,
           m = n_impute_mi,
           maxit = 10,
           defaultMethod = c("norm","logreg","polyreg","polr"))

    bayesreg_tst <- im_test_pre_impute %>%
      select(-time, -status) %>%
      mice(pred = qp,
           m = n_impute_mi,
           maxit = 10,
           defaultMethod = c("norm","logreg","polyreg","polr"))

    bayesreg_mi <- tibble(
      impute = 1:n_impute_mi,
      training = map(complete(bayesreg_trn, 'all'), as_tibble),
      testing = map(complete(bayesreg_tst, 'all'), as_tibble)
    ) %>%
      mutate(
        training = map(
          training,
          ~bind_cols(time = im_train_pre_impute$time,
                     status = im_train_pre_impute$status,
                     .x)
        ),
        testing = map(
          testing,
          ~bind_cols(time = im_test_pre_impute$time,
                     status = im_test_pre_impute$status,
                     .x)
        )
      )

    bayesreg_si <- slice(bayesreg_mi, 1)

  }
  if('hotdeck_si' %in% md_strat) {

    numeric_features <- im_train_pre_impute %>%
      select(all_of(features)) %>%
      select(where(is.numeric)) %>%
      names()

    hotdeck_trn <- hotdeck_safe(data = im_train_pre_impute,
                                ord_var = numeric_features[1:5],
                                recipe = pipe_meanmode_impute)

    hotdeck_tst <- hotdeck_safe(data = im_test_pre_impute,
                                ord_var = numeric_features[1:5],
                                recipe = pipe_meanmode_impute)

    hotdeck_si <- tibble(
      impute = 1,
      training = list(as_tibble(hotdeck_trn)),
      testing = list(as_tibble(hotdeck_tst))
    )

  }
  if('hotdeck_mi' %in% md_strat) {

    numeric_features <- im_train_pre_impute %>%
      select(all_of(features)) %>%
      select(where(is.numeric)) %>%
      names() %>%
      .[1:n_impute_mi]

    hotdeck_trn <- map(
      .x = numeric_features,
      .f = ~ hotdeck_safe(data = im_train_pre_impute,
                          ord_var = .x,
                          recipe = pipe_meanmode_impute)
    )

    hotdeck_tst <- map(
      .x = numeric_features,
      .f = ~ hotdeck_safe(data = im_test_pre_impute,
                          ord_var = .x,
                          recipe = pipe_meanmode_impute)
    )

    hotdeck_mi <- tibble(
      impute = 1:n_impute_mi,
      training = map(hotdeck_trn, as_tibble),
      testing = map(hotdeck_tst, as_tibble)
    )

  }
  if('nbrs_mi' %in% md_strat){

    nbrs_mi <-
      brew_nbrs(im_train_pre_impute, outcome = c(time, status)) %>%
      spice(k_neighbors = rep(10, 5), aggregate = FALSE) %>%
      verbose_on(level = 2) %>%
      mash() %>%
      stir(timer = TRUE) %>%
      ferment(data_new = im_test_pre_impute) %>%
      bottle() %>%
      tidy_nbrs()

  }
  if('nbrs_si' %in% md_strat){

    nbrs_si <-
      brew_nbrs(im_train_pre_impute, outcome = c(time, status)) %>%
      spice(k_neighbors = 10, aggregate = TRUE) %>%
      verbose_on(level = 2) %>%
      mash() %>%
      stir(timer = TRUE) %>%
      ferment(data_new = im_test_pre_impute) %>%
      bottle() %>%
      tidy_nbrs()

  }
  if('ranger_si' %in% md_strat){

    ranger_si_train <- miceRanger(
      data = im_train_pre_impute,
      m = 1,
      maxiter = 10,
      vars = miceranger_vars,
      returnModels = TRUE,
      verbose = TRUE,
      num.trees = 250
    )

    ranger_si_test <- impute(data = im_test_pre_impute,
                             miceObj = ranger_si_train)

    ranger_si = tidy_ranger(ranger_si_train, ranger_si_test)

    rm(ranger_si_train, ranger_si_test)

  }
  if('ranger_mi' %in% md_strat){

    ranger_mi_train <- miceRanger(
      data = im_train_pre_impute,
      m = n_impute_mi,
      vars = miceranger_vars,
      maxiter = 5,
      meanMatchCandidates = 10,
      returnModels = TRUE,
      verbose = TRUE,
      num.trees = round(250 / n_impute_mi)
    )

    ranger_mi_test <- impute(data = im_test_pre_impute,
                             miceObj = ranger_mi_train)

    ranger_mi <- tidy_ranger(ranger_mi_train, ranger_mi_test)

    rm(ranger_mi_train, ranger_mi_test)

  }

  # save memory
  rm(im_train_pre_impute, im_test_pre_impute)

  # bind the imputations ----

  imputes <- bind_rows(
    mia = mia,
    meanmode_si = meanmode_si,
    pmm_mi = pmm_mi,
    pmm_si = pmm_si,
    bayesreg_mi = bayesreg_mi,
    bayesreg_si = bayesreg_si,
    hotdeck_mi = hotdeck_mi,
    hotdeck_si = hotdeck_si,
    nbrs_mi = nbrs_mi,
    nbrs_si = nbrs_si,
    ranger_si = ranger_si,
    ranger_mi = ranger_mi,
    .id = 'md_strat'
  )


  # score imputations ----
  # default values used for imputation scores
  # if no additional missingness is added

  impute_scores <- tibble(
    md_strat = unique(imputes$md_strat),
    numeric = NaN,
    nominal = NaN
  )

  # score the imputes if there is additional missingness
  if(additional_missing_pct > 0){

    impute_scores <- imputes %>%
      mutate(scores = map(training, score_imputes, amputed_values)) %>%
      unnest_wider(col = scores) %>%
      group_by(md_strat) %>%
      summarize(across(c(nominal, numeric), mean))

  }


  # save memory
  rm(nbrs_mi, nbrs_si)

  # predicting risk for event -----

  recipe_post_impute <-
    recipe(time + status ~ ., data = imputes$training[[1]]) %>%
    step_novel(all_nominal()) %>%
    step_dummy(all_nominal())

  imputes %>%
    group_by(md_strat) %>%
    summarize(
      cph = list(
        predict_risk_cph(
          training = training,
          testing = testing,
          pipeline = recipe_post_impute,
          verbose = FALSE,
          times = times
        )
      ),
      xgb = list(
        predict_risk_xgb(
          training = training,
          testing = testing,
          pipeline = recipe_post_impute,
          verbose = TRUE,
          times = times
        )
      ),
      .groups = 'drop'
    ) %>%
    pivot_longer(
      cols = -c(md_strat),
      values_to = 'sprobs',
      names_to = 'model'
    ) %>%
    left_join(impute_scores, by = 'md_strat') %>%
    mutate(
      iteration = iteration,
      md_strat = md_strat,
      outcome = outcome,
      prop_miss_train = prop_miss_train,
      prop_miss_test = prop_miss_test,
      additional_missing_pct = additional_missing_pct,
      .before = model
    )

}

.rslurm_params <- expand.grid(
  md_strat = c(
    'mia',
    'ranger_si',
    'meanmode_si',
    'ranger_mi',
    'pmm',
    'bayesreg',
    'hotdeck_si',
    'hotdeck_mi',
    'nbrs_mi',
    'nbrs_si'
  ),
  additional_missing_pct = c(0, 15, 30),
  outcome = c('dead', 'txpl'),
  iteration = 1:100
)

.rslurm_id <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))
.rslurm_istart <- .rslurm_id * 1 + 1
.rslurm_iend <- min((.rslurm_id + 1) * 1, nrow(.rslurm_params))

.rslurm_result <- do.call(
  what = parallel::mcmapply,
  args = c(
    FUN = .rslurm_func,
    .rslurm_params[.rslurm_istart:.rslurm_iend, , drop = FALSE],
    mc.cores = 1,
    mc.preschedule = TRUE,
    SIMPLIFY = FALSE
  )
)

saveRDS(
  .rslurm_result,
  file = glue('results/output_{.rslurm_id}.RDS')
)
