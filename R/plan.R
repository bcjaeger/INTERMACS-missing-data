

the_plan <- drake_plan(

  # when to evaluate models, in months since implant
  times = 6,

  # labels to describe methods ----

  md_method_labels = c(
    'meanmode' = 'Imputation to the mean',
    'mia' = 'Missingness as an attribute',
    'hotdeck' = 'Hot deck',
    'nbrs' = 'K-nearest-neighbors',
    'pmm' = 'Predictive mean matching',
    'ranger' = 'Random forest',
    'bayesreg' = 'Bayesian regression'
  ),

  md_type_labels = c(
    'si' = 'Single imputation',
    'mi' = 'Multiple imputation'
  ),

  model_labels = c(
    'rf' = 'Random forest',
    'xgb' = 'Gradient boosted decision trees',
    'cph' = 'Proportional hazards'
  ),

  outcome_labels = c(
    'dead' = 'Mortality',
    'txpl' = 'Transplant'
  ),

  additional_missing_labels = c(
    'No additional missing data'  = '0',
    '+15% additional missing data'= '15',
    '+30% additional missing data'= '30'
  ),

  # rounding specification for the project
  rspec = round_spec() %>%
    round_using_magnitude(digits = c(2, 2, 1),
                          breaks = c(1, 10, 100)) %>%
    round_half_even(),

  # these two .csv files are needed to complete the slurm runs ----

  # creates intermacs_clean.csv
  im = clean_intermacs(),

  # tabulate descriptives
  tbl_descriptives = tabulate_descriptives(im, times),

  # creates resamples.csv
  resamples = mc_cv_light(data = im, train_prop = 1/2, ntimes = 200),

  # slurm runs need to be completed/cleaned to proceed from here ----
  mc_cv_results = load_mccv(),

  tbl_impute_accuracy = tabulate_impute_accuracy(mc_cv_results),

  risk_evaluation = make_risk_evaluation(mc_cv_results, im, resamples, times),

  bayes_mccv_fits = make_bayes_mccv_fit(risk_evaluation),

  fig_risk_evaluation = visualize_risk_evaluation(risk_evaluation,
                                                  md_method_labels,
                                                  md_type_labels,
                                                  model_labels,
                                                  outcome_labels,
                                                  additional_missing_labels,
                                                  times),

  fig_md_strat_infer = visualize_md_strat_inference(bayes_mccv_fits,
                                                    md_type_labels,
                                                    md_method_labels,
                                                    outcome_labels,
                                                    rspec),

  tbl_md_strat = tabulate_md_strat(risk_evaluation,
                                   md_method_labels,
                                   md_type_labels,
                                   model_labels,
                                   outcome_labels,
                                   additional_missing_labels,
                                   rspec),

  gt_tbls = as_gt(tbl_md_strat,
                  model_labels,
                  additional_missing_labels,
                  md_method_labels,
                  rspec),

  arxiv_preprint = target(
    command = {
      rmarkdown::render(knitr_in("doc_arxiv/doc_arxiv.Rmd"))
      file_out("doc_arxiv/doc_arxiv.pdf")
    }
  )


)

# risk_evaluation %>%
#   filter(additional_missing_pct == 0) %>%
#   group_by(model, outcome, md_strat) %>%
#   summarize(across(auc:GND_pvalue, median, na.rm = T))
