

the_plan <- drake_plan(

  # when to evaluate models, in months since implant
  times = 6,

  # these two .csv files are needed to complete the slurm runs ----

  # creates intermacs_clean.csv
  im = clean_intermacs(),
  # creates resamples.csv
  resamples = mc_cv_light(data = im, train_prop = 2/3, ntimes = 250),

  # slurm runs need to be completed/cleaned to proceed from here ----
  mc_cv_results = load_mccv(),
  risk_evaluation = make_risk_evaluation(mc_cv_results, im, resamples, times),

  # labels to describe methods ----

  md_method_labels = c(
    'mia' = 'Missingness as an attribute',
    'ranger' = 'Random forest',
    'meanmode' = 'Mean or mode',
    'bayesreg' = 'Bayesian regression',
    'hotdeck' = 'Hot deck',
    'pmm' = 'Predictive mean matching',
    'nbrs' = 'K-nearest-neighbors'
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
    '+10% additional missing data'= '10',
    '+20% additional missing data'= '20'
  ),

  fig_risk_evaluation = visualize_risk_evaluation(risk_evaluation,
                                                  md_method_labels,
                                                  md_type_labels,
                                                  model_labels,
                                                  outcome_labels,
                                                  additional_missing_labels,
                                                  times),


  arxiv_preprint = target(
    command = {
      rmarkdown::render(knitr_in("doc_arXiv/doc_arXiv.Rmd"))
      file_out("doc_arXiv/doc_arXiv.pdf")
    }
  )

  #tbl_characteristics = tabulate_characteristics(im)

)


# risk_evaluation %>%
#   filter(additional_missing_pct == 0) %>%
#   group_by(outcome,
#            md_strat,
#            model) %>%
#   select(starts_with('GND')) %>%
#   summarize(cal_prob = mean(GND_pvalue > 0.05)) %>%
#   ungroup() %>%
#   arrange(desc(cal_prob)) %>%
#   print(n=20)

#
# mc_cv_results %>%
#   select(md_strat,
#          iteration,
#          additional_missing_pct,
#          outcome,
#          numeric,
#          nominal) %>%
#   distinct() %>%
#   group_by(md_strat, additional_missing_pct) %>%
#   summarize(across(c(numeric, nominal), mean, na.rm = TRUE)) %>%
#   pivot_wider(names_from = md_strat, values_from = c(numeric, nominal))
#
# risk_evaluation %>%
#   filter(model == 'xgb') %>%
#   group_by(outcome, md_strat) %>%
#   summarize(across(auc:ipa, mean, na.rm = T)) %>%
#   arrange(outcome, auc) %>%
#   print(n=Inf)

# %>%
#   mutate(kappa = nominal - nominal[method == 'meanmode'],
#          kappa = kappa / (1 - nominal[method == 'meanmode']))

# need to make chicago dot plot with SI --- MI the bands
# md_strat_levels <- c(
#   'mia',
#   'meanmode_si',
#   'hotdeck_si',
#   'pmm_si',
#   'bayesreg_si',
#   'nbrs_si',
#   'ranger_si',
#   'hotdeck_mi',
#   'pmm_mi',
#   'bayesreg_mi',
#   'nbrs_mi',
#   'ranger_mi'
# )
#
# risk_evaluation %>%
#   filter(additional_missing_pct == 20) %>%
#   mutate(md_strat = factor(md_strat, levels = md_strat_levels)) %>%
#   ggplot(aes(x=md_strat, y=ipa)) +
#   stat_summary(fun.data = mean_cl_normal,
#                position = position_dodge(width = 1/2)) +
#   facet_grid(model~outcome, scales = 'free') +
#   coord_flip()
