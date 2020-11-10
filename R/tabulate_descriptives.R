##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param im
tabulate_descriptives <- function(im) {

  tbl_data <- im %>%
    mutate(
      status = case_when(
        pt_outcome_dead == 1 ~ "Dead",
        pt_outcome_txpl == 1 ~ "Transplant",
        pt_outcome_cess == 1 ~ "Cessation of support",
        TRUE ~ "Censored"
      ),
      demo_race = case_when(
        demo_race_af_amer == 'yes' ~ "Black",
        demo_race_white == 'yes' ~ "White",
        TRUE ~ "Other"
      ),
      demo_gender = fct_recode(
        demo_gender,
        Male = 'male',
        Female = 'female',
      ),
      demo_race = fct_infreq(demo_race),
      pi_hgt_m = pi_hgt_cm / 100,
      pi_bmi = pi_wgt_kg / pi_hgt_m^2,
      pi_device_strategy = fct_recode(
        pi_device_strategy,
        'Destination therapy'  = "destination_therapy_patient_definitely_not_eligible_for_transplant",
        'Bridge to transplant' = "bridge_to_transplant_patient_currently_listed_for_transplant",
        'Bridge to transplant' = "possible_bridge_to_transplant_likely_to_be_eligible",
        'Bridge to transplant' = "possible_bridge_to_transplant_moderate_likelihood_of_becoming_eligible",
        'Bridge to transplant' = "possible_bridge_to_transplant_unlikely_to_become_eligible",
        'Other' = "bridge_to_recovery",
        'Other' = 'rescue_therapy',
        'Other' = 'other_specify'
      ),
      im_device_ty = fct_recode(
        im_device_ty,
        'Left-ventricular assistance device' = 'lvad',
        'Bi-ventricular assistance device'   = 'bivad'
      )
    ) %>%
    select(
      status,
      #months_post_implant,
      demo_age,
      demo_gender,
      demo_race,
      pi_bmi,
      pi_device_strategy,
      pi_lvedd,
      pi_albumin_g_dl,
      pi_creat_mg_dl,
      pi_cv_pres,
      pi_peripheral_edema,
      pi_bun_mg_dl,
      pi_bili_total_mg_dl,
      im_device_ty,
      im_surgery_time,
      im_cpb_time
    )

  rspec <- round_spec() %>%
    round_half_even() %>%
    round_using_magnitude(digits = c(2,2,1,0),
                          breaks = c(1,10,100,Inf))

  tbl_missingness_by_status <- tbl_data %>%
    group_by(status) %>%
    miss_var_summary()

  tbl_missingness_overall <- miss_var_summary(tbl_data) %>%
    mutate(status = 'Overall')

  tbl_missingness <- bind_rows(
    tbl_missingness_overall,
    tbl_missingness_by_status
  ) %>%
    transmute(
      status,
      variable,
      tbl_value = table_glue("{n_miss} ({pct_miss}%)", rspec = rspec),
      label = recode(
        variable,
        demo_age = "Age, years",
        demo_gender = "Sex",
        demo_race = "Race",
        pi_bmi = "Body mass index",
        im_device_ty = "Device type",
        im_surgery_time = 'Surgery time, minutes',
        im_cpb_time = 'CPB time, minutes',
        pi_cv_pres = "CV pressure",
        pi_peripheral_edema = 'Periphal edema',
        pi_lvedd = 'LVEDD',
        pi_device_strategy = "Device strategy",
        pi_albumin_g_dl = 'Urinary albumin, g/dl',
        pi_creat_mg_dl = 'Urinary creatinine, mg/dl',
        pi_bun_mg_dl = 'BUN, mg/dl',
        pi_bili_total_mg_dl = 'Bilirubin levels, mg/dl'
      )
    )

  fig_upset <- tbl_data %>%
    select(demo_age,
           demo_gender,
           demo_race,
           pi_bmi,
           im_device_ty,
           `Surgery time` = im_surgery_time,
           im_cpb_time,
           `CV pressure` = pi_cv_pres,
           `Peripheral edema` = pi_peripheral_edema,
           pi_lvedd,
           pi_device_strategy,
           pi_albumin_g_dl,
           pi_creat_mg_dl,
           pi_bun_mg_dl,
           pi_bili_total_mg_dl)

  tbl_characteristics <- tbl_data %>%
    mutate(`No. of patients` = 1, .before = 1) %>%
    tbl_summary(
      by = status,
      label = list(
        demo_age ~ "Age, years",
        demo_gender ~ "Sex",
        demo_race ~ "Race",
        pi_bmi ~ "Body mass index",
        im_device_ty ~ "Device type",
        im_surgery_time ~ 'Surgery time, minutes',
        im_cpb_time ~ 'CPB time, minutes',
        pi_cv_pres ~ "CV pressure",
        pi_peripheral_edema ~ 'Periphal edema',
        pi_lvedd ~ 'LVEDD',
        pi_device_strategy ~ "Device strategy",
        pi_albumin_g_dl ~ 'Urinary albumin, g/dl',
        pi_creat_mg_dl ~ 'Urinary creatinine, mg/dl',
        pi_bun_mg_dl ~ 'BUN, mg/dl',
        pi_bili_total_mg_dl ~ 'Bilirubin levels, mg/dl'
      ),
      missing = 'no',
      type = list(`No. of patients` ~ 'continuous'),
      statistic = list(
        all_continuous() ~ "{mean} ({sd})",
        all_categorical() ~ "{p}",
        `No. of patients` ~ "{sum}"
      ),
      digits = list(`No. of patients` ~ 0)
    ) %>%
    modify_footnote(update = everything() ~ NA) %>%
    add_overall(last = FALSE, col_label = '**Overall**') %>%
    modify_header(stat_by="**{level}**")

  list(characteristics = tbl_characteristics,
       missingness = tbl_missingness,
       upset = fig_upset)

}
