##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param im
tabulate_characteristics <- function(im) {

  tbl <- im %>%
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
      months_post_implant,
      demo_age,
      demo_gender,
      demo_race,
      pi_bmi,
      pi_device_strategy,
      im_device_ty
    ) %>%
    mutate(`No. of patients` = 1, .before = 1) %>%
    tbl_summary(
      by = status,
      label = list(
        months_post_implant ~ "Follow up time, months",
        demo_age ~ "Age, years",
        demo_gender ~ "Sex",
        demo_race ~ "Race",
        pi_bmi ~ "Body mass index",
        pi_device_strategy ~ "Device strategy",
        im_device_ty ~ "Device type"
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

}
