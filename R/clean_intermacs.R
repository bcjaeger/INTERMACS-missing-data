##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param visit
clean_intermacs <- function(na_string = c('',
                                          ' ',
                                          "NA",
                                          "Missing",
                                          "Unknown",
                                          "Unspecified")) {


  intermacs <- read_csv("data/PIMD_v1.csv",
                        guess_max = 20000,
                        na = na_string)

  im <- intermacs %>%
    # drop the identifier (ends with 'ID') columns.
    select(-ends_with("_ID")) %>%
    clean_names() %>%
    filter(im_impl_yr >=2012) %>%
    remove_constant(na.rm = TRUE, quiet = F) %>%
    remove_empty(which = 'cols') %>%
    remove_empty(which = 'rows') %>%
    mutate(
      pi_primary_dgn = case_when(
        pi_primary_dgn == 1 ~ "cancer",
        pi_primary_dgn %in% c(4:12) ~ 'dilated_myopathy',
        pi_primary_dgn == 13 ~ 'hypertrophic_cardiomyopathy',
        pi_primary_dgn %in% c(14:19) ~ 'restrictive_myopathy',
        pi_primary_dgn == 20 ~ 'valvular_heart_disease',
        pi_primary_dgn %in% c(51:64) ~ 'congenital_heart_disease'
      ),
      across(where(is.character), clean_chr)
    )

  cc     <- names(im)[grep('_cc_', names(im))]
  cc2    <- names(im)[grep('_cc2_', names(im))]
  cc_new <- gsub(pattern = "_cc_",
                 replacement = "",
                 x = cc)

  # Loop through all the items in the CC variables
  # create a new variable without the cc prefix
  for(i in 1:length(cc)){
    im[[cc_new[i]]]<-
      factor(ifelse(im[[cc[i]]]==1 | im[[cc2[i]]]==1, 'yes', 'no'))
  }

  # remove the old variables
  im[,cc]  <- NULL
  im[,cc2] <- NULL

  output <- im %>%
    mutate(across(where(is.character), as.factor))

  # need to save to an immediate file
  # rather than drake format file for slurm runs
  write_csv(output, 'data/intermacs_clean.csv')

  output

}
