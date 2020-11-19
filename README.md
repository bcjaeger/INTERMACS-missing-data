
<!-- README.md is generated from README.Rmd. Please edit that file -->

# INTERMACS-missing-data

[![DOI](https://zenodo.org/badge/298893426.svg)](https://zenodo.org/badge/latestdoi/298893426)

This repository contains all of the code used in the paper *Improving
Outcome Predictions for Patients Receiving Mechanical Circulatory
Support by Optimizing Imputation of Missing Values*.

# ABSTRACT

Risk prediction models play an important role in clinical decision
making. When developing risk prediction models, practitioners often
impute missing values to the mean. We evaluated the impact of applying
other strategies to impute missing values on the prognostic accuracy of
downstream risk prediction models, i.e., models fitted to the imputed
data. A secondary objective was to compare the accuracy of imputation
methods based on artificially induced missing values. To complete these
objectives, we used data from the Interagency Registry for Mechanically
Assisted Circulatory Support (INTERMACS).

## Methods and Results

We applied twelve imputation strategies in combination with two
different modeling strategies for mortality and transplant risk
prediction following surgery to receive mechanical circulatory support.
Model performance was evaluated using Monte-Carlo cross validation and
measured based on outcomes 6-months following surgery using the scaled
Brier score, concordance index, and calibration error. We used Bayesian
hierarchical models to compare model performance. Multiple imputation
with random forests emerged as a robust strategy to impute missing
values, increasing model concordance by 0.003 (25th, 75th percentile:
0.0008, 0.052) compared with imputation to the mean for mortality risk
prediction using a downstream proportional hazards model. The posterior
probability that single and multiple imputation using random forests
would improve concordance versus mean imputation was 0.464 and
&gt;0.999, respectively.

## Conclusion

Selecting an optimal strategy to impute missing values and applying
multiple imputation can improve the prognostic accuracy of downstream
risk prediction models.
