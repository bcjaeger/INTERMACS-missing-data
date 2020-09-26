

## library() calls go here
library(conflicted)
library(dotenv)
library(drake)
## data cleaning
library(janitor)
library(naniar)
## imputation
library(ipa)
library(mice)
library(miceRanger)
## reporting
library(glue)
library(table.glue)
## data analysis
library(tidyverse)
library(data.table)
library(magrittr)
library(survival)
library(xgboost)
library(xgboost.surv)
library(tidymodels)
library(randomForestSRC)
library(MASS)
library(VIM)
library(Hmisc)
## model evaluation
library(riskRegression)
library(nricens)

conflict_prefer("roc",       "pROC")
conflict_prefer("complete",  "mice")
conflict_prefer("filter",    "dplyr")
conflict_prefer("select",    "dplyr")
conflict_prefer("slice",     "dplyr")
conflict_prefer('summarise', 'dplyr')
conflict_prefer('summarize', 'dplyr')
conflict_prefer('select',    'dplyr')
conflict_prefer("gather",    "tidyr")
conflict_prefer("set_names", "purrr")
conflict_prefer("matches",   "tidyselect")
conflict_prefer("impute",    "miceRanger")
library(rmarkdown)
