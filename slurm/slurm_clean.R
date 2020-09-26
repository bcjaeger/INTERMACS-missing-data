
library(tidyverse)
library(glue)

files <- list.files(path = "slurm/results",
                    pattern = '^output',
                    full.names = TRUE)

file_nums <- str_extract_all(files, '\\d') %>%
  map(paste, collapse = '') %>%
  map_dbl(as.numeric)

message("Unsuccessful slurm runs:",
        paste(setdiff(0:2999, file_nums), collapse = ', '))

output_cleaned <- map_dfr(
  .x = files,
  .f = ~ read_rds(.x)[[1]]
)

write_rds(output_cleaned, 'slurm/sim_results.rds')
