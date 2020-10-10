##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @param output_path
##' @param output_pattern
##' @param nruns
##'
##' @title

load_mccv <- function(output_path = "slurm/results",
                      output_pattern = '^output',
                      nruns = 6000) {

  files <- list.files(path = output_path,
                      pattern = output_pattern,
                      full.names = TRUE)

  message("A total of ", length(files), " outputs were identified")

  file_nums <- str_extract_all(files, '\\d') %>%
    map(paste, collapse = '') %>%
    map_dbl(as.numeric)

  message("Unsuccessful slurm runs:",
          paste(setdiff(0:(nruns-1), file_nums), collapse = ', '))

  map_dfr(.x = files, .f = ~ read_rds(.x)[[1]])

}
