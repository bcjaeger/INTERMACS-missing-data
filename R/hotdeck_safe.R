##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param ...
##' @param recipe
hotdeck_safe <- function(..., recipe) {
  bake(recipe, new_data = suppressWarnings(hotdeck(...)))
}
