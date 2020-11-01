##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param gt_object
##' @param caption
##' @param label
gt_latex <- function(gt_object, caption, label) {

    header = paste0(
      '\\begin{table} \n \\caption{',caption,'} \n'
    )

    if(!is.null(label)){
      header = paste0(header, '\\label{', label, '} \n')
    }

    gt_object %>%
      as_latex() %>%
      as.character() %>%
      gsub("*", "", ., fixed = T) %>%
      gsub("\\\\ \n\\small \\\\ \n", "", ., fixed = T) %>%
      gsub("\n\\large", "", ., fixed = T) %>%
      gsub("\\captionsetup[table]{labelformat=empty,skip=1pt}",
           "",
           .,
           fixed = T) %>%
      gsub("longtable", "table", ., fixed = T) %>%
      gsub('\\begin{table}',
           '\\begin{table} \n\\begin{tabular}',
           .,
           fixed = T) %>%
      gsub('\\end{table}', '\\end{tabular} \n \\end{table}', ., fixed = T) %>%
      gsub('\\begin{table}', header, ., fixed = T) %>%
      cat()


}
