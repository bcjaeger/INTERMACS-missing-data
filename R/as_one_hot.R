##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data
as_one_hot <- function (data){

  output_fun <- switch (class(data)[1],
                        'data.frame' = as.data.frame,
                        'matrix' = as.matrix,
                        'tbl_df' = tibble::as_tibble,
                        'data.table' = function(x) x,
                        stop("unrecognized type for data", call. = FALSE)
  )

  if(!is.data.table(data)){
    DT <- as.data.table(data)
  } else {
    DT <- copy(data)
  }

  if(any(sapply(DT, is.character))){

    chr_cols <- names(DT)[sapply(DT, is.character)]

    for(col in chr_cols)
      data.table::set(DT, j = col, value = as.factor(DT[[col]]))

  }

  # Will use these original names to help re-order the output
  DT_names <- names(DT)

  # will use the factor info about DT to connect
  # one-hot columns to original factors
  fctr_info <- get_factor_info(DT)

  for(i in seq_along(fctr_info$cols)){

    # the idea is to make a matrix for each factor
    # with nrow = nrow(DT) and ncol = length of factor levels.
    mat <- matrix(0,
                  nrow = nrow(DT),
                  ncol = length(fctr_info$lvls[[i]])
    )

    colnames(mat) <- fctr_info$keys[[i]]

    # missing values of the factor become missing rows
    mat[is.na(DT[[fctr_info$cols[i]]]), ] <- NA_integer_

    # we will one-hot encode the matrix and then bind it to DT,
    # replacing the original factor column. Go through the matrix
    # column by column, where each column corresponds to a level
    # of the current factor (indexed by i). Flip the values
    # of the j'th column to 1 whenever the current factor's value
    # is the j'th level.

    for (j in seq(ncol(mat))) {

      # find which rows to turn into 1's. These should be the
      # indices in the currect factor where it's value is equal
      # to the j'th level.
      hot_rows <- which(
        DT[[fctr_info$cols[i]]] == fctr_info$lvls[[i]][j]
      )

      # after finding the rows, flip the values from 0 to 1
      if(!purrr::is_empty(hot_rows)){
        mat[hot_rows , j] <- 1
      }

    }


    DT[, fctr_info$cols[i]] <- NULL

    DT <- cbind(DT, mat)

  }

  OH_names <- DT_names

  for (i in seq_along(fctr_info$cols)){

    OH_names <- insert_vals(
      vec = OH_names,
      where = which(fctr_info$cols[i] == OH_names),
      what = fctr_info$keys[[i]]
    )

  }

  data.table::setcolorder(DT, OH_names)

  output_fun(DT)

}

get_factor_info <- function(data){

  is_cat <- function(x) is.factor(x) & !is.ordered(x)

  fctr_info <- vector(mode = 'list', length = 3L)
  names(fctr_info) <- c('cols', 'lvls', 'keys')

  fctr_info$cols <- names(data)[which(sapply(data, is_cat))]

  fctr_info$lvls <- fctr_info$cols %>%
    purrr::set_names() %>%
    purrr::map(~levels(data[[.x]]))

  fctr_info$keys <- purrr::map2(
    purrr::set_names(fctr_info$cols),
    fctr_info$lvls,
    paste,
    sep = '..'
  )

  fctr_info

}

insert_vals <- function(vec, where, what){

  stopifnot(
    typeof(what) == typeof(vec),
    where >= 1 & where <= length(vec)
  )

  if(where == 1){

    if(length(vec) == 1) return(c(what)) else return(c(what, vec[-1]))
  }

  if(where == length(vec)) return(c(vec[1:(length(vec)-1)], what))

  vec_left <- vec[1:(where-1)]
  vec_right <- vec[(where+1):length(vec)]

  c(vec_left, what, vec_right)

}
