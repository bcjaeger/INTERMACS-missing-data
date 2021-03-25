##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param im
make_intermacs_fake <- function(im) {

  data_fake <- syn(data = im, method = 'cart', k = nrow(im))

  write_csv(as_tibble(data_fake$syn),
            file = 'doc_ccqo/data_fake.csv')

  data_fake$syn

}
