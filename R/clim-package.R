#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL


#' @export
allIndice <- function(X, Quantile = clim_quantile) data.frame(TIndice(X, Quantile), precpIndice(X, Quantile))

#' @export
Clim.Indice <- function(X, quantile = clim_quantile, index = "all") {
  ## index输入：两种情况1.all indice, 2.如c("FD", "SU", ),
  index <- c("FD", "SU", "ID", "TR", "GSL", "TXx", "TNx", "TXn", "TNn", "Tthp", "WSDI", "CSDI")
}
