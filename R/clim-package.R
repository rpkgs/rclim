#' @import magrittr
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL


#' clim.quantile
#'
#' @return
#' quantiles of `c("Tmax.10th", "Tmax.90th", "Tmin.10th", "Tmin.90th", "RR.95th", "RR.99th")`
#'
#' @importFrom data.table data.table
#' @export
clim.quantile <- function(df, ref = c(1961, 1990)) {
  dates <- df[, make_date(year, month, day)]

  date_begin <- make_date(ref[1])
  date_end <- make_date(ref[2], 12, 31)
  inds <- dates >= date_begin & dates <= date_end
  d_ref <- df[inds, ]

  if (length(d_ref) == 0) {
    warning("在选择的标准期内不存在数据！")
    return()
  }

  Tmax.filter <- roll5(d_ref$Tmax)
  Tmin.filter <- roll5(d_ref$Tmin)

  ## 95th percentile of precipitation on wet days in the ref
  prcp <- d_ref$prcp
  Precp_trim <- prcp[prcp >= 1.0]
  ans <- c(
    quantile(Tmax.filter, c(0.1, 0.9)),
    quantile(Tmin.filter, c(0.1, 0.9)),
    quantile(Precp_trim, c(0.95, 0.99))
  )
  setNames(ans, c("Tmax.10th", "Tmax.90th", "Tmin.10th", "Tmin.90th", "RR.95th", "RR.99th"))
}
