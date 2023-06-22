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

  Tmax.filter <- filterSmooth(d_ref$Tmax)
  Tmin.filter <- filterSmooth(d_ref$Tmin)

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


#' clim_index
#' @param q_ref quantile of the reference period
#' 
#' @export
clim_index <- function(d, q_ref) {
  data.frame(index_T(d, q_ref), index_P(d, q_ref))
}

# 关于温度的16个指标 --------------------------------------------------------
#' @rdname clim_index
#' @export
index_T <- function(d, q_ref) {
  Tmin <- d$Tmin
  Tmax <- d$Tmax
  Tavg <- d$Tavg
  prcp <- d$prcp

  FD <- clim.FD(Tmin)
  SU <- clim.SU(Tmax)
  ID <- clim.ID(Tmax)
  TR <- clim.TR(Tmin)
  GSL <- clim.GSL(Tavg)
  TXx <- clim.TXx(Tmax)
  TNx <- clim.TNx(Tmin)
  TXn <- clim.TXn(Tmax)
  TNn <- clim.TNn(Tmin)
  Tthp <- clim.Tthp(Tmax, Tmin, q_ref)
  WSDI <- clim.WSDI(Tmax, q_ref)
  CSDI <- clim.CSDI(Tmin, q_ref)
  DTR <- clim.DTR(Tmax, Tmin)
  cbind(FD, SU, ID, TR, GSL, TXx, TNx, TXn, TNn, Tthp, WSDI, CSDI, DTR) ## quickly return 13 extreme prcp indice
}

# 关于降水的11个指标 ----------------------------------------------------------
#' @rdname clim_index
#' @export
index_P <- function(d, q_ref) {
  prcp <- d$prcp
  Rx <- clim.RX(prcp)
  SDII <- clim.SDII(prcp)
  RRN <- clim.RRN(prcp, nm = c(10, 20, 25))
  CDD <- clim.CDD(prcp, item = "drought")
  CWD <- clim.CDD(prcp, item = "wet")
  Rquantile <- clim.Rquantile(prcp, q_ref[c("RR.95th", "RR.99th")])
  PRCPTOT <- clim.PRCPTOT(prcp)

  data.table(Rx, SDII, RRN, CDD, CWD, Rquantile, PRCPTOT) ## quickly return 13 extreme prcp indice
}
