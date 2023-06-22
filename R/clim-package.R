#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL


#' clim_index
#' @param q_ref quantile of the reference period
#' 
#' @export
clim_index <- function(d, q_ref = clim_quantile) {
  data.frame(index_T(d, q_ref), index_P(d, q_ref))
}

# 关于温度的16个指标 --------------------------------------------------------
#' @rdname clim_index
#' @export
index_T <- function(d, q_ref) {
  Tmin <- d$Tmin
  Tmax <- d$Tmax
  Tavg <- d$Tavg
  precp <- d$precp

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
  cbind(FD, SU, ID, TR, GSL, TXx, TNx, TXn, TNn, Tthp, WSDI, CSDI, DTR) ## quickly return 13 extreme precp indice
}

# 关于降水的11个指标 ----------------------------------------------------------
#' @rdname clim_index
#' @export
index_P <- function(d, q_ref) {
  precp <- d$precp
  Rx <- clim.RX(precp)
  SDII <- clim.SDII(precp)
  RRN <- clim.RRN(precp, nm = c(10, 20, 25))
  CDD <- clim.CDD(precp, item = "drought")
  CWD <- clim.CDD(precp, item = "wet")
  Rquantile <- clim.Rquantile(precp, q_ref[c("RR.95th", "RR.99th")])
  PRCPTOT <- clim.PRCPTOT(precp)

  data.table(Rx, SDII, RRN, CDD, CWD, Rquantile, PRCPTOT) ## quickly return 13 extreme precp indice
}
