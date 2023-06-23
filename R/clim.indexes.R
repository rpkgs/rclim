#' clim_index
#' @param q_ref quantile of the reference period
#' 
#' @export
clim.indexes <- function(df) {
  info <- df[, .N, year][N >= 365 * 0.9, ]
  df2 <- df %>% merge(info[, .(year)])

  lst <- df2 %>% split(df2$year)
  q_ref <- clim.quantile(df2, ref = c(1961, 1990))

  res <- purrr::map(lst, ~ clim.indexes_year(.x, q_ref))
  d_index <- Ipaper::melt_list(res, "year")
}

#' @rdname clim.indexes
clim.indexes_year <- function(d, q_ref) {
  data.table(index_T(d, q_ref), index_P(d, q_ref))
}

# 关于温度的16个指标 --------------------------------------------------------
#' @rdname clim.indexes
#' @export
index_T <- function(d, q_ref) {
  Tmin <- d$Tmin
  Tmax <- d$Tmax
  Tavg <- d$Tavg
  # prcp <- d$prcp

  FD   <- clim.FD(Tmin)
  SU   <- clim.SU(Tmax)
  ID   <- clim.ID(Tmax)
  TR   <- clim.TR(Tmin)
  GSL  <- clim.GSL(Tavg)
  TXx  <- clim.TXx(Tmax)
  TNx  <- clim.TNx(Tmin)
  TXn  <- clim.TXn(Tmax)
  TNn  <- clim.TNn(Tmin)
  Tthp <- clim.Tthp(Tmax, Tmin, q_ref)
  WSDI <- clim.WSDI(Tmax, q_ref)
  CSDI <- clim.CSDI(Tmin, q_ref)
  DTR  <- clim.DTR(Tmax, Tmin)
  data.table(FD, SU, ID, TR, GSL, TXx, TNx, TXn, TNn, Tthp, WSDI, CSDI, DTR)
}

# 关于降水的11个指标 ----------------------------------------------------------
#' @rdname clim.indexes
#' @export
index_P <- function(d, q_ref) {
  prcp      <- d$prcp
  Rx        <- clim.RX(prcp)
  SDII      <- clim.SDII(prcp)
  RRN       <- clim.RRN(prcp, nm = c(10, 20, 25))
  CDD       <- clim.CDD(prcp, item = "drought")
  CWD       <- clim.CDD(prcp, item = "wet")
  Rquantile <- clim.Rquantile(prcp, q_ref[c("RR.95th", "RR.99th")])
  PRCPTOT   <- clim.PRCPTOT(prcp)

  data.table(Rx, SDII, RRN, CDD, CWD, Rquantile, PRCPTOT)
}
