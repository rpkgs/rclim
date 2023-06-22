# 关于降水的11个指标 --------------------------------------------------------------
##  17-18 n日降水量最大值
#' @export
clim.RX <- function(precp) {
  precp <- as.matrix(precp)
  Ndays <- length(precp)
  data_R5 <- cbind(precp[1:(Ndays - 4)], precp[2:(Ndays - 3)], precp[3:(Ndays - 2)], precp[4:(Ndays - 1)], precp[5:(Ndays)]) # 采用向后滑动求和
  Rx5 <- max(apply(data_R5, 1, sum, na.rm = T))
  Rx1 <- max(precp)
  data.frame(Rx1 = Rx1, Rx5 = Rx5) # quickly return
}

# 19SDII    日降水量之和与日降水量≥ 1 mm的日数之比
#' @export
clim.SDII <- function(precp) sum(precp[which(precp >= 1)]) / length(which(precp >= 1))

## 20-22降雨量大于n mm的日数
#' @export
clim.RRN <- function(precp, nm = c(10, 20, 25)) {
  rrn <- sapply(nm, function(x) length(which(precp >= x)))
  rrn <- data.frame(t(rrn))
  colnames(rrn) <- paste("RR", nm, sep = "")
  rrn
}

## 23-24计算连续干旱或连续湿润天数
#' @export
clim.CDD <- function(precp, item = "drought") {
  if (!item %in% c("drought", "wet")) stop("item param must bu 'drought' or 'wet'!")
  if (item == "drought") cdd.id <- which(precp < 1) else cdd.id <- which(precp >= 1)

  Tag <- ContinueTag(cdd.id)
  cdd <- max(sapply(1:Tag[length(Tag)], function(i) length(which(Tag == i)))) ## 计算连续干旱天数
  cdd ## quickly return
}

## 25-26降雨大于95%、99%百分位值对应的天数与累计降雨量，
#' @export
clim.Rquantile <- function(precp, quantile.standard) {
  R_days <- sapply(1:2, function(i) length(which(precp > quantile.standard[i]))) # list1 for q95, list2 for q99, days
  R_precp <- sapply(1:2, function(i) sum(precp[which(precp > quantile.standard[i])])) # list1 for q95, list2 for q99, calculate
  data.frame(R95D = R_days[1], R99D = R_days[2], R95P = R_precp[1], R99p = R_precp[2]) ## quickly return
}

# 27PRCPTOT 日降水量≥ 1 mm的降水量之和
#' @export
clim.PRCPTOT <- function(precp) sum(precp[which(precp >= 1)])


# 综合 ----------------------------------------------------------------------

# 关于温度的16个指标 --------------------------------------------------------
#' @export
TIndice <- function(X, Quantile = clim_quantile) { ## 集总所有极端降水指标
  Tmin <- X$Tmin
  Tmax <- X$Tmax
  Taver <- X$Taver
  precp <- X$precp
  FD <- clim.FD(Tmin)
  SU <- clim.SU(Tmax)
  ID <- clim.ID(Tmax)
  TR <- clim.TR(Tmin)
  GSL <- clim.GSL(Taver)
  TXx <- clim.TXx(Tmax)
  TNx <- clim.TNx(Tmin)
  TXn <- clim.TXn(Tmax)
  TNn <- clim.TNn(Tmin)
  Tthp <- clim.Tthp(Tmax, Tmin, Quantile)
  WSDI <- clim.WSDI(Tmax, Quantile)
  CSDI <- clim.CSDI(Tmin, Quantile)
  DTR <- clim.DTR(Tmax, Tmin)
  cbind(FD, SU, ID, TR, GSL, TXx, TNx, TXn, TNn, Tthp, WSDI, CSDI, DTR) ## quickly return 13 extreme precp indice
}

# 关于降水的11个指标 ----------------------------------------------------------
#' @export
precpIndice <- function(X, Quantile = clim_quantile) { ## 集中所有极端降水指标
  precp <- X$precp
  Rx <- clim.RX(precp)
  SDII <- clim.SDII(precp)
  RRN <- clim.RRN(precp, nm = c(10, 20, 25))
  CDD <- clim.CDD(precp, item = "drought")
  CWD <- clim.CDD(precp, item = "wet")
  Rquantile <- clim.Rquantile(precp, Quantile[c("RR.95th", "RR.99th")])
  PRCPTOT <- clim.PRCPTOT(precp)
  data.frame(Rx, SDII, RRN, CDD, CWD, Rquantile, PRCPTOT) ## quickly return 13 extreme precp indice
}
