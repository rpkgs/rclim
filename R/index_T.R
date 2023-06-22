# 关于温度的16个指标 --------------------------------------------------------------

## 1 FD, number of frost days: Annual count of days when TN (daily minimum temperature) < 0
#' @export
clim.FD <- function(Tmin) sum(Tmin < 0) ## for Tmin

## 2 SU, Number of summer days: Annual count of days when TX (daily maximum temperature) > 25
#' @export
clim.SU <- function(Tmax) sum(Tmax > 25) ## for Tmin

## 3 ID,  Number of icing days: Annual count of days when TX (daily maximum temperature) < 0
#' @export
clim.ID <- function(Tmax) sum(Tmax < 0) ## for Tmin

## 4 TR,   Number of tropical nights: Annual count of days when TN (daily minimum temperature) > 20
#' @export
clim.TR <- function(Tmin) sum(Tmin > 20) ## for Tmin

## 5 Growing season length: Annual (1st Jan to 31st Dec in Northern Hemisphere (NH), 1st July to 30th
#  June in Southern Hemisphere (SH)) count between first span of at least 6 days with daily mean
#  temperature TG>5oC and first span after July 1st (Jan 1st in SH) of 6 days with TG<5oC.
#  1-6月的第一次至少连续6日平均气温高于定义温度至7-12月第一次至少连续6日平均气温高于定义温度的持续天数
#  生长季指数要求数据为整年，非整年的部分自动截去
#  未记录生长季的起始时间，如有需要call me
clim.GSL <- function(Tavg) {
  if (length(Tavg) < 365) {
    warning("生长季计算只能按照年尺度")
    return(NA)
  }
  Nmid <- floor(length(Tavg) / 2)
  N <- length(Tavg)
  ## 假定生长季Taver数据是整年输入，则生长季开始时间在1:(n/2)段，结束点在[(n/2)+1]:n段，n表示数据长度
  Id_begin <- which(Tavg > 5)
  Tag <- ContinueTag(Id_begin)
  seg.len <- sapply(1:last(Tag), function(i) sum(Tag == i))
  TagId <- which(seg.len >= 6)[1] ## 如果查找不到则返回空值
  point.begin <- Id_begin[which(Tag == TagId)[1]] ## 生长季开始点需要在7月之前,如果未查找到则为空值，空值报错

  Id_end <- which(Tavg[(Nmid + 1):N] < 5) + Nmid
  Tag <- ContinueTag(Id_end)
  seg.len <- sapply(1:last(Tag), function(i) sum(Tag == i))
  TagId <- which(seg.len >= 6)
  TagId <- TagId[1] ## 如果查找不到则返回空值
  point.end <- Id_end[which(Tag == TagId)]
  point.end <- point.end[1] ## 生长季开始点需要在7月之前,如果未查找到则为空值，空值报错
  if ((length(point.begin) == 0) | (length(point.end) == 0)) {
    return(NA)
  } ## 数据过短
  if (is.na(point.begin) | is.na(point.end)) {
    return(NA)
  } ## 数据过短.;

  if (point.begin >= 183) point.begin <- NA
  if (point.end < 183) end.point <- NA
  if (is.na(point.begin) | is.na(point.end)) warning("生长季的起始时间不在规定范围内，请核对数据！")
  point.end - point.begin ## return gsl,如果起始点不在1-6月，结束点不在7-12月则返回空值
}

## 6-9 月极端最高气温、月最低气温极大值、月最高气温极小值、月极端最低气温
#     TXx, Monthly maximum value of daily maximum temperature
#     TNx, Monthly maximum value of daily minimum temperature
#     TXn, Monthly minimum value of daily maximum temperature
#     TNn, Monthly minimum value of daily minimum temperature

#' @export
clim.TXx <- function(Tmax) max(Tmax, na.rm = T)

#' @export
clim.TNx <- function(Tmin) max(Tmin, na.rm = T)

#' @export
clim.TXn <- function(Tmax) min(Tmax, na.rm = T)

#' @export
clim.TNn <- function(Tmin) min(Tmin, na.rm = T)

##   10-13 TN10p, Percentage of days when TN < 10th percentile
#       TX10p, Percentage of days when TX < 10th percentile
#       TN90p, Percentage of days when TN > 90th percentile
#       TX90p, Percentage of days when TX > 90th percentile

#' @export
clim.Tthp <- function(Tmax, Tmin, Tquantile = clim_quantile) {
  N <- length(Tmax) ## Tmax, Tmin长度需保持一致
  TN10p <- sum(Tmin < Tquantile["Tmin.10th"]) / N
  TX10p <- sum(Tmax < Tquantile["Tmax.10th"]) / N
  TN90p <- sum(Tmin > Tquantile["Tmin.90th"]) / N
  TX90p <- sum(Tmax > Tquantile["Tmax.90th"]) / N
  data.frame(TN10p, TX10p, TN90p, TX90p)
}

## 14 WSDI, Warm speel duration index: Annual count of days with at least 6 consecutive days when TX > 90th percentile
#' @export
clim.WSDI <- function(Tmax, Tquantile = clim_quantile) {
  Id <- which(Tmax > Tquantile["Tmax.90th"])
  Tag <- ContinueTag(Id)
  seg.len <- sapply(1:last(Tag), function(i) sum(Tag == i))
  sum(seg.len[seg.len >= 6])
}

## 15 CSDI, Cold speel duration index: Annual count of days with at least 6 consecutive days when TN < 10th percentile
#' @export
clim.CSDI <- function(Tmin, Tquantile = clim_quantile) {
  Id <- which(Tmin < Tquantile["Tmin.10th"])
  Tag <- ContinueTag(Id)
  seg.len <- sapply(1:last(Tag), function(i) sum(Tag == i))
  sum(seg.len[seg.len >= 6])
}

## 16 DTR, Daily temperature range: Monthly mean difference between TX and TN
#' @export
clim.DTR <- function(Tmax, Tmin) {
  N <- length(Tmax) ## Tmax, Tmin长度需保持一致
  sum(Tmax - Tmin, na.rm = T) / N ## 平均气温日较差
}
