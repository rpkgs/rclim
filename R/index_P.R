# 关于降水的11个指标 --------------------------------------------------------------
##  17-18 n日降水量最大值
#' @export
clim.RX <- function(prcp) {
  Rx5 <- max(roll5(prcp, fun = sum))
  Rx1 <- max(prcp)
  data.frame(Rx1 = Rx1, Rx5 = Rx5)
}

# 19SDII    日降水量之和与日降水量≥ 1 mm的日数之比
#' @export
clim.SDII <- function(prcp) sum(prcp[prcp >= 1]) / sum(prcp >= 1)

## 20-22降雨量大于n mm的日数
#' @export
clim.RRN <- function(prcp, nm = c(10, 20, 25)) {
  nm %<>% set_names(., paste("RR", ., sep = ""))
  ans <- sapply(nm, function(x) sum(prcp >= x))
  as.data.frame(t(ans))
}

## 23-24计算连续干旱或连续湿润天数
#' @export
clim.CDD <- function(prcp, item = "drought") {
  if (!item %in% c("drought", "wet")) stop("item param must bu 'drought' or 'wet'!")
  cdd.id <- if(item == "drought") which(prcp < 1) else which(prcp >= 1)

  Tag <- ContinueTag(cdd.id)
  cdd <- max(sapply(1:Tag[length(Tag)], function(i) sum(Tag == i))) ## 计算连续干旱天数
  cdd
}

## 25-26降雨大于95%、99%百分位值对应的天数与累计降雨量，
#' @export
clim.Rquantile <- function(prcp, q_prcp) {
  R_days <- sapply(1:2, function(i) sum(prcp > q_prcp[i])) # list1 for q95, list2 for q99, days
  R_precp <- sapply(1:2, function(i) sum(prcp[prcp > q_prcp[i]])) # list1 for q95, list2 for q99, calculate

  # print2(prcp, R_days, R_precp)
  data.frame(R95D = R_days[1], R99D = R_days[2], R95P = R_precp[1], R99p = R_precp[2])
}

# 27PRCPTOT 日降水量≥ 1 mm的降水量之和
#' @export
clim.PRCPTOT <- function(prcp) sum(prcp[prcp >= 1])
