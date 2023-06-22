daysOfMonth <- function(year, month) {
  days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  day <- days[month]
  if ((year %% 4 == 0 && year %% 100 != 0 || year %% 400 == 0) && month == 2) day <- 29
  day
}

## 连续序列标记
ContinueTag <- function(X) {
  Nind <- length(X)
  if (Nind == 0) {
    return(0)
  } # 如果不存在连续干旱或湿润则返回空值
  if (Nind == 1) {
    return(1)
  } # 如果仅有一天干旱或湿润

  Tag <- numeric(Nind) # 标记第几段连续序列
  nEvent <- 1
  Tag[1] <- 1 # 第几段连续序列

  for (i in 1:(Nind - 1)) {
    if (X[i + 1] != X[i] + 1) nEvent <- nEvent + 1
    Tag[i + 1] <- nEvent
  }
  Tag ## quickly return
}

## 对于温度 the calendar day 90th percentile centred on a 5-day window for the base ref
filterSmooth <- function(x, width = 5) {
  x <- as.numeric(x)
  n <- length(x)
  X_temp <- cbind(x[1:(n - width + 1)], x[2:(n - width + 2)], x[3:(n - width + 3)], x[4:(n - width + 4)], x[5:(n - width + 5)])
  apply(X_temp, 1, mean, na.rm = T)
}

#' clim.quantile
#' 
#' @return 
#' quantiles of `c("Tmax.10th", "Tmax.90th", "Tmin.10th", "Tmin.90th", "RR.95th", "RR.99th")`
#' 
#' @importFrom data.table data.table
#' @export
clim.quantile <- function(d, ref = c(1961, 1990)) {
  data <- SpecialValue(d)
  dates <- d[, make_date(year, month, day)]

  ## 截取标准期数据求分位数值
  date_begin <- make_date(ref[1])
  date_end <- make_date(ref[2], 12, 31)
  inds <- dates >= date_begin & dates <= date_end
  d_ref <- data[inds, ]

  if (length(d_ref) == 0) {
    warning("在选择的标准期内不存在数据！")
    return()
  }

  Tmax.filter <- filterSmooth(d_ref[, 5])
  Tmin.filter <- filterSmooth(d_ref[, 6])

  ## 95th percentile of precipitation on wet days in the ref
  precp <- d_ref[, 7]
  Precp_trim <- precp[precp >= 1.0]
  result <- c(quantile(Tmax.filter, c(0.1, 0.9)), quantile(Tmin.filter, c(0.1, 0.9)), quantile(Precp_trim, c(0.95, 0.99)))
  names(result) <- c("Tmax.10th", "Tmax.90th", "Tmin.10th", "Tmin.90th", "RR.95th", "RR.99th")
  result ## quantile result quickly return
}
