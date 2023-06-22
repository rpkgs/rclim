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
