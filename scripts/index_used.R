# consecutive dry days,< 1mm
CDD <- function(x) {
  if (length(x[!is.na(x)]) == 0) { # if it's all NA, return NA
    return(NA)
  } else {
    x <- x[!is.na(x)] # remove NA
    n <- 0 # duration of days
    m <- 0 # matrix position
    maxdays <- matrix()
    for (item in x) {
      m <- m + 1
      if (item < 1) {
        n <- n + 1
        maxdays[m] <- n
      } else {
        maxdays[m] <- 0
        n <- 0
      }
    }
    return(max(maxdays))
  }
}

# the days of P >=20mm/day in the year
R20 <- function(x) {
  if (length(x[!is.na(x)]) == 0) { # if it's all NA, return NA
    return(NA)
  } else {
    n <- 0 # wetdays
    for (item in x) {
      if (is.na(item)) {
        next
      } else if (item >= 20) {
        n <- n + 1
      }
    }
    return(n)
  }
}


# Rx5day,每月连续5日最大降水量，na设为0,这里暂且算的是年尺度的
Rx5day <- function(x) {
  if (length(x[!is.na(x)]) == 0) { # 如果全是NA,则返回NA
    return(NA)
  } else {
    x <- x[!is.na(x)]
    m <- 0
    j <- length(x) - 4
    for (n in 1:j) {
      if ((x[n] + x[n + 1] + x[n + 2] + x[n + 3] + x[n + 4]) >= m) {
        m <- x[n] + x[n + 1] + x[n + 2] + x[n + 3] + x[n + 4]
      }
    }
    return(m)
  }
}

# 大于95%分位数的总的降雨量/年总降水量
R95pTOT <- function(x) {
  if (length(x[!is.na(x)]) == 0) { # 如果全是NA,则返回NA
    return(NA)
  } else {
    x <- x[which(x >= 1)]
    p95 <- quantile(x, c(0.95))
    m <- 0 # R95p
    n <- 0 # total pre
    for (item in x) {
      if (item > p95) {
        m <- m + item
      }
      n <- n + item
    }
    if (n == 0) {
      return(0)
    } else {
      return(m / n)
    }
  }
}

# Annual total precipitation from days ≥ 1 mm
PRCPTOT <- function(x) {
  if (length(x[!is.na(x)]) == 0) { # if it's all NA, return NA
    return(NA)
  } else {
    return(sum(x[which(x >= 1)]))
  }
}
