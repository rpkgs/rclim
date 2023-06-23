mean_new <- function(x) {
  if (length(x[!is.na(x)]) == 0) {
    return(NA)
  } else {
    return(mean(x, na.rm = T))
  }
}

# CWD降水量连续大于等于1mm的最长天数
CWD <- function(x) {
  if (length(x[!is.na(x)]) == 0) { # 如果全是NA,则返回NA
    return(NA)
  } else {
    x <- x[!is.na(x)] # 把NA值去掉
    n <- 0 # 持续天数
    m <- 0 # 矩阵位置
    maxdays <- matrix()
    for (item in x) {
      m <- m + 1
      if (item >= 1) {
        n <- n + 1
        maxdays[m] <- n
      } else {
        maxdays[m] <- 0
        n <- 0
      }
    }
    return(max(maxdays, na.rm = TRUE))
  }
}

# Max 1 day precipitation amount
Rx1day <- function(x) {
  if (length(x[!is.na(x)]) == 0) { # if it's all NA, return NA
    return(NA)
  } else {
    return(max(x, na.rm = T))
  }
}


# annual total precipitation form days >95th percentile
R95p <- function(x) {
  if (length(x[!is.na(x)]) == 0) { # if it's all NA, return NA
    return(NA)
  } else {
    x <- x[which(x >= 1)]
    p95 <- quantile(x, c(0.95))
    m <- 0
    for (item in x) {
      if (item > p95) {
        m <- m + item
      }
    }
    return(m)
  }
}



# 总的降水量(日降水大于99%分位数时)
R99p <- function(x) {
  if (length(x[!is.na(x)]) == 0) { # 如果全是NA,则返回NA
    return(NA)
  } else {
    x <- x[which(x >= 1)]
    p99 <- quantile(x, c(0.99))
    m <- 0
    for (item in x) {
      if (item > p99) {
        m <- m + item
      }
    }
    return(m)
  }
}

# 大于99%分位数的总的降雨量/年总降水量
R99pTOT <- function(x) {
  if (length(x[!is.na(x)]) == 0) { # 如果全是NA,则返回NA
    return(NA)
  } else {
    x <- x[which(x >= 1)]
    p99 <- quantile(x, c(0.99))
    m <- 0 # R99p
    n <- 0 # total pre
    for (item in x) {
      if (item > p99) {
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

# 日降水强度(大多数1mm)
SDII <- function(x) {
  if (length(x[!is.na(x)]) == 0) { # 如果全是NA,则返回NA
    return(NA)
  } else {
    n <- 0 # wetdays
    m <- 0 # totalpre
    for (item in x) {
      if (is.na(item)) {
        next
      } else if (item >= 1) {
        m <- m + item
        n <- n + 1
      }
    }
    if (m == 0) { # 整年都没有降水
      return(0)
    } else {
      return(m / n)
    }
  }
}

# 中雨日数，the days of P >=10mm/day in the year，na设为0
R10 <- function(x) {
  if (length(x[!is.na(x)]) == 0) { # 如果全是NA,则返回NA
    return(NA)
  } else {
    n <- 0 # wetdays
    for (item in x) {
      if (is.na(item)) {
        next
      } else if (item >= 10) {
        n <- n + 1
      }
    }
    return(n)
  }
}


# 降雨1%分位数
R01_percentile <- function(x) {
  a <- quantile(x, c(0.01), na.rm = TRUE)
  return(a)
}

# 降雨5%分位数
R05_percentile <- function(x) {
  a <- quantile(x, c(0.05), na.rm = TRUE)
  return(a)
}

# 特殊情况
R05_percentile_rm <- function(x) {
  x <- x[which(x > 0)]
  a <- quantile(x, c(0.05), na.rm = TRUE)
  return(a)
}

# 降雨95%分位数
R95_percentile <- function(x) {
  a <- quantile(x, c(0.95), na.rm = TRUE)
  return(a)
}

# 降雨99%分位数
R99_percentile <- function(x) {
  a <- quantile(x, c(0.99), na.rm = TRUE)
  return(a)
}
