## 任意时间尺度ETCCDI气候指数
## Writed By kongdd, 2015/06/05
## Sun Yat-Sen University, Guangzhou, China
## email: kongdd@mail2.sysu.edu.cn
## 函数已使用climdex.pcic已经验证，除和T quantile有关的四个TN10p,TX10p,TN90p,TX90p，其余完全一致
## ========================================================================
## 所需计算的各项指标
## 参考文献：
#         [1] 慈晖, 张强, 张江辉, 等. 1961-2010 年新疆极端降水过程时空特征[J]. 地理研究, 2014, 10: 010.
#         [2] Bartholy J, Pongrácz R. Regional analysis of extreme temperature and precipitation indices for the Carpathian basin
#             from 1946 to 2001. Global and Planetary Change, 2007, 57(1-2): 83-95.
#         [3] http://etccdi.pacificclimate.org/list_27_indices.shtml
# =========================================================================
# 关于温度的16个指标 --------------------------------------------------------------
## 1 FD,  Number of frost days: Annual count of days when TN (daily minimum temperature) < 0
## 2 SU,  Number of summer days: Annual count of days when TX (daily maximum temperature) > 25
## 3 ID,  Number of icing days: Annual count of days when TX (daily maximum temperature) < 0
## 4 TR,  Number of tropical nights: Annual count of days when TN (daily minimum temperature) > 20
## 5 GSL, Growing season length: Annual (1st Jan to 31st Dec in Northern Hemisphere (NH), 1st July to 30th
#         June in Southern Hemisphere (SH)) count between first span of at least 6 days with daily mean
#         temperature TG>5oC and first span after July 1st (Jan 1st in SH) of 6 days with TG<5oC.
#         1-6月的第一次至少连续6日平均气温高于定义温度至7-12月第一次至少连续6日平均气温高于定义温度的持续天数
#  6-9 月极端最高气温、月最低气温极大值、月最高气温极小值、月极端最低气温
#   TXx,  Monthly maximum value of daily maximum temperature
#   TNx,  Monthly maximum value of daily minimum temperature
#   TXn,  Monthly minimum value of daily maximum temperature
#   TNn,  Monthly minimum value of daily minimum temperature
#  10-13  TN10p, Percentage of days when TN < 10th percentile
#         TX10p, Percentage of days when TX < 10th percentile
#         TN90p, Percentage of days when TN > 90th percentile
#         TX90p, Percentage of days when TX > 90th percentile
#  14 WSDI, Warm speel duration index: Annual count of days with at least 6 consecutive days when TX > 90th percentile
#  15 CSDI, Cold speel duration index: Annual count of days with at least 6 consecutive days when TN < 10th percentile
#  16 DTR, Daily temperature range: Monthly mean difference between TX and TN
# 关于降水的11个指标 --------------------------------------------------------------
#  17.Rx1day  1 日降水量最大值
#  18.Rx5day  连续5 日降水量最大值
#  19 SDII    日降水量之和与日降水量≥ 1 mm的日数之比
#  20-22.RRn  降雨量大于n mm的日数
#  23.CDD     年内日降水量连续低于1 mm的日数最大值
#  24.CWD     年内日降水量连续高于等于1 mm的日数最大值
#  25-26      降雨大于95%、99%百分位值对应的天数与累计降雨量，
#             R95D     年内日降水量高于标准时段日降水量序列第95 百分位值的日数之和
#             R99D     年内日降水量高于标准时段日降水量序列第99 百分位值的日数之和
#             R95P    年内日降水量高于标准时段日降水量序列第95 百分位值的降水量之和
#             R99P    年内日降水量高于标准时段日降水量序列第99 百分位值的降水量之和
#  27.PRCPTOT 日降水量≥ 1 mm的降水量之和
## ========================================================================

daysOfMonth <- function(year, month) {
  days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  day <- days[month]
  if ((year %% 4 == 0 && year %% 100 != 0 || year %% 400 == 0) && month == 2) day <- 29
  day ## return quickly
}

## 数据格式整理
dataArange_Before <- function(data, Segment = "04-03") {
  ## data <- [year, month, day, Taver, Tmax, Tmin, Precp];列名无限制
  ## Segment时间段：生长季则为："04-10";水文年则为："04-03";春季则为"03-05";四月则为"04-04",逐月则为"month"
  # data <- unique(data)  #如果数据有重复则去除
  date_string <- sprintf("%d-%d-%d", data[, 1], data[, 2], data[, 3])
  date_daily <- as.Date(date_string)
  n <- nrow(data)
  ## by year, season,  hydrologic year(generally this.year04 - next.year03)
  # 这个问题出现在一开始数据截取上
  if (Segment == "month") {
    beginMonth <- data[1, 2]
    endMonth <- data[n, 2]
    ## 非完整周期内的数据则进行剔除
    id_begin <- 1
    id_end <- n
    if (data[1, 3] != 1) {
      id_begin <- which(date_daily == as.Date(sprintf("%d-%d-%d", data[1, 1], beginMonth, daysOfMonth(data[1, 1], beginMonth)))) + 1
    }
    ## 数据不是以year-12-31结束
    if (data[n, 3] != daysOfMonth(data[n, 1], endMonth)) {
      id_end <- which(date_daily == as.Date(sprintf("%d-%d-%d", data[n, 1], endMonth, 1))) - 1
    }
    if (length(id_end) == 0 | length(id_begin) == 0 | (id_begin == id_end)) warning("数据长度过短，至少需要一整月！\n")
    data <- data[id_begin:id_end, ] ## 截除不需要的部分
    date_daily <- date_daily[id_begin:id_end]
    data_trim <- cbind(data, labels = format(date_daily, "%Y-%m"))
  } else {
    ## 按照水文年,输入数据格式如Segment <- "04-10"
    beginMonth <- as.numeric(substr(Segment, 1, 2))
    endMonth <- as.numeric(substr(Segment, 4, 5))
    ## 非完整周期内的数据则进行剔除
    if (!(data[1, 2] == beginMonth & data[1, 3] == 1)) {
      id_begin <- which(date_daily == as.Date(sprintf("%d-%d-01", data[1, 1] + 1, beginMonth)))
    }
    ## 数据不是以year-12-31结束
    if (!(data[n, 2] == endMonth & data[n, 3] == daysOfMonth(data[n, 1], endMonth))) {
      id_end <- which(date_daily == as.Date(sprintf("%d-%d-%d", data[n, 1] - 1, endMonth, daysOfMonth(data[n, 1] - 1, endMonth))))
    }
    if (length(id_end) == 0 | length(id_begin) == 0) warning("数据长度过短，至少需要一整年！\n")

    data <- data[c(id_begin:id_end), ] ## 截除不需要的部分
    #  数据分段标记处理，配合aggregate使用
    if ((endMonth - beginMonth == 11) | (beginMonth - endMonth == 1)) {
      Years <- data[, 1]
      Months <- data[, 2]
      ## example:时间段"04-03", 则2014.01-03标记为2013年，2014.04-2015.03标记为2014年
      label_func <- function(i) {
        if (Months[i] < beginMonth) {
          label <- Years[i] - 1
        } else {
          label <- Years[i]
        }
        label ## return quickly
      }
      labels <- sapply(1:length(Years), label_func)
      data_trim <- cbind(data, labels)
    } else {
      ## 如果beginMonth <= endMonth,如时间段"04-010"， InMonth = 4:10;else时间段"04-02"， InMonth = c(4:12, 1:2);
      if (beginMonth <= endMonth) InMonth <- beginMonth:endMonth else InMonth <- c(beginMonth:12, 1:endMonth)

      Id <- which(as.numeric(data[, 2]) %in% InMonth)

      Nind <- length(Id)
      indTag <- numeric(Nind) + data[1, 1] ## 所属年份标记
      nEvent <- data[1, 1] ## 起始年

      for (i in 1:(Nind - 1)) {
        if (Id[i + 1] != Id[i] + 1) nEvent <- nEvent + 1
        indTag[i + 1] <- nEvent
      }
      data_trim <- cbind(data[Id, ], labels = indTag)
    }
  }
  clim.data <- data.frame(SpecialValue(data_trim), labels = data_trim$labels)
  # colnames(clim.data) <- c("year", "month", "day", "precp", "Taver", "Tmax", "Tmin", "labels")
  clim.data ## quickly return,clim.data
}

# 降雨、温度、特殊值标记处理 -----------------------------------------------------------

SpecialValue <- function(X) {
  ## 提取对应气象数据
  Precp <- X[, 7]
  Tem_aver <- X[, 4]
  Tem_max <- X[, 5]
  Tem_min <- X[, 6]

  # 特殊值标记处理----------------------------------------------------------------- 降水
  Precp[Precp == 32766] <- NA
  Precp[Precp == 32700] <- 1
  Precp[which(Precp >= 32000)] <- Precp[which(Precp >= 32000)] - 32000 # 纯雾露霜
  Precp[which(Precp >= 31000)] <- Precp[which(Precp >= 31000)] - 31000 # 雨和雪的总量
  Precp[which(Precp >= 30000)] <- Precp[which(Precp >= 30000)] - 30000 # 雪量(仅包括雨夹雪，雪暴）
  Precp <- Precp / 10 # origin 0.1mm
  ## 温度
  Tem_max[Tem_max == 32766] <- NA
  Tem_min[Tem_min == 32766] <- NA
  Tem_max <- Tem_max / 10
  Tem_min <- Tem_min / 10
  Tem_aver[Tem_aver == 32766] <- NA
  Tem_aver <- Tem_aver / 10

  Xnew <- data.frame(X[, 1:3], Tem_aver, Tem_max, Tem_min, Precp)
  colnames(Xnew) <- c("year", "month", "day", "Taver", "Tmax", "Tmin", "precp")
  Xnew ## quickly return
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
