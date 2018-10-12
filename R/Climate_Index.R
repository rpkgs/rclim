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

daysOfMonth <- function(year, month){
  days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  day <- days[month]
  if((year %% 4 == 0 && year %% 100 != 0 || year %% 400 ==0) && month ==2) day <- 29
  day ## return quickly
}
## 数据格式整理
dataArange_Before <- function(data, Segment = "04-03"){
##data <- [year, month, day, Taver, Tmax, Tmin, Precp];列名无限制
##Segment时间段：生长季则为："04-10";水文年则为："04-03";春季则为"03-05";四月则为"04-04",逐月则为"month"
  # data <- unique(data)  #如果数据有重复则去除
  date_string <- sprintf("%d-%d-%d", data[, 1], data[, 2], data[, 3])
  date_daily <- as.Date(date_string)
  n <- nrow(data)
  ## by year, season,  hydrologic year(generally this.year04 - next.year03)
  # 这个问题出现在一开始数据截取上
  if (Segment == "month"){
    beginMonth <- data[1, 2]
    endMonth <- data[n, 2]
    ## 非完整周期内的数据则进行剔除
    id_begin <- 1; id_end <- n
    if (data[1, 3] != 1)
      id_begin <- which(date_daily == as.Date(sprintf("%d-%d-%d", data[1, 1], beginMonth, daysOfMonth(data[1,1], beginMonth)))) + 1
    ## 数据不是以year-12-31结束
    if (data[n, 3] != daysOfMonth(data[n, 1], endMonth)) 
      id_end <- which(date_daily == as.Date(sprintf("%d-%d-%d", data[n, 1], endMonth, 1))) - 1
    if (length(id_end) == 0 | length(id_begin) == 0 | (id_begin == id_end)) warning("数据长度过短，至少需要一整月！\n")
    data <- data[id_begin:id_end, ]##截除不需要的部分
    date_daily <- date_daily[id_begin:id_end]
    data_trim <- cbind(data, labels = format(date_daily, "%Y-%m"))
  }else{
    ## 按照水文年,输入数据格式如Segment <- "04-10"
    beginMonth <- as.numeric(substr(Segment, 1, 2))
    endMonth <- as.numeric(substr(Segment, 4, 5))
    ## 非完整周期内的数据则进行剔除
    if (!(data[1, 2] == beginMonth & data[1, 3] == 1)) 
      id_begin <- which(date_daily == as.Date(sprintf("%d-%d-01", data[1, 1] + 1, beginMonth)))
    ## 数据不是以year-12-31结束
    if (!(data[n, 2] == endMonth & data[n, 3] == daysOfMonth(data[n, 1], endMonth))) 
      id_end <- which(date_daily == as.Date(sprintf("%d-%d-%d", data[n, 1] - 1, endMonth, daysOfMonth(data[n, 1] - 1, endMonth))))
    if (length(id_end) == 0 | length(id_begin) == 0) warning("数据长度过短，至少需要一整年！\n")
    
    data <- data[c(id_begin:id_end), ]##截除不需要的部分
    #  数据分段标记处理，配合aggregate使用
    if ((endMonth - beginMonth == 11) | (beginMonth - endMonth == 1)){
      Years <- data[, 1]; Months <- data[, 2]
      ##example:时间段"04-03", 则2014.01-03标记为2013年，2014.04-2015.03标记为2014年
      label_func <- function(i) {
        if (Months[i] < beginMonth)
          label = Years[i] - 1
        else
          label = Years[i]
        label##return quickly
      }
      labels <- sapply(1:length(Years), label_func)
      data_trim <- cbind(data, labels)
    }else{
      ##如果beginMonth <= endMonth,如时间段"04-010"， InMonth = 4:10;else时间段"04-02"， InMonth = c(4:12, 1:2); 
      if (beginMonth <= endMonth) InMonth <- beginMonth:endMonth else InMonth <- c(beginMonth:12, 1:endMonth)
      
      Id <- which(as.numeric(data[, 2]) %in% InMonth)
      
      Nind <- length(Id)
      indTag <- numeric(Nind) + data[1, 1] ##所属年份标记
      nEvent <- data[1, 1]                 ##起始年
      
      for (i in 1:(Nind-1)){
        if (Id[i + 1] != Id[i] + 1) nEvent <- nEvent+1
        indTag[i+1] <- nEvent
      }
      data_trim <- cbind(data[Id, ], labels = indTag)
    }
  }
  clim.data <- data.frame(SpecialValue(data_trim), labels = data_trim$labels)
  # colnames(clim.data) <- c("year", "month", "day", "precp", "Taver", "Tmax", "Tmin", "labels")
  clim.data##quickly return,clim.data
}

# 降雨、温度、特殊值标记处理 -----------------------------------------------------------

SpecialValue <- function(X){
  ## 提取对应气象数据
  Precp <- X[, 7]
  Tem_aver <- X[, 4]
  Tem_max <- X[, 5]
  Tem_min <- X[, 6]
  
  # 特殊值标记处理----------------------------------------------------------------- 降水
  Precp[Precp == 32766] = NA
  Precp[Precp == 32700] = 1
  Precp[which(Precp >= 32000)] <- Precp[which(Precp >= 32000)] - 32000  #纯雾露霜
  Precp[which(Precp >= 31000)] <- Precp[which(Precp >= 31000)] - 31000  #雨和雪的总量
  Precp[which(Precp >= 30000)] <- Precp[which(Precp >= 30000)] - 30000  #雪量(仅包括雨夹雪，雪暴）
  Precp <- Precp/10  #origin 0.1mm
  ## 温度
  Tem_max[Tem_max == 32766] = NA
  Tem_min[Tem_min == 32766] = NA
  Tem_max <- Tem_max/10
  Tem_min <- Tem_min/10
  Tem_aver[Tem_aver == 32766] = NA
  Tem_aver <- Tem_aver/10
  
  Xnew <- data.frame(X[, 1:3], Tem_aver, Tem_max, Tem_min, Precp)
  colnames(Xnew) <- c("year", "month", "day", "Taver", "Tmax", "Tmin", "precp");Xnew##quickly return
}

# temperature and Precipitation quantile values ---------------------------

clim.quantile <- function(X, Period = "1961-1990"){
  data <- SpecialValue(X)
  date_string <- sprintf("%d-%d-%d", data[, 1], data[, 2], data[, 3])
  date_daily <- as.Date(date_string)
  data <- zoo(data, date_daily)
  BeginYear <- as.numeric(substr(Period, 1, 4)); EndYear <- as.numeric(substr(Period, 6, 9))
  ## 截取标准期数据求分位数值
  standardPeriod <- seq(as.Date(sprintf("%d-01-01", BeginYear)), as.Date(sprintf("%d-12-31", EndYear)), by = "day")
  data_trim <- window(data, standardPeriod)
  if (length(data_trim) == 0){
    warning("在选择的标准期内不存在数据！")
    return()
  }
  ## 对于温度 the calendar day 90th percentile centred on a 5-day window for the base period
  filterSmooth <- function(x, width = 5){
    x <- as.numeric(x);n <- length(x)
    X_temp <- cbind(x[1:(n - width + 1)], x[2:(n - width + 2)], x[3:(n - width + 3)], x[4:(n - width + 4)], x[5:(n - width + 5)])
    apply(X_temp, 1, mean, na.rm = T)
  }
  
  Tmax.filter <- filterSmooth(data_trim[, 5])
  Tmin.filter <- filterSmooth(data_trim[, 6])
  
  ## 95th percentile of precipitation on wet days in the period
  Precp <- data_trim[, 7]; Precp_trim <- Precp[Precp >= 1.0]
  result <- c(quantile(Tmax.filter, c(0.1, 0.9)), quantile(Tmin.filter, c(0.1, 0.9)), quantile(Precp_trim, c(0.95, 0.99)))
  names(result) <- c("Tmax.10th", "Tmax.90th", "Tmin.10th", "Tmin.90th", "RR.95th", "RR.99th")
  result##quantile result quickly return
}

## 连续序列标记
ContinueTag <- function(X){
  Nind <- length(X)
  if (Nind == 0) return(0) #如果不存在连续干旱或湿润则返回空值
  if (Nind == 1) return(1)  #如果仅有一天干旱或湿润
  
  Tag <- numeric(Nind)   #标记第几段连续序列
  nEvent = 1;Tag[1] <- 1 #第几段连续序列
  
  for (i in 1:(Nind-1)){
    if (X[i+1]!=X[i]+1) nEvent<-nEvent+1
    Tag[i+1] <- nEvent
  }
  Tag##quickly return
}

# 关于温度的16个指标 --------------------------------------------------------------
## 1 FD, number of frost days: Annual count of days when TN (daily minimum temperature) < 0
clim.FD <- function(Tmin) length(which(Tmin < 0))##for Tmin
## 2 SU, Number of summer days: Annual count of days when TX (daily maximum temperature) > 25
clim.SU <- function(Tmax) length(which(Tmax > 25))##for Tmin
## 3 ID,  Number of icing days: Annual count of days when TX (daily maximum temperature) < 0
clim.ID <- function(Tmax) length(which(Tmax < 0))##for Tmin
## 4 TR,   Number of tropical nights: Annual count of days when TN (daily minimum temperature) > 20
clim.TR <- function(Tmin) length(which(Tmin > 20))##for Tmin

## 5 Growing season length: Annual (1st Jan to 31st Dec in Northern Hemisphere (NH), 1st July to 30th 
#  June in Southern Hemisphere (SH)) count between first span of at least 6 days with daily mean 
#  temperature TG>5oC and first span after July 1st (Jan 1st in SH) of 6 days with TG<5oC. 
#  1-6月的第一次至少连续6日平均气温高于定义温度至7-12月第一次至少连续6日平均气温高于定义温度的持续天数
#  生长季指数要求数据为整年，非整年的部分自动截去
#  未记录生长季的起始时间，如有需要call me
clim.GSL <- function(Taver){
  if (length(Taver) < 365) {
    warning("生长季计算只能按照年尺度")
    return(NA)
  }
  Nmid <- floor(length(Taver)/2); N <- length(Taver)
  ## 假定生长季Taver数据是整年输入，则生长季开始时间在1:(n/2)段，结束点在[(n/2)+1]:n段，n表示数据长度
  Id_begin <- which(Taver > 5)
  Tag <- ContinueTag(Id_begin)
  segment.length <- sapply(1:Tag[length(Tag)], function(i) length(which(Tag == i)))
  TagId <- which(segment.length >= 6)[1]            ##如果查找不到则返回空值
  point.begin <- Id_begin[which(Tag == TagId)[1]]   ##生长季开始点需要在7月之前,如果未查找到则为空值，空值报错
  
  Id_end <- which(Taver[(Nmid+1):N] < 5) + Nmid
  Tag <- ContinueTag(Id_end)
  segment.length <- sapply(1:Tag[length(Tag)], function(i) length(which(Tag == i)))
  TagId <- which(segment.length >= 6); TagId <- TagId[1]            ##如果查找不到则返回空值
  point.end <- Id_end[which(Tag == TagId)]; point.end <- point.end[1]##生长季开始点需要在7月之前,如果未查找到则为空值，空值报错
  if ((length(point.begin)==0) | (length(point.end)==0 ))  return(NA)##数据过短
  if (is.na(point.begin) | is.na(point.end))  return(NA)##数据过短.;
  
  if (point.begin >= 183) point.begin <- NA
  if (point.end < 183) end.point <- NA
  if (is.na(point.begin) | is.na(point.end)) warning("生长季的起始时间不在规定范围内，请核对数据！")
  point.end - point.begin##return gsl,如果起始点不在1-6月，结束点不在7-12月则返回空值
}

## 6-9 月极端最高气温、月最低气温极大值、月最高气温极小值、月极端最低气温
#     TXx, Monthly maximum value of daily maximum temperature
#     TNx, Monthly maximum value of daily minimum temperature
#     TXn, Monthly minimum value of daily maximum temperature
#     TNn, Monthly minimum value of daily minimum temperature
clim.TXx <- function(Tmax) max(Tmax, na.rm = T)
clim.TNx <- function(Tmin) max(Tmin, na.rm = T)
clim.TXn <- function(Tmax) min(Tmax, na.rm = T)
clim.TNn <- function(Tmin) min(Tmin, na.rm = T)
##   10-13 TN10p, Percentage of days when TN < 10th percentile
#       TX10p, Percentage of days when TX < 10th percentile
#       TN90p, Percentage of days when TN > 90th percentile
#       TX90p, Percentage of days when TX > 90th percentile
clim.Tthp <- function(Tmax, Tmin, Tquantile = clim_quantile){
  N <- length(Tmax) ##Tmax, Tmin长度需保持一致
  TN10p <- length(which(Tmin < Tquantile["Tmin.10th"]))/N
  TX10p <- length(which(Tmax < Tquantile["Tmax.10th"]))/N
  TN90p <- length(which(Tmin > Tquantile["Tmin.90th"]))/N
  TX90p <- length(which(Tmax > Tquantile["Tmax.90th"]))/N
  data.frame(TN10p, TX10p, TN90p, TX90p)
} 
## 14 WSDI, Warm speel duration index: Annual count of days with at least 6 consecutive days when TX > 90th percentile
clim.WSDI <- function(Tmax, Tquantile = clim_quantile){
  Id <- which(Tmax > Tquantile["Tmax.90th"])
  Tag <- ContinueTag(Id)
  segment.length <- sapply(1:Tag[length(Tag)], function(i) length(which(Tag == i)))
  sum(segment.length[which(segment.length >= 6)])
}
## 15 CSDI, Cold speel duration index: Annual count of days with at least 6 consecutive days when TN < 10th percentile
clim.CSDI <- function(Tmin, Tquantile = clim_quantile){
  Id <- which(Tmin < Tquantile["Tmin.10th"])
  Tag <- ContinueTag(Id)
  segment.length <- sapply(1:Tag[length(Tag)], function(i) length(which(Tag == i)))
  sum(segment.length[which(segment.length >= 6)])
}
## 16 DTR, Daily temperature range: Monthly mean difference between TX and TN
clim.DTR <- function(Tmax, Tmin){
  N <- length(Tmax) ##Tmax, Tmin长度需保持一致
  sum(Tmax - Tmin, na.rm = T)/N ##平均气温日较差
}
# 关于降水的11个指标 --------------------------------------------------------------
##  17-18 n日降水量最大值
clim.RX <- function(precp){
  precp <- as.matrix(precp); Ndays <- length(precp)
  data_R5 <- cbind(precp[1:(Ndays-4)], precp[2:(Ndays-3)],precp[3:(Ndays-2)],precp[4:(Ndays-1)],precp[5:(Ndays)])#采用向后滑动求和
  Rx5 <- max(apply(data_R5, 1, sum, na.rm=T))
  Rx1 <- max(precp)
  data.frame(Rx1 = Rx1, Rx5 = Rx5)#quickly return
}

# 19SDII    日降水量之和与日降水量≥ 1 mm的日数之比
clim.SDII <- function(precp) sum(precp[which(precp >= 1)])/length(which(precp >= 1))

## 20-22降雨量大于n mm的日数
clim.RRN <- function(precp, nm = c(10, 20, 25)){
  rrn <- sapply(nm, function(x) length(which(precp >= x)))
  rrn <- data.frame(t(rrn))
  colnames(rrn) <- paste("RR", nm, sep = "");rrn
}

## 23-24计算连续干旱或连续湿润天数
clim.CDD <- function(precp, item = "drought"){
  if (!item %in% c("drought", "wet")) stop("item param must bu 'drought' or 'wet'!")
  if(item == "drought") cdd.id <- which(precp < 1) else cdd.id <- which(precp >= 1)
  
  Tag <- ContinueTag(cdd.id)
  cdd <- max(sapply(1:Tag[length(Tag)],function(i) length(which(Tag == i)))) ##计算连续干旱天数
  cdd##quickly return
}

## 25-26降雨大于95%、99%百分位值对应的天数与累计降雨量，
clim.Rquantile <- function(precp, quantile.standard){
  R_days <- sapply(1:2, function(i) length(which(precp > quantile.standard[i])))#list1 for q95, list2 for q99, days
  R_precp <- sapply(1:2, function(i) sum(precp[which(precp > quantile.standard[i])]))#list1 for q95, list2 for q99, calculate
  data.frame(R95D = R_days[1], R99D = R_days[2], R95P = R_precp[1], R99p = R_precp[2])##quickly return 
}
# 27PRCPTOT 日降水量≥ 1 mm的降水量之和
clim.PRCPTOT <- function(precp) sum(precp[which(precp >= 1)])


# 综合 ----------------------------------------------------------------------

# 关于温度的16个指标 --------------------------------------------------------
TIndice <- function(X, Quantile = clim_quantile){ ##集总所有极端降水指标
  Tmin <- X$Tmin; Tmax <- X$Tmax; Taver <- X$Taver; precp <- X$precp
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
  cbind(FD, SU, ID, TR, GSL, TXx, TNx, TXn, TNn, Tthp, WSDI, CSDI, DTR)##quickly return 13 extreme precp indice
}
# 关于降水的11个指标 ----------------------------------------------------------
precpIndice <- function(X, Quantile = clim_quantile){ ##集中所有极端降水指标
  precp <- X$precp
  Rx <- clim.RX(precp)
  SDII <- clim.SDII(precp)
  RRN <- clim.RRN(precp, nm = c(10, 20, 25))
  CDD <- clim.CDD(precp, item = "drought")
  CWD <- clim.CDD(precp, item = "wet")
  Rquantile <- clim.Rquantile(precp, Quantile[c("RR.95th", "RR.99th")])
  PRCPTOT <- clim.PRCPTOT(precp)
  data.frame(Rx, SDII, RRN, CDD, CWD, Rquantile, PRCPTOT)##quickly return 13 extreme precp indice
}
allIndice <- function(X, Quantile = clim_quantile) data.frame(TIndice(X, Quantile), precpIndice(X, Quantile))
Clim.Indice <- function(X, quantile = clim_quantile, index = "all"){
  ## index输入：两种情况1.all indice, 2.如c("FD", "SU", ), 
  index <- c("FD", "SU", "ID", "TR", "GSL", "TXx","TNx", "TXn", "TNn", "Tthp", "WSDI", "CSDI") 
}