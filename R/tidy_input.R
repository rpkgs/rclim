## 数据格式整理
## Segment时间段：
# - 生长季则为："04-10";
# - 水文年则为："04-03";春季则为"03-05";四月则为"04-04",逐月则为"month"


# 降雨、温度、特殊值标记处理 -----------------------------------------------------------
SpecialValue <- function(d) {
  ## 提取对应气象数据
  precp <- d[, 7]
  Tavg <- d[, 4]
  Tmax <- d[, 5]
  Tmin <- d[, 6]

  # 特殊值标记处理----------------------------------------------------------------- 降水
  precp[precp == 32766] <- NA
  precp[precp == 32700] <- 1
  precp[which(precp >= 32000)] <- precp[which(precp >= 32000)] - 32000 # 纯雾露霜
  precp[which(precp >= 31000)] <- precp[which(precp >= 31000)] - 31000 # 雨和雪的总量
  precp[which(precp >= 30000)] <- precp[which(precp >= 30000)] - 30000 # 雪量(仅包括雨夹雪，雪暴）
  precp <- precp / 10 # origin 0.1mm
  
  ## 温度
  Tmax[Tmax == 32766] <- NA
  Tmin[Tmin == 32766] <- NA
  Tmax <- Tmax / 10
  Tmin <- Tmin / 10
  
  Tavg[Tavg == 32766] <- NA
  Tavg <- Tavg / 10

  Xnew <- data.table(d[, 1:3], Tavg, Tmax, Tmin, precp)
  colnames(Xnew) <- c("year", "month", "day", "Tavg", "Tmax", "Tmin", "precp")
  Xnew ## quickly return
}


#' tidy_input
#' @param data A data.frame with the columns of `year, month, day, Tavg, Tmax, Tmin, precp`
#' @export
tidy_input <- function(data, Segment = "04-03") {
  date_string <- sprintf("%d-%d-%d", data[, 1], data[, 2], data[, 3])
  date_daily <- as.Date(date_string)

  n <- nrow(data)
  ## by year, season,  hydrologic year(generally this.year04 - next.year03)

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
        ifelse(Months[i] < beginMonth, Years[i] - 1, Years[i])
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

  # c("year", "month", "day", "precp", "Tavg", "Tmax", "Tmin", "labels")
  cbind(SpecialValue(data_trim), labels = data_trim$labels) # return
}
