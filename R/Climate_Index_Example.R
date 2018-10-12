require(zoo)
require(xlsx)
rm(list = ls())
setwd("G:/数据资料/新疆干旱/27极端气候指标Climate_Index_kongdd")
source('Climate_Index.R', encoding = 'GB2312', echo=TRUE)
## 新疆干旱系统：极端降水指标
# fnames <- dir("G:\\数据资料\\新疆干旱\\xinjia1957-2009ziliao", pattern = "5*.xls", full.names = T)
# data <- read.xlsx2(fnames[1], sheetIndex = 1, colClasses = rep("numeric", 8))[, -1]

load("clim.RData")
data <- unique(clim.OriginalData)  #如果数据有重复则去除
## data <- [year, month, day, Taver, Tmax, Tmin, Precp];列名无限制
#  输入数据需为连续逐日数据，按行排列，允许有空值
##Segment时间段：生长季则为："04-10";水文年则为："04-03";春季则为"03-05";四月则为"04-04",逐月则为"month"

# 按年尺度 --------------------------------------------------------------------
clim_data <- dataArange_Before(clim.OriginalData, Segment = "01-12")
clim_dataLs <- split(clim_data, clim_data$labels) ##把数据按照标记label切分成段

clim_quantile <- clim.quantile(clim.OriginalData, Period = "1961-1990")    ##计算降雨和温度的百分位数
AIndice <-lapply(clim_dataLs, allIndice)                      ##数据整理成dataframe
AIndice <- do.call(rbind.data.frame, AIndice)                   
head(AIndice)
write.xlsx2(AIndice, file = "气候指标.xlsx")
# file.show("气候指标.xlsx")
# # 按月尺度 --------------------------------------------------------------------
# 
# clim_data <- dataArange_Before(data, Segment = "month")
# clim_dataLs <- split(clim_data, clim_data$labels) ##把数据按照标记label切分成段
# 
# clim_quantile <- clim.quantile(data, Period = "1961-1990")    ##计算降雨和温度的百分位数
# AIndice <-lapply(clim_dataLs, allIndice)                      ##数据整理成dataframe
# AIndice <- do.call(rbind.data.frame, AIndice)                   
# head(AIndice)
