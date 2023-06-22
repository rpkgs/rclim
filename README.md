# clim

27 Extreme climate index defined by ETCCDI (<http://etccdi.pacificclimate.org/list_27_indices.shtml>).

```R
# remotes::install_github("pacificclimate/climdex.pcic")
# remotes::install_github("rpkgs/clim")
library(clim)
library(Ipaper)

df = climdata_raw %>% SpecialValue()
# climdata_raw = fread("data-raw/climdata_raw.csv") %>% unique()

d <- df[year == 1958, ]
head(d)

q_ref <- clim.quantile(df, ref = c(1961, 1990)) 
r <- clim_index(d, q_ref)
# index_P(d, q_ref)
# index_T(d, q_ref)
r
```

   FD SU  ID TR GSL TXx  TNx TXn   TNn     TN10p      TX10p      TN90p      TX90p WSDI CSDI      DTR  Rx1
1 187 57 124  0 204  33 17.2 -26 -40.2 0.1342466 0.08219178 0.04109589 0.03287671    0    9 11.77589 16.2
   Rx5     SDII RR10 RR20 RR25 CDD CWD R95D R99D R95P R99p PRCPTOT
1 26.3 3.628571    3    0    0  27   3    2    0 28.4    0   228.6


```r
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
```

# References

1. <https://github.com/ECCC-CDAS/RClimDex>

2. <https://github.com/pacificclimate/climdex.pcic>

3. 慈晖, 张强, 张江辉, 等. 1961-2010 年新疆极端降水过程时空特征[J]. 地理研究, 2014, 10:010.

4. Bartholy J, Pongrácz R. Regional analysis of extreme temperature and
   precipitation indices for the Carpathian basin from 1946 to 2001. Global and
   Planetary Change, 2007, 57(1-2): 83-95.
