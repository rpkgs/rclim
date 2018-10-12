require(zoo)
require("PCICt")
require(xlsx)
require(climdex.pcic)

# load("clim.RData")
data <- read.table("clim_data.txt")

tmax.dates <- as.PCICt(do.call(paste, data[,1:3]), format = "%Y%m%d", cal = "gregorian")
tmin.dates <- as.PCICt(do.call(paste, data[,1:3]), format = "%Y%m%d", cal = "gregorian")
tavg.dates <- as.PCICt(do.call(paste, data[,1:3]), format = "%Y%m%d", cal = "gregorian")
prec.dates <- as.PCICt(do.call(paste, data[,1:3]), format = "%Y%m%d", cal = "gregorian")

ci <- climdexInput.raw(data[, 5], data[, 6], data[, 7], tmax.dates, tmin.dates, prec.dates, base.range = c(1961, 1990),
                       tavg = data[, 4], tavg.dates = tavg.dates)

rx1day <- climdex.rx1day(ci, freq = "annual")
rx5day <- climdex.rx5day(ci, freq = "annual")

r10 <- climdex.r10mm(ci)

CDD <- climdex.cdd(ci, spells.can.span.years = FALSE)

CWD <- climdex.cwd(ci, spells.can.span.years = FALSE)

PRCPTOT <- climdex.prcptot(ci)


climdex.csdi(ci)

climdex.fd(ci)
climdex.su(ci)
climdex.id(ci)

climdex.gsl(ci)
Taver <- data[792:(792+365), 4]
plot(Taver)
abline(h = 5, col = "red")


# temp --------------------------------------------------------------------

  temp.data <- Taver
  ts.mid <- floor(365/2)

  ts.len<- length(temp.data)
  gs.begin <- which(select.blocks.gt.length(temp.data[1:(ts.mid-1)] > 5, 6 - 1))
  
  ## Growing season actually ends the day -before- the sequence of sketchy days
  gs.end <- which(select.blocks.gt.length(temp.data[ts.mid:ts.len] < 5, 6 - 1)) - 1
  
  ## If no growing season start, 0 length; if no end, ends at end of year; otherwise, end - start + 1
#   return(ifelse(length(gs.begin) == 0, 0, ifelse(length(gs.end) == 0, ts.len - gs.begin[1] + 1,
#                                                  gs.end[1] - gs.begin[1] + ts.mid)))
