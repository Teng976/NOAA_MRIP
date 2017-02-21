# This piece of code reads wireless only households information and extrapolates to wireless only percentage ranging from 1982 to 2020 by a splines model.

# You should have wireless_only_rates.sas7bdat in your working directory.

# You'll get a Wireless.RData after you running the code.


library(sas7bdat)

rm(list=ls())

cell <- read.sas7bdat('wireless_only_rates.sas7bdat')
cell[cell$state=='DE'&cell$month==12&cell$year==2009,]$percent_wireless = 15.6


wireless15 <- c(46,31.1,32.8,51,50.7,43.6,45.2,37.4,34.5,57.7,36.6,27.5,32.7,46.1,37.2,52,43.3)
st <- unique(cell$st)


cell15 <- cbind(wireless15,st,rep(2015,17), rep(12,17))

colnames(cell15)[c(1,3,4)] <- c("percent_wireless", "year", "month")

as.data.frame(cell15)




cellall <- merge(cell[,-3],cell15, all = T, by = c("st", "year", "month", "percent_wireless"))

cellall[,5] <- cellall$year + (cellall$month) / 12

cellall$percent_wireless <- cellall$percent_wireless / 100

colnames(cellall)[5] <- "time"



cellallc <- cbind(rep(unique(cellall$st),each=14),c(rep(2007:2011,each=2),2012:2015), c(rep(c(6,12), 5),rep(6,4)) ,cellall$percent_wireless)

colnames(cellallc) <- c('st', 'year', 'month', 'percent_wireless')

cellallc <- as.data.frame(cellallc)

cellallc[,5] <- cellallc$year + (cellallc$month) / 12

colnames(cellallc)[5] <- "time"

dim(cellallc)



# with data 03 to 05
cell03to05 <- cbind(rep(unique(cellallc$st), 6), rep(2003:2005, each = 17*2), rep(rep(c(6,12),each=17),3), c(0.029 / mean(cellallc[cellallc$time==2007.5, 4]) * cellallc[cellallc$time==2007.5, 4], 0.035 / mean(cellallc[cellallc$time==2007.5, 4]) * cellallc[cellallc$time==2007.5, 4], 0.044 / mean(cellallc[cellallc$time==2007.5, 4]) * cellallc[cellallc$time==2007.5, 4], 0.054 / mean(cellallc[cellallc$time==2007.5, 4]) * cellallc[cellallc$time==2007.5, 4], 0.067 / mean(cellallc[cellallc$time==2007.5, 4]) * cellallc[cellallc$time==2007.5, 4], 0.077 / mean(cellallc[cellallc$time==2007.5, 4]) * cellallc[cellallc$time==2007.5, 4]), rep(seq(2003.5, 2006.0, length.out=6), each = 17))

colnames(cell03to05) <- c('st', 'year', 'month', 'percent_wireless', 'time')

cellalla <- rbind(cell03to05, cellallc)

cellalla <- cellalla[order(cellalla[,1], cellalla[,2], cellalla[,3]),]

rownames(cellalla) <- paste(1:dim(cellalla)[1])

dim(cellalla)

temp <- cellalla$time - 2010
cellalla[,6] <- temp*(temp>0)
colnames(cellalla)[6] <- 'ind'

cell.spline <- lm(log(percent_wireless/(1-percent_wireless)) ~ time * factor(st) + ind * factor(st), data = cellalla)


# prediction from 1982 to 2020, 6 waves each year 39 * 6 = 234
predset <- data.frame(cbind(rep(rep(1982:2020, each = 6) + rep(seq(1,11,length.out=6)/12, (2020-1982+1)), 17), rep(unique(cellalla$st), each = 234)))


colnames(predset) <- c('time', 'st')

predset2 <- cbind(predset, (predset$time-2010)*((predset$time-2010)>0))
colnames(predset2)[3] <- 'ind'

predvalsp <- predict.lm(cell.spline, predset2)

rm(list=setdiff(ls(), 'predvalsp'))

save.image('Wireless.RData')

