# This piece of code merges the data you get from three previous steps.

# You should have wireless_only_rates.sas7bdat in your directory.

# You'll get MergeData.RData after you running the code.
library(sas7bdat)
library(maps)

rm(list=ls())
load('Wireless.RData')
load('Pop.RData')
load('PhoneMail.RData')

mg <-merge(phoneall, mailall, all = T, by=c('survey','year','wave','st','mode_fx','est','var','samplesize','cov_37'))



cell <- read.sas7bdat('wireless_only_rates.sas7bdat')
cell[cell$state=='DE'&cell$month==12&cell$year==2009,]$percent_wireless = 15.6


cellpred <- data.frame(cbind(rep(rep(1982:2020,each=6), 17), rep(1:6, 39*17), rep(unique(cell$st), each=234), exp(predvalsp)/(1+exp(predvalsp))))
colnames(cellpred) <- c('year', 'wave', 'st', 'wireless')

mg2 <- merge(mg, pop82to16, by=c('st', 'year'))

mg3 <- merge(mg2, cellpred, by = c('year', 'wave', 'st'))


mg4 <- mg3
mg4[mg4$survey=='mail',]$wireless=0
mg4 <- mg4[!(mg4$year==2011&mg4$wave==5),]
mg4$wirelesscount <- mg4$wireless * mg4$pop


fips <- state.fips[-c(20,22,23,34,36,37,38,40,53,54,56:59),c(1,5)]
fipscode <- fips
colnames(fipscode)[1] <- 'st'
mg4 <- merge(mg4, fipscode, by = 'st' )
mg4 <- mg4[order(mg4[,2], mg4[,1], mg4[,3]),]
mg4$Mail <- mg4$survey == 'mail'

rm(list=setdiff(ls(),'mg4'))
save.image('MergeData.RData')