# This file reads the combined dataset and the design variance estimates and gives the Fay-Herriot fit for mode 3 and mode 7 separately. 

# You'll get FH.RData after running this.

library(sae)

rm(list=ls())
load("MergeData.RData")
load('DesignVar.RData')
ls()
names(mg4)
Time<-as.numeric(mg4$year)+(as.numeric(mg4$wave)-1)/6
Phone<-(mg4$survey=="phone")
Mail<-!Phone


######## up to 2015 
# mg4 <- mg4[order(mg4$survey, mg4$year),]
# mgphone <- mg4[-c(1:172),]
# mgmail <- mg4[1:172,]

# #phone data missing two points RI wave 2 mode 7 and NH wave 5 mode 7

# sigma2_M <- sigma2_M[-which(mgmail$wave==2&mgmail$st==44&mgmail$mode_fx==7)]
# sigma2_M <- sigma2_M[-which(mgmail$wave==5&mgmail$st==33&mgmail$mode_fx==7)]

# mgmail <- mgmail[!(mgmail$wave==2&mgmail$st==44&mgmail$mode_fx==7),]
# mgmail <- mgmail[!(mgmail$wave==5&mgmail$st==33&mgmail$mode_fx==7),]

# sigma2 <- rep(0, 5797)
# sigma2[1:5627] <- sigma2_T[1:5627]
# sigma2[5628:5797] <- (sigma2_T[5628:5797] + sigma2_M) / 4


# Y <- rep(0, 5797)
# Y[1:5627] <- log(mgphone[1:5627,]$est)
# Y[5628:5797] <- (log(mgphone[5628:5797,]$est) + log(mgmail$est)) / 2

# MailT <- c(rep(0, 5627), rep(1/2, 170))

# mgphone[5628:5797,]$wireless <- mgphone[5628:5797,]$wireless / 2
# mgphone[5628:5797,]$wirelesscount <- mgphone[5628:5797,]$wirelesscount / 2
######### up to 2015




####### up to 2016 wave 4
mg4 <- mg4[order(mg4$survey, mg4$year),]
mgphone <- mg4[-c(1:280),]
mgmail <- mg4[1:280,]

#phone data missing two points RI wave 2 mode 7 and NH wave 5 mode 7
#2016 phone data missing CT wave 2 mode 7 and RI wave 2 mode 7

sigma2_M <- sigma2_M[-which(mgmail$wave==2&mgmail$st==44&mgmail$mode_fx==7&mgmail$year==2015)]
sigma2_M <- sigma2_M[-which(mgmail$wave==5&mgmail$st==33&mgmail$mode_fx==7&mgmail$year==2015)]
sigma2_M <- sigma2_M[-which(mgmail$wave==2&mgmail$st==9&mgmail$mode_fx==7&mgmail$year==2016)]
sigma2_M <- sigma2_M[-which(mgmail$wave==2&mgmail$st==44&mgmail$mode_fx==7&mgmail$year==2016)]

mgmail <- mgmail[!(mgmail$wave==2&mgmail$st==44&mgmail$mode_fx==7&mgmail$year==2015),]
mgmail <- mgmail[!(mgmail$wave==5&mgmail$st==33&mgmail$mode_fx==7&mgmail$year==2015),]
mgmail <- mgmail[!(mgmail$wave==2&mgmail$st==9&mgmail$mode_fx==7&mgmail$year==2016),]
mgmail <- mgmail[!(mgmail$wave==2&mgmail$st==44&mgmail$mode_fx==7&mgmail$year==2016),]

sigma2 <- rep(0, 5903)
sigma2[1:5627] <- sigma2_T[1:5627]
sigma2[5628:5903] <- (sigma2_T[5628:5903] + sigma2_M) / 4


Y <- rep(0, 5903)
Y[1:5627] <- log(mgphone[1:5627,]$est)
Y[5628:5903] <- (log(mgphone[5628:5903,]$est) + log(mgmail$est)) / 2

MailT <- c(rep(0, 5627), rep(1/2, 276))

mgphone[5628:5903,]$wireless <- mgphone[5628:5903,]$wireless / 2
mgphone[5628:5903,]$wirelesscount <- mgphone[5628:5903,]$wirelesscount / 2

attach(mgphone)
Wave1<-1*(wave=="1")
Wave2<-1*(wave=="2")
Wave3<-1*(wave=="3")
Wave4<-1*(wave=="4")
Wave5<-1*(wave=="5")
Wave6<-1*(wave=="6")
## Check sums.
length(Wave1)
sum(Wave1+Wave2+Wave3+Wave4+Wave5+Wave6)
##
## 
FIPS<-cbind(c("AL","CT","DE","FL","GA","LA","ME","MD","MA","MS","NH","NJ","NY","NC","RI","SC","VA"),c(1,9,10,12,13,22,23,24,25,28,33,34,36,37,44,45,51))
AL<-1*(st=="1")
CT<-1*(st=="9")
DE<-1*(st=="10")
FL<-1*(st=="12")
GA<-1*(st=="13")
LA<-1*(st=="22")
ME<-1*(st=="23")
MD<-1*(st=="24")
MA<-1*(st=="25")
MS<-1*(st=="28")
NH<-1*(st=="33")
NJ<-1*(st=="34")
NY<-1*(st=="36")
NC<-1*(st=="37")
RI<-1*(st=="44")
SC<-1*(st=="45")
VA<-1*(st=="51")




dat <- cbind(mgphone,MailT,Wave1,Wave2,Wave3,Wave4,Wave5,Wave6,AL,CT,DE,FL,GA,LA,ME,MD,MA,MS,NH,NJ,NY,NC,RI,SC,VA,Y,sigma2)




FH_fit3 <- eblupFH(Y~MailT+log(pop)+(Wave2+Wave3+Wave4+Wave5+Wave6)
+(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+(Wave2+Wave3+Wave4+Wave5):(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+Wave6:(DE+FL+GA+LA+MD+MS+NC+NJ+SC+VA)
+wireless
+wireless:log(pop)
+wireless:(Wave2+Wave3+Wave4+Wave5+Wave6)
+wireless:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA),vardir=sigma2,data=dat[dat$mode_fx=='3',])

plot(FH_fit3$eblup, dat[dat$mode_fx=='3',]$Y, pch=16, col='grey')

FH_mse3 <- mseFH(Y~MailT+log(pop)+(Wave2+Wave3+Wave4+Wave5+Wave6)
+(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+(Wave2+Wave3+Wave4+Wave5):(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+Wave6:(DE+FL+GA+LA+MD+MS+NC+NJ+SC+VA)
+wireless
+wireless:log(pop)
+wireless:(Wave2+Wave3+Wave4+Wave5+Wave6)
+wireless:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA),vardir=sigma2,data=dat[dat$mode_fx=='3',])


FH_fit3$fit$estcoef$beta





FH_fit7 <- eblupFH(Y~MailT+log(pop)+(Wave2+Wave3+Wave4+Wave5+Wave6)
+(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+(Wave2+Wave3+Wave4+Wave5):(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+Wave6:(DE+FL+GA+LA+MD+MS+NC+NJ+SC+VA)
+wireless
+wireless:log(pop)
+wireless:(Wave2+Wave3+Wave4+Wave5+Wave6)
+wireless:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA),vardir=sigma2,data=dat[dat$mode_fx=='7',])

#plot(FH_fit7$eblup, dat[dat$mode_fx=='7',]$Y, pch=16, col='grey')

FH_mse7 <- mseFH(Y~MailT+log(pop)+(Wave2+Wave3+Wave4+Wave5+Wave6)
+(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+(Wave2+Wave3+Wave4+Wave5):(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+Wave6:(DE+FL+GA+LA+MD+MS+NC+NJ+SC+VA)
+wireless
+wireless:log(pop)
+wireless:(Wave2+Wave3+Wave4+Wave5+Wave6)
+wireless:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA),vardir=sigma2,data=dat[dat$mode_fx=='7',])


FH_fit7$fit$estcoef$beta


rm(list=setdiff(ls(),c('FH_fit3','FH_fit7','FH_mse3','FH_mse7','mgmail','dat','mgphone','sigma2','Y','sigma2_M','sigma2_T')))
save.image('FH.RData')