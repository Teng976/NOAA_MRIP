# This file reads the combined dataset and the design variance estimates and gives the Fay-Herriot fit for mode 3 and mode 7 separately. 

# You'll get FH.RData after running this.



rm(list=ls())

library(sae)
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




# ####### up to 2016 wave 4
# mg4 <- mg4[order(mg4$survey, mg4$year),]
# mgphone <- mg4[-c(1:280),]
# mgmail <- mg4[1:280,]

# #phone data missing two points RI wave 2 mode 7 and NH wave 5 mode 7
# #2016 phone data missing CT wave 2 mode 7 and RI wave 2 mode 7

# sigma2_M <- sigma2_M[-which(mgmail$wave==2&mgmail$st==44&mgmail$mode_fx==7&mgmail$year==2015)]
# sigma2_M <- sigma2_M[-which(mgmail$wave==5&mgmail$st==33&mgmail$mode_fx==7&mgmail$year==2015)]
# sigma2_M <- sigma2_M[-which(mgmail$wave==2&mgmail$st==9&mgmail$mode_fx==7&mgmail$year==2016)]
# sigma2_M <- sigma2_M[-which(mgmail$wave==2&mgmail$st==44&mgmail$mode_fx==7&mgmail$year==2016)]

# mgmail <- mgmail[!(mgmail$wave==2&mgmail$st==44&mgmail$mode_fx==7&mgmail$year==2015),]
# mgmail <- mgmail[!(mgmail$wave==5&mgmail$st==33&mgmail$mode_fx==7&mgmail$year==2015),]
# mgmail <- mgmail[!(mgmail$wave==2&mgmail$st==9&mgmail$mode_fx==7&mgmail$year==2016),]
# mgmail <- mgmail[!(mgmail$wave==2&mgmail$st==44&mgmail$mode_fx==7&mgmail$year==2016),]

# sigma2 <- rep(0, 5903)
# sigma2[1:5627] <- sigma2_T[1:5627]
# sigma2[5628:5903] <- (sigma2_T[5628:5903] + sigma2_M) / 4


# Y <- rep(0, 5903)
# Y[1:5627] <- log(mgphone[1:5627,]$est)
# Y[5628:5903] <- (log(mgphone[5628:5903,]$est) + log(mgmail$est)) / 2

# MailT <- c(rep(0, 5627), rep(1/2, 276))

# mgphone[5628:5903,]$wireless <- mgphone[5628:5903,]$wireless / 2
# mgphone[5628:5903,]$wirelesscount <- mgphone[5628:5903,]$wirelesscount / 2





####### up to 2016 wave 5

mg4 <- mg4[order(mg4$survey, mg4$year),]
mgphone <- mg4[-c(1:314),]
mgmail <- mg4[1:314,]


####### delete wave 6 phone data and also sigma2_T
sigma2_T <- sigma2_T[-which(mgphone$year==2016&mgphone$wave==6)]
mgphone <- mgphone[!(mgphone$year==2016&mgphone$wave==6),]
#######



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

sigma2 <- rep(0, dim(mgphone)[1])
sigma2[1:5627] <- sigma2_T[1:5627]
sigma2[5628:dim(mgphone)[1]] <- (sigma2_T[5628:dim(mgphone)[1]] + sigma2_M) / 4


Y <- rep(0, dim(mgphone)[1])
Y[1:5627] <- log(mgphone[1:5627,]$est)
Y[5628:dim(mgphone)[1]] <- (log(mgphone[5628:dim(mgphone)[1],]$est) + log(mgmail$est)) / 2

MailT <- c(rep(0, 5627), rep(1/2, dim(mgmail)[1]))

mgphone[5628:dim(mgphone)[1],]$wireless <- mgphone[5628:dim(mgphone)[1],]$wireless / 2
mgphone[5628:dim(mgphone)[1],]$wirelesscount <- mgphone[5628:dim(mgphone)[1],]$wirelesscount / 2





attach(mgphone)
Wave1<-1*(dat$wave=="1")
Wave2<-1*(dat$wave=="2")
Wave3<-1*(dat$wave=="3")
Wave4<-1*(dat$wave=="4")
Wave5<-1*(dat$wave=="5")
Wave6<-1*(dat$wave=="6")
## Check sums.
length(Wave1)
sum(Wave1+Wave2+Wave3+Wave4+Wave5+Wave6)
##
## 
FIPS<-cbind(c("AL","CT","DE","FL","GA","LA","ME","MD","MA","MS","NH","NJ","NY","NC","RI","SC","VA"),c(1,9,10,12,13,22,23,24,25,28,33,34,36,37,44,45,51))
AL<-1*(dat$st=="1")
CT<-1*(dat$st=="9")
DE<-1*(dat$st=="10")
FL<-1*(dat$st=="12")
GA<-1*(dat$st=="13")
LA<-1*(dat$st=="22")
ME<-1*(dat$st=="23")
MD<-1*(dat$st=="24")
MA<-1*(dat$st=="25")
MS<-1*(dat$st=="28")
NH<-1*(dat$st=="33")
NJ<-1*(dat$st=="34")
NY<-1*(dat$st=="36")
NC<-1*(dat$st=="37")
RI<-1*(dat$st=="44")
SC<-1*(dat$st=="45")
VA<-1*(dat$st=="51")




dat <- cbind(mgphone,MailT,Wave1,Wave2,Wave3,Wave4,Wave5,Wave6,AL,CT,DE,FL,GA,LA,ME,MD,MA,MS,NH,NJ,NY,NC,RI,SC,VA,Y,sigma2)


stwave <- paste(dat[dat$mode_fx=='3',]$st,dat[dat$mode_fx=='3',]$wave, sep='_')






FH_fit3 <- eblupFH(Y~0+factor(stwave)+log(pop):(AL+CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless
+wireless:log(pop)
+wireless:(Wave2+Wave3+Wave4+Wave5+Wave6)
+wireless:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless:log(pop):(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+MailT
+MailT:(Wave2+Wave3+Wave4+Wave5+Wave6)
+MailT:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
,vardir=sigma2,data=dat[dat$mode_fx=='3',])

#plot(FH_fit3$eblup, dat[dat$mode_fx=='3',]$Y, pch=16, col='grey')

FH_mse3 <- mseFH(Y~0+factor(stwave)+log(pop):(AL+CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless
+wireless:log(pop)
+wireless:(Wave2+Wave3+Wave4+Wave5+Wave6)
+wireless:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless:log(pop):(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+MailT
+MailT:(Wave2+Wave3+Wave4+Wave5+Wave6)
+MailT:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
,vardir=sigma2,data=dat[dat$mode_fx=='3',])


#FH_fit3$fit$estcoef$beta

FH_fit3_2 <- eblupFH(Y~0+factor(stwave)+log(pop):(AL+CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless
+wireless:log(pop)
+wireless:(Wave2+Wave3+Wave4+Wave5+Wave6)
+wireless:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless:log(pop):(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+MailT
,vardir=sigma2,data=dat[dat$mode_fx=='3',])





stwave <- paste(dat[dat$mode_fx=='7',]$st,dat[dat$mode_fx=='7',]$wave, sep='_')



FH_fit7 <- eblupFH(Y~0+factor(stwave)+log(pop):(AL+CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless
+wireless:log(pop)
+wireless:(Wave2+Wave3+Wave4+Wave5+Wave6)
+wireless:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless:log(pop):(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+MailT
+MailT:(Wave2+Wave3+Wave4+Wave5+Wave6)
+MailT:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
,vardir=sigma2,data=dat[dat$mode_fx=='7',])

#plot(FH_fit7$eblup, dat[dat$mode_fx=='7',]$Y, pch=16, col='grey')

FH_mse7 <- mseFH(Y~0+factor(stwave)+log(pop):(AL+CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless
+wireless:log(pop)
+wireless:(Wave2+Wave3+Wave4+Wave5+Wave6)
+wireless:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless:log(pop):(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+MailT
+MailT:(Wave2+Wave3+Wave4+Wave5+Wave6)
+MailT:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
,vardir=sigma2,data=dat[dat$mode_fx=='7',])

FH_fit7_2 <- eblupFH(Y~0+factor(stwave)+log(pop):(AL+CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless
+wireless:log(pop)
+wireless:(Wave2+Wave3+Wave4+Wave5+Wave6)
+wireless:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless:log(pop):(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+MailT
,vardir=sigma2,data=dat[dat$mode_fx=='7',])



#FH_fit7$fit$estcoef$beta

save(list=c('FH_fit3','FH_fit7','FH_fit3_2','FH_fit7_2','FH_mse3','FH_mse7','mgmail','dat','mgphone','sigma2','Y','sigma2_M','sigma2_T'), file='FH.RData')

rm(list=setdiff(ls(),c('FH_fit3','FH_fit7','FH_mse3','FH_mse7','mgmail','dat','mgphone','sigma2','Y','sigma2_M','sigma2_T')))
save.image('FH.RData')