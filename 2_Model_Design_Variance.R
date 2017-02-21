# This piece of code reads the the combined dataset and gives us the design variance in our model.

# You'll get DesignVar.RData after running the code.


rm(list=ls())
library(nlme)
load("MergeData.Rdata")
ls()
names(mg4)
Time<-as.numeric(mg4$year)+(as.numeric(mg4$wave)-1)/6
Phone<-(mg4$survey=="phone")
Mail<-!Phone
##
##
## Construct indicators to ensure
## full rank fit and to make 
## interpreting the models easier.
##
attach(mg4)
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
##
## Estimate mean-variance relationships by regressing
## log of empirical coefficient of variation on log of sample size.
## This is equivalent to regressing log of variance estimate
## on log of sample size plus known offset of 2*log(estimate).
##
os<-2*log(mg4$est)
dd<-cbind(mg4,Time,Phone,Mail,Wave1,Wave2,Wave3,Wave4,Wave5,Wave6,AL,CT,DE,FL,GA,LA,ME,MD,MA,MS,NH,NJ,NY,NC,RI,SC,VA,os)
##########################################
##########################################
ddT<-subset(dd,Phone==TRUE)
VT_fit<-lm(log(var)~log(samplesize)+(Wave2+Wave3+Wave4+Wave5+Wave6)+(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+(Wave2+Wave3+Wave4+Wave5):(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+Wave6:(DE+FL+GA+LA+MD+MS+NC+NJ+SC+VA),offset=os,data=ddT)
#summary(VT_fit)
##########################################
##########################################
ddM<-subset(dd,Phone==FALSE)
VM_fit<-lm(log(var)~log(samplesize)+(Wave2+Wave3+Wave4+Wave5+Wave6)+(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+(Wave2+Wave3+Wave4+Wave5):(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+Wave6:(DE+FL+GA+LA+MD+MS+NC+NJ+SC+VA),offset=os,data=ddM)
#summary(VM_fit)

## 
## Use the fitted model to solve for design variances on the log scale.
## This involves solution of a quartic equation, which turns out to have one pair of complex conjugate roots,
## one negative real root, and positive real root. 
tau2T<-sum(VT_fit$resid^2/VT_fit$df.residual)
rhs<-exp(predict(VT_fit)-2*log(ddT$est)+tau2T/2) # need to remove the offset
roots<-matrix(0,length(rhs),4)
for(i in 1:length(rhs)){
	roots[i,]<-polyroot(c(-rhs[i],0,0,-1,1))
}
## Fourth root is always real, others are always complex or negative.
apply(Im(roots),MAR=2,FUN="range")
apply(Re(roots),MAR=2,FUN="range")
##
## Estimate of \sigma^2_T is log of the fourth (real, positive) root.
##
sigma2_T<-log(Re(roots[,4]))

## MAIL: 
## Use the fitted model to solve for design variances on the log scale.
## This involves solution of a quartic equation, which turns out to have one pair of complex conjugate roots,
## one negative real root, and positive real root. 
tau2M<-sum(VM_fit$resid^2/VM_fit$df.residual)
rhs<-exp(predict(VM_fit)-2*log(ddM$est)+tau2M/2) # need to remove the offset
roots<-matrix(0,length(rhs),4)
for(i in 1:length(rhs)){
	roots[i,]<-polyroot(c(-rhs[i],0,0,-1,1))
}
## Fourth root is always real, others are always complex or negative.
apply(Im(roots),MAR=2,FUN="range")
apply(Re(roots),MAR=2,FUN="range")
##
## Estimate of \sigma^2_M is log of the fourth (real, positive) root.
##
sigma2_M<-log(Re(roots[,4]))


rm(list=setdiff(ls(),c('sigma2_M','sigma2_T')))
save.image('DesignVar.RData')