# This last piece of code reads the Fay-Herriot fit and gives you our prediction of Mail effort.

# You'll get Mode_3_logeffort.pdf, Mode_3_logeffort2000.pdf, Mode_3_effort.pdf, Mode_3_effort2000.pdf, Mode_7_logeffort.pdf, Mode_7_logeffort2000.pdf, Mode_7_effort.pdf, Mode_7_effort2000.pdf. These 8 pdf files show you the prediction plots of mail effort for mode 3 and mode 7 in log scale and in original scale, since 1982 and since 2000 respectively.

rm(list=ls())
load('FH.RData')
ls()


# borrow the design matrix from lm
lm_fit <- lm(Y~MailT+log(pop)+(Wave2+Wave3+Wave4+Wave5+Wave6)
+(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+(Wave2+Wave3+Wave4+Wave5):(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+Wave6:(DE+FL+GA+LA+MD+MS+NC+NJ+SC+VA)
+wireless
+wireless:log(pop)
+wireless:(Wave2+Wave3+Wave4+Wave5+Wave6)
+wireless:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA),
data=dat[dat$mode_fx=='3',])

dsgmat <- model.matrix(lm_fit)
# nu_st by subtraction
uvec_s <- FH_fit3$eblup - dsgmat %*% FH_fit3$fit$estcoef$beta


ddw<-subset(dat,mode_fx==3)
ddw$Time <- as.numeric(ddw$year)+(as.numeric(ddw$wave)-1)/6
dd0<-ddw
dd0$wireless<-0*dd0$wireless

StateNo<-sort(as.numeric(unique(ddw$st)))
StateName<-c("AL","CT","DE","FL","GA","LA","ME","MD","MA","MS","NH","NJ","NY","NC","RI","SC","VA")


lm_fit2 <- lm(Y~0+log(pop)+(Wave2+Wave3+Wave4+Wave5+Wave6)
+(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+(Wave2+Wave3+Wave4+Wave5):(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+Wave6:(DE+FL+GA+LA+MD+MS+NC+NJ+SC+VA)
+wireless
+wireless:log(pop)
+wireless:(Wave2+Wave3+Wave4+Wave5+Wave6)
+wireless:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA),
data=dd0)

dsgmat2 <- model.matrix(lm_fit2)

# # eblup of M with wireless on
# M_a <- cbind(1, 1, dsgmat[,-c(1,2)]) %*% FH_fit3$fit$estcoef$beta + uvec_s

# eblup of M with wireless off
M_b <- cbind(1, 1, dsgmat2) %*% FH_fit3$fit$estcoef$beta + uvec_s	

mgmail$Time <- as.numeric(mgmail$year)+(as.numeric(mgmail$wave)-1)/6
	


# calculate mse of M

psi <- FH_fit3$fit$refvar
D <- sigma2[dat$mode_fx=='3']
X <- dsgmat

# mse of M

Xp <- dsgmat %*% diag(c(rep(1,24), 0, rep(1, 74), rep(0, 22)))

mat <- X %*% solve(t(X) %*% diag(1 / (psi + D)) %*% X) %*% t(X)
matp <- Xp %*% solve(t(X) %*% diag(1 / (psi + D)) %*% X) %*% t(Xp)
matm <- X %*% solve(t(X) %*% diag(1 / (psi + D)) %*% X) %*% t(Xp)

g1 <- psi * D / (psi + D)
g2 <- split(matp, row(matp)-col(matp))$`0` - 2 * psi / (psi + D) * split(matm, row(matm)-col(matm))$`0` + psi^2 / (psi + D)^2 * split(mat, row(mat)-col(mat))$`0`
g3 <- 2 * D^2 / (psi + D)^3 / sum(1 / (psi + D)^2)

mse_M <- g1 + g2 + 2 * g3





# 2nd predictor

lm_fit3 <- lm(Y~0
+wireless
+wireless:log(pop)
+wireless:(Wave2+Wave3+Wave4+Wave5+Wave6)
+wireless:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA),
data=ddw)

dsgmat3 <- model.matrix(lm_fit3)

lm_fit4 <- lm(Y~0
+wireless
+wireless:log(pop)
+wireless:(Wave2+Wave3+Wave4+Wave5+Wave6)
+wireless:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA),
data=dd0)

dsgmat4 <- model.matrix(lm_fit4)

# Second predictor 
# \hat T_st + \hat\mu_st - w_st*b_st'*gamma
M2_a <- log(ddw$est) - dsgmat3 %*% FH_fit3$fit$estcoef$beta[c(25, 100:121)] + FH_fit3$fit$estcoef$beta[2]



# approximated mean square error of w_st*b_st'*gamma

# cov of beta
cov_beta = solve(t(X)%*%diag(1/(psi+D))%*%X)

cov_wireless = dsgmat[,c(25, 100:121)] %*% cov_beta[c(25, 100:121),c(25, 100:121)] %*% t(dsgmat[,c(25, 100:121)]) 
var_wireless = split(cov_wireless, row(cov_wireless)-col(cov_wireless))$`0`





pdf('Mode_3_logeffort.pdf')

for(i in 1:length(StateNo)){

	plot(c(ddw$Time,mgmail$Time),c(log(ddw$est), log(mgmail$est)),xlab="Time",ylab="log(Effort)",type='n')
	a<-subset(ddw,st==paste(StateNo[i]))
	b<-subset(dd0,st==paste(StateNo[i]))
	lines(a$Time,log(a$est)+ FH_fit3$fit$estcoef$beta[2],col="gray",lwd=3)
	points(a$Time,log(a$est),col="gray",pch=16,cex=0.7)
	lines(a$Time,M2_a[ddw$st==paste(StateNo[i])],col="black", lwd=3)
	lines(a$Time,M_b[ddw$st==paste(StateNo[i])],col="yellow")
	points(mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3'],log(mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3']),col='blue',pch=16,cex=1)
	title(main=paste("Mode 3 for", StateName[i],": FIPS =",StateNo[i]))
	legend(x='bottomright', legend=c(expression(widehat(M)), expression(widehat(T)), expression(paste(widehat(T), ' + ', hat(mu))), expression(paste(widehat(T),' + ', hat(mu), ' - wireless')), 'EBLUP(M)'), col=c('blue','gray','gray','black', 'yellow'), lty=c(0,0,1,1,1), pch=c(16,16,NA,NA,NA), lwd=c(NA,NA,3,3,1))

}
dev.off()


pdf('Mode_3_logeffort2000.pdf')

for(i in 1:length(StateNo)){

	plot(c(ddw$Time,mgmail$Time),c(log(ddw$est), log(mgmail$est)),xlab="Time",ylab="log(Effort)",type='n',xlim=c(2000,2016))
	a<-subset(ddw,st==paste(StateNo[i]))
	b<-subset(dd0,st==paste(StateNo[i]))
	lines(a$Time,log(a$est)+ FH_fit3$fit$estcoef$beta[2],col="gray",lwd=3)
	points(a$Time,log(a$est),col="gray",pch=16,cex=0.7)
	lines(a$Time,M2_a[ddw$st==paste(StateNo[i])],col="black", lwd=3)
	lines(a$Time,M_b[ddw$st==paste(StateNo[i])],col="yellow")
	points(mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3'],log(mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3']),col='blue',pch=16,cex=1)
	title(main=paste("Mode 3 for", StateName[i],": FIPS =",StateNo[i]))
	legend(x='bottomright', legend=c(expression(widehat(M)), expression(widehat(T)), expression(paste(widehat(T), ' + ', hat(mu))), expression(paste(widehat(T),' + ', hat(mu), ' - wireless')), 'EBLUP(M)'), col=c('blue','gray','gray','black', 'yellow'), lty=c(0,0,1,1,1), pch=c(16,16,NA,NA,NA), lwd=c(NA,NA,3,3,1))

}
dev.off()




pdf('Mode_3_effort.pdf')

for(i in 1:length(StateNo)){

	a<-subset(ddw,st==paste(StateNo[i]))
	b<-subset(dd0,st==paste(StateNo[i]))
	plot(c(a$Time,a$Time,a$Time,mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3']), c((a$est), exp(M2_a[ddw$st==paste(StateNo[i])] + (sigma2_T[dat$st==paste(StateNo[i])&dat$mode_fx==3]+ var_wireless[ddw$st==paste(StateNo[i])] + cov_beta[2,2])/2), exp(log(a$est)+ FH_fit3$fit$estcoef$beta[2] + (sigma2_T[dat$st==paste(StateNo[i])&dat$mode_fx==3]+ cov_beta[2,2])/2), (mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3'])),xlab="Time",ylab="Effort",type='n')
	lines(a$Time, exp(log(a$est)+ FH_fit3$fit$estcoef$beta[2] + (sigma2_T[dat$st==paste(StateNo[i])&dat$mode_fx==3] + cov_beta[2,2])/2),col="gray",lwd=3)
	points(a$Time,(a$est),col="gray",pch=16,cex=0.7)
	lines(a$Time, exp(M2_a[ddw$st==paste(StateNo[i])] + (sigma2_T[dat$st==paste(StateNo[i])&dat$mode_fx==3] + var_wireless[ddw$st==paste(StateNo[i])] + cov_beta[2,2])/2),col="black", lwd=3)
	lines(a$Time, exp(M_b[ddw$st==paste(StateNo[i])] + mse_M[ddw$st==paste(StateNo[i])]/2),col="yellow")
	points(mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3'],(mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3']),col='blue',pch=16,cex=1)
	title(main=paste("Mode 3 for", StateName[i],": FIPS =",StateNo[i]))
	legend(x='topleft', legend=c(expression(widehat(M)), expression(widehat(T)), expression(paste(widehat(T), ' + ', hat(mu))), expression(paste(widehat(T),' + ', hat(mu), ' - wireless')), 'EBLUP(M)'), col=c('blue','gray','gray','black', 'yellow'), lty=c(0,0,1,1,1), pch=c(16,16,NA,NA,NA), lwd=c(NA,NA,3,3,1))

}
dev.off()

pdf('Mode_3_effort2000.pdf')

for(i in 1:length(StateNo)){

	a<-subset(ddw,st==paste(StateNo[i]))
	b<-subset(dd0,st==paste(StateNo[i]))
	plot(c(a$Time,a$Time,a$Time,mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3']), c((a$est), exp(M2_a[ddw$st==paste(StateNo[i])] + (sigma2_T[dat$st==paste(StateNo[i])&dat$mode_fx==3]+ var_wireless[ddw$st==paste(StateNo[i])] + cov_beta[2,2])/2), exp(log(a$est)+ FH_fit3$fit$estcoef$beta[2] + (sigma2_T[dat$st==paste(StateNo[i])&dat$mode_fx==3]+ cov_beta[2,2])/2), (mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3'])),xlab="Time",ylab="Effort",type='n',xlim=c(2000,2016))
	lines(a$Time, exp(log(a$est)+ FH_fit3$fit$estcoef$beta[2] + (sigma2_T[dat$st==paste(StateNo[i])&dat$mode_fx==3] + cov_beta[2,2])/2),col="gray",lwd=3)
	points(a$Time,(a$est),col="gray",pch=16,cex=0.7)
	lines(a$Time, exp(M2_a[ddw$st==paste(StateNo[i])] + (sigma2_T[dat$st==paste(StateNo[i])&dat$mode_fx==3] + var_wireless[ddw$st==paste(StateNo[i])] + cov_beta[2,2])/2),col="black", lwd=3)
	lines(a$Time, exp(M_b[ddw$st==paste(StateNo[i])] + mse_M[ddw$st==paste(StateNo[i])]/2),col="yellow")
	points(mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3'],(mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3']),col='blue',pch=16,cex=1)
	title(main=paste("Mode 3 for", StateName[i],": FIPS =",StateNo[i]))
	legend(x='topleft', legend=c(expression(widehat(M)), expression(widehat(T)), expression(paste(widehat(T), ' + ', hat(mu))), expression(paste(widehat(T),' + ', hat(mu), ' - wireless')), 'EBLUP(M)'), col=c('blue','gray','gray','black', 'yellow'), lty=c(0,0,1,1,1), pch=c(16,16,NA,NA,NA), lwd=c(NA,NA,3,3,1))

}
dev.off()



######## mode 7


# borrow the design matrix from lm
lm_fit <- lm(Y~MailT+log(pop)+(Wave2+Wave3+Wave4+Wave5+Wave6)
+(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+(Wave2+Wave3+Wave4+Wave5):(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+Wave6:(DE+FL+GA+LA+MD+MS+NC+NJ+SC+VA)
+wireless
+wireless:log(pop)
+wireless:(Wave2+Wave3+Wave4+Wave5+Wave6)
+wireless:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA),
data=dat[dat$mode_fx=='7',])

dsgmat <- model.matrix(lm_fit)

# nu_st by subtraction
uvec_s <- FH_fit7$eblup - dsgmat %*% FH_fit7$fit$estcoef$beta


ddw<-subset(dat,mode_fx==7)
ddw$Time <- as.numeric(ddw$year)+(as.numeric(ddw$wave)-1)/6
dd0<-ddw
dd0$wireless<-0*dd0$wireless

StateNo<-sort(as.numeric(unique(ddw$st)))
StateName<-c("AL","CT","DE","FL","GA","LA","ME","MD","MA","MS","NH","NJ","NY","NC","RI","SC","VA")


lm_fit2 <- lm(Y~0+log(pop)+(Wave2+Wave3+Wave4+Wave5+Wave6)
+(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+(Wave2+Wave3+Wave4+Wave5):(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+Wave6:(DE+FL+GA+LA+MD+MS+NC+NJ+SC+VA)
+wireless
+wireless:log(pop)
+wireless:(Wave2+Wave3+Wave4+Wave5+Wave6)
+wireless:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA),
data=dd0)

dsgmat2 <- model.matrix(lm_fit2)

# # eblup of M with wireless on
# M_a <- cbind(1, 1, dsgmat[,-c(1,2)]) %*% FH_fit7$fit$estcoef$beta + uvec_s

# eblup of M with wireless off
M_b <- cbind(1, 1, dsgmat2) %*% FH_fit7$fit$estcoef$beta + uvec_s	


mgmail$Time <- as.numeric(mgmail$year)+(as.numeric(mgmail$wave)-1)/6

# calculate mse of M

psi <- FH_fit7$fit$refvar
D <- sigma2[dat$mode_fx=='7']
X <- dsgmat

# mse of M

Xp <- dsgmat %*% diag(c(rep(1,24), 0, rep(1, 74), rep(0, 22)))

mat <- X %*% solve(t(X) %*% diag(1 / (psi + D)) %*% X) %*% t(X)
matp <- Xp %*% solve(t(X) %*% diag(1 / (psi + D)) %*% X) %*% t(Xp)
matm <- X %*% solve(t(X) %*% diag(1 / (psi + D)) %*% X) %*% t(Xp)

g1 <- psi * D / (psi + D)
g2 <- split(matp, row(matp)-col(matp))$`0` - 2 * psi / (psi + D) * split(matm, row(matm)-col(matm))$`0` + psi^2 / (psi + D)^2 * split(mat, row(mat)-col(mat))$`0`
g3 <- 2 * D^2 / (psi + D)^3 / sum(1 / (psi + D)^2)

mse_M <- g1 + g2 + 2 * g3




# 2nd predictor

lm_fit3 <- lm(Y~0
+wireless
+wireless:log(pop)
+wireless:(Wave2+Wave3+Wave4+Wave5+Wave6)
+wireless:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA),
data=ddw)

dsgmat3 <- model.matrix(lm_fit3)

lm_fit4 <- lm(Y~0
+wireless
+wireless:log(pop)
+wireless:(Wave2+Wave3+Wave4+Wave5+Wave6)
+wireless:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA),
data=dd0)

dsgmat4 <- model.matrix(lm_fit4)

# Second predictor 
# \hat T_st + \hat\mu_st - w_st*b_st'*gamma
M2_a <- log(ddw$est) - dsgmat3 %*% FH_fit7$fit$estcoef$beta[c(25, 100:121)] + FH_fit7$fit$estcoef$beta[2]


# approximated mean square error of w_st*b_st'*gamma

# cov of beta
cov_beta = solve(t(X)%*%diag(1/(psi+D))%*%X)

cov_wireless = dsgmat[,c(25, 100:121)] %*% cov_beta[c(25, 100:121),c(25, 100:121)] %*% t(dsgmat[,c(25, 100:121)]) 
var_wireless = split(cov_wireless, row(cov_wireless)-col(cov_wireless))$`0`





pdf('Mode_7_logeffort.pdf')

for(i in 1:length(StateNo)){

	plot(c(ddw$Time,mgmail$Time),c(log(ddw$est), log(mgmail$est)),xlab="Time",ylab="log(Effort)",type='n')
	a<-subset(ddw,st==paste(StateNo[i]))
	b<-subset(dd0,st==paste(StateNo[i]))
	lines(a$Time,log(a$est)+ FH_fit7$fit$estcoef$beta[2],col="gray",lwd=3)
	points(a$Time,log(a$est),col="gray",pch=16,cex=0.7)
	lines(a$Time,M2_a[ddw$st==paste(StateNo[i])],col="black", lwd=3)
	lines(a$Time,M_b[ddw$st==paste(StateNo[i])],col="yellow")
	points(mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7'],log(mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7']),col='blue',pch=16,cex=1)
	title(main=paste("Mode 7 for", StateName[i],": FIPS =",StateNo[i]))
	legend(x='bottomright', legend=c(expression(widehat(M)), expression(widehat(T)), expression(paste(widehat(T), ' + ', hat(mu))), expression(paste(widehat(T),' + ', hat(mu), ' - wireless')), 'EBLUP(M)'), col=c('blue','gray','gray','black', 'yellow'), lty=c(0,0,1,1,1), pch=c(16,16,NA,NA,NA), lwd=c(NA,NA,3,3,1))

}
dev.off()




pdf('Mode_7_logeffort2000.pdf')

for(i in 1:length(StateNo)){

	plot(c(ddw$Time,mgmail$Time),c(log(ddw$est), log(mgmail$est)),xlab="Time",ylab="log(Effort)",type='n',xlim=c(2000,2016))
	a<-subset(ddw,st==paste(StateNo[i]))
	b<-subset(dd0,st==paste(StateNo[i]))
	lines(a$Time,log(a$est)+ FH_fit7$fit$estcoef$beta[2],col="gray",lwd=3)
	points(a$Time,log(a$est),col="gray",pch=16,cex=0.7)
	lines(a$Time,M2_a[ddw$st==paste(StateNo[i])],col="black", lwd=3)
	lines(a$Time,M_b[ddw$st==paste(StateNo[i])],col="yellow")
	points(mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7'],log(mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7']),col='blue',pch=16,cex=1)
	title(main=paste("Mode 7 for", StateName[i],": FIPS =",StateNo[i]))
	legend(x='bottomright', legend=c(expression(widehat(M)), expression(widehat(T)), expression(paste(widehat(T), ' + ', hat(mu))), expression(paste(widehat(T),' + ', hat(mu), ' - wireless')), 'EBLUP(M)'), col=c('blue','gray','gray','black', 'yellow'), lty=c(0,0,1,1,1), pch=c(16,16,NA,NA,NA), lwd=c(NA,NA,3,3,1))

}
dev.off()







pdf('Mode_7_effort.pdf')

for(i in 1:length(StateNo)){

	a<-subset(ddw,st==paste(StateNo[i]))
	b<-subset(dd0,st==paste(StateNo[i]))
	plot(c(a$Time,a$Time,a$Time,mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7']),c((a$est), exp(M2_a[ddw$st==paste(StateNo[i])] + (sigma2_T[dat$st==paste(StateNo[i])&dat$mode_fx==7] + var_wireless[ddw$st==paste(StateNo[i])] + cov_beta[2,2])/2), exp(log(a$est)+ FH_fit7$fit$estcoef$beta[2] + (sigma2_T[dat$st==paste(StateNo[i])&dat$mode_fx==7] + cov_beta[2,2])/2), (mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7'])),xlab="Time",ylab="Effort",type='n')
	lines(a$Time, exp(log(a$est)+ FH_fit7$fit$estcoef$beta[2] + (sigma2_T[dat$st==paste(StateNo[i])&dat$mode_fx==7] + cov_beta[2,2])/2),col="gray",lwd=3)
	points(a$Time,(a$est),col="gray",pch=16,cex=0.7)
	lines(a$Time, exp(M2_a[ddw$st==paste(StateNo[i])] + (sigma2_T[dat$st==paste(StateNo[i])&dat$mode_fx==7] + var_wireless[ddw$st==paste(StateNo[i])] + cov_beta[2,2])/2),col="black", lwd=3)
	lines(a$Time, exp(M_b[ddw$st==paste(StateNo[i])] + mse_M[ddw$st==paste(StateNo[i])]/2),col="yellow")
	points(mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7'],(mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7']),col='blue',pch=16,cex=1)
	title(main=paste("Mode 7 for", StateName[i],": FIPS =",StateNo[i]))
	legend(x='topleft', legend=c(expression(widehat(M)), expression(widehat(T)), expression(paste(widehat(T), ' + ', hat(mu))), expression(paste(widehat(T),' + ', hat(mu), ' - wireless')), 'EBLUP(M)'), col=c('blue','gray','gray','black', 'yellow'), lty=c(0,0,1,1,1), pch=c(16,16,NA,NA,NA), lwd=c(NA,NA,3,3,1))


}
dev.off()


pdf('Mode_7_effort2000.pdf')

for(i in 1:length(StateNo)){

	a<-subset(ddw,st==paste(StateNo[i]))
	b<-subset(dd0,st==paste(StateNo[i]))
	plot(c(a$Time,a$Time,a$Time,mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7']),c((a$est), exp(M2_a[ddw$st==paste(StateNo[i])] + (sigma2_T[dat$st==paste(StateNo[i])&dat$mode_fx==7] + var_wireless[ddw$st==paste(StateNo[i])] + cov_beta[2,2])/2), exp(log(a$est)+ FH_fit7$fit$estcoef$beta[2] + (sigma2_T[dat$st==paste(StateNo[i])&dat$mode_fx==7] + cov_beta[2,2])/2), (mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7'])),xlab="Time",ylab="Effort",type='n',xlim=c(2000,2016))
	lines(a$Time, exp(log(a$est)+ FH_fit7$fit$estcoef$beta[2] + (sigma2_T[dat$st==paste(StateNo[i])&dat$mode_fx==7] + cov_beta[2,2])/2),col="gray",lwd=3)
	points(a$Time,(a$est),col="gray",pch=16,cex=0.7)
	lines(a$Time, exp(M2_a[ddw$st==paste(StateNo[i])] + (sigma2_T[dat$st==paste(StateNo[i])&dat$mode_fx==7] + var_wireless[ddw$st==paste(StateNo[i])] + cov_beta[2,2])/2),col="black", lwd=3)
	lines(a$Time, exp(M_b[ddw$st==paste(StateNo[i])] + mse_M[ddw$st==paste(StateNo[i])]/2),col="yellow")
	points(mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7'],(mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7']),col='blue',pch=16,cex=1)
	title(main=paste("Mode 7 for", StateName[i],": FIPS =",StateNo[i]))
	legend(x='topleft', legend=c(expression(widehat(M)), expression(widehat(T)), expression(paste(widehat(T), ' + ', hat(mu))), expression(paste(widehat(T),' + ', hat(mu), ' - wireless')), 'EBLUP(M)'), col=c('blue','gray','gray','black', 'yellow'), lty=c(0,0,1,1,1), pch=c(16,16,NA,NA,NA), lwd=c(NA,NA,3,3,1))


}
dev.off()
