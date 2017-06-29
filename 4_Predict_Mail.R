# This last piece of code reads the Fay-Herriot fit and gives you our prediction of Mail effort.

# You'll get Mode_3_logeffort.pdf, Mode_3_logeffort2000.pdf, Mode_3_effort.pdf, Mode_3_effort2000.pdf, Mode_7_logeffort.pdf, Mode_7_logeffort2000.pdf, Mode_7_effort.pdf, Mode_7_effort2000.pdf. These 8 pdf files show you the prediction plots of mail effort for mode 3 and mode 7 in log scale and in original scale, since 1982 and since 2000 respectively.

rm(list=ls())
load('FH.RData')
ls()



stwave <- paste(dat[dat$mode_fx=='3',]$st,dat[dat$mode_fx=='3',]$wave, sep='_')

# borrow the design matrix from lm
lm_fit <- lm(Y~0+factor(stwave)+log(pop):(AL+CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless
+wireless:log(pop)
+wireless:(Wave2+Wave3+Wave4+Wave5+Wave6)
+wireless:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless:log(pop):(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+MailT
+MailT:(Wave2+Wave3+Wave4+Wave5+Wave6)
+MailT:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA),
data=dat[dat$mode_fx=='3',])

dsgmat <- model.matrix(lm_fit)
# nu_st by subtraction
uvec_s <- FH_fit3$eblup - dsgmat %*% FH_fit3$fit$estcoef$beta


ddw<-subset(dat,mode_fx==3)
ddw$Time <- as.numeric(ddw$year)+(as.numeric(ddw$wave)-1)/6
dd0<-ddw
dd0$wireless<-0*dd0$wireless
dd02 <- dd0
dd02$MailT <- 1

StateNo<-sort(as.numeric(unique(ddw$st)))
StateName<-c("AL","CT","DE","FL","GA","LA","ME","MD","MA","MS","NH","NJ","NY","NC","RI","SC","VA")


lm_fit2 <- lm(Y~0+factor(stwave)+log(pop):(AL+CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless
+wireless:log(pop)
+wireless:(Wave2+Wave3+Wave4+Wave5+Wave6)
+wireless:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless:log(pop):(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+MailT
+MailT:(Wave2+Wave3+Wave4+Wave5+Wave6)
+MailT:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA),
data=dd02)

dsgmat2 <- model.matrix(lm_fit2)



# eblup of M with wireless off
M_b <- dsgmat2 %*% FH_fit3$fit$estcoef$beta + uvec_s	


mgmail$Time <- as.numeric(mgmail$year)+(as.numeric(mgmail$wave)-1)/6
	


# calculate mse of M

psi <- FH_fit3$fit$refvar
D <- sigma2[dat$mode_fx=='3']
X <- dsgmat

# mse of M

g1 <- psi * D / (psi + D)
mat <- ((psi * (dsgmat2 - dsgmat) + D * dsgmat2) / (psi + D))%*%solve(t(dsgmat)%*%diag(1/(psi + D))%*%dsgmat)%*%t((psi * (dsgmat2 - dsgmat) + D * dsgmat2) / (psi + D))
g2 <- mat[row(mat)==col(mat)]
g3 <- 2 * D^2 / (psi + D)^3 / sum(1 / (psi + D)^2)

mse_M <- g1 + g2 + 2 * g3





# 2nd predictor


ddw2 <- ddw
ddw2$MailT <- 1

lm_fit3 <- lm(Y~0+factor(stwave)+log(pop):(AL+CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless
+wireless:log(pop)
+wireless:(Wave2+Wave3+Wave4+Wave5+Wave6)
+wireless:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless:log(pop):(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+MailT
+MailT:(Wave2+Wave3+Wave4+Wave5+Wave6)
+MailT:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA),
data=ddw2)

dsgmat3 <- model.matrix(lm_fit3)

# Second predictor 
# \hat T_st + b_st'*\hat\mu - w_st*c_st'*\hat\gamma
# wireless index c(97,116:137,159:174)
# mail index 
wireind <- c(97,116:137,159:174)
mailind <- c(104,144:164) - 6
M2_a <- log(ddw$est) - dsgmat3[,wireind] %*% FH_fit3$fit$estcoef$beta[wireind] + dsgmat3[,mailind] %*% FH_fit3$fit$estcoef$beta[mailind]



# approximated mean square error of w_st*b_st'*gamma

# # cov of beta
# cov_beta = solve(t(X)%*%diag(1/(psi+D))%*%X)

# cov_wireless = dsgmat[,c(25, 100:121, 159:174)] %*% cov_beta[c(25, 100:121, 159:174),c(25, 100:121, 159:174)] %*% t(dsgmat[,c(25, 100:121, 159:174)]) 
# var_wireless = split(cov_wireless, row(cov_wireless)-col(cov_wireless))$`0`









########## predictor without mail interactions

lm_fit_2 <- lm(Y~0+factor(stwave)+log(pop):(AL+CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless
+wireless:log(pop)
+wireless:(Wave2+Wave3+Wave4+Wave5+Wave6)
+wireless:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless:log(pop):(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+MailT
,
data=dat[dat$mode_fx=='3',])

dsgmat_2 <- model.matrix(lm_fit_2)
# nu_st by subtraction
uvec_s_2 <- FH_fit3_2$eblup - dsgmat_2 %*% FH_fit3_2$fit$estcoef$beta




lm_fit2_2 <- lm(Y~0+factor(stwave)+log(pop):(AL+CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless
+wireless:log(pop)
+wireless:(Wave2+Wave3+Wave4+Wave5+Wave6)
+wireless:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless:log(pop):(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+MailT
,
data=dd02)

dsgmat2_2 <- model.matrix(lm_fit2_2)



# eblup of M with wireless off
M_b_2 <- dsgmat2_2 %*% FH_fit3_2$fit$estcoef$beta + uvec_s_2	



# calculate mse of M

psi <- FH_fit3_2$fit$refvar
D <- sigma2[dat$mode_fx=='3']
X <- dsgmat_2

# mse of M

g1 <- psi * D / (psi + D)
mat <- ((psi * (dsgmat2_2 - dsgmat_2) + D * dsgmat2_2) / (psi + D))%*%solve(t(dsgmat_2)%*%diag(1/(psi + D))%*%dsgmat_2)%*%t((psi * (dsgmat2_2 - dsgmat_2) + D * dsgmat2_2) / (psi + D))
g2 <- mat[row(mat)==col(mat)]
g3 <- 2 * D^2 / (psi + D)^3 / sum(1 / (psi + D)^2)

mse_M_2 <- g1 + g2 + 2 * g3





# 2nd predictor


ddw2 <- ddw
ddw2$MailT <- 1

lm_fit3_2 <- lm(Y~0+factor(stwave)+log(pop):(AL+CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless
+wireless:log(pop)
+wireless:(Wave2+Wave3+Wave4+Wave5+Wave6)
+wireless:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless:log(pop):(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+MailT
,
data=ddw2)

dsgmat3_2 <- model.matrix(lm_fit3_2)

# Second predictor 
# \hat T_st + b_st'*\hat\mu - w_st*c_st'*\hat\gamma
# wireless index c(97,116:153)
# mail index 
wireind <- c(97,116:153)
mailind <- c(104) - 6
M2_a_2 <- log(ddw$est) - dsgmat3_2[,wireind] %*% FH_fit3_2$fit$estcoef$beta[wireind] + dsgmat3_2[,mailind] * FH_fit3_2$fit$estcoef$beta[mailind]

#############


pdf('Mode_3_logeffort.pdf')

for(i in 1:length(StateNo)){

	plot(c(ddw$Time,mgmail$Time),c(log(ddw$est), log(mgmail$est)),xlab="Time",ylab="log(Effort)",type='n')
	a<-subset(ddw,st==paste(StateNo[i]))
	b<-subset(dd0,st==paste(StateNo[i]))
	lines(a$Time,log(a$est)+ dsgmat3[ddw2$st==paste(StateNo[i]),mailind] %*% FH_fit3$fit$estcoef$beta[mailind],col="gray",lwd=3)
	points(a$Time,log(a$est),col="blue",pch=16,cex=0.7)
	lines(a$Time,M2_a[ddw$st==paste(StateNo[i])],col="black", lwd=3)
	lines(a$Time,M_b[ddw$st==paste(StateNo[i])],col="yellow")
	points(mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3'],log(mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3']),col='magenta',pch=17,cex=1)
	title(main=paste("Mode 3 for", StateName[i],": FIPS =",StateNo[i]))
	legend(x='bottomright', legend=c(expression(widehat(M)[st]), expression(widehat(T)[st]), expression(paste(widehat(T)[st], ' + mail')), expression(paste(widehat(T)[st],' + mail', ' - wireless')), expression(paste('EBLUP(',M[st],')'))), col=c('magenta','blue','gray','black', 'yellow'), lty=c(0,0,1,1,1), pch=c(17,16,NA,NA,NA), lwd=c(NA,NA,3,3,1))

}
dev.off()


# pdf('Mode_3_logeffort2000.pdf')

# for(i in 1:length(StateNo)){

	# plot(c(ddw$Time,mgmail$Time),c(log(ddw$est), log(mgmail$est)),xlab="Time",ylab="log(Effort)",type='n',xlim=c(2000,2016))
	# a<-subset(ddw,st==paste(StateNo[i]))
	# b<-subset(dd0,st==paste(StateNo[i]))
	# lines(a$Time,log(a$est)+ dsgmat3[ddw2$st==paste(StateNo[i]),mailind] %*% FH_fit3$fit$estcoef$beta[mailind],col="gray",lwd=3)
	# points(a$Time,log(a$est),col="blue",pch=16,cex=0.7)
	# lines(a$Time,M2_a[ddw$st==paste(StateNo[i])],col="black", lwd=3)
	# lines(a$Time,M_b[ddw$st==paste(StateNo[i])],col="yellow")
	# points(mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3'],log(mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3']),col='magenta',pch=17,cex=1)
	# title(main=paste("Mode 3 for", StateName[i],": FIPS =",StateNo[i]))
	# legend(x='bottomright', legend=c(expression(widehat(M)[st]), expression(widehat(T)[st]), expression(paste(widehat(T)[st], ' + mail')), expression(paste(widehat(T)[st],' + mail', ' - wireless')), expression(paste('EBLUP(',M[st],')'))), col=c('magenta','blue','gray','black', 'yellow'), lty=c(0,0,1,1,1), pch=c(17,16,NA,NA,NA), lwd=c(NA,NA,3,3,1))

# }
# dev.off()


pdf('Mode_3_effort.pdf')

for(i in 1:length(StateNo)){

	a<-subset(ddw,st==paste(StateNo[i]))
	b<-subset(dd0,st==paste(StateNo[i]))
	plot(c(a$Time,a$Time,mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3']), c((a$est), exp(M_b[ddw$st==paste(StateNo[i])] + mse_M[ddw$st==paste(StateNo[i])]/2), (mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3'])),xlab="Time",ylab="Effort",type='n')
	points(a$Time,(a$est),col="blue",pch=16,cex=0.7)
	lines(a$Time, exp(M_b[ddw$st==paste(StateNo[i])] + mse_M[ddw$st==paste(StateNo[i])]/2),col="black",lwd=2)
	points(mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3'],(mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3']),col='magenta',pch=17,cex=1)
	title(main=paste("Mode 3 for", StateName[i],": FIPS =",StateNo[i]))
	legend(x='topleft', legend=c(expression(widehat(M)[st]), expression(widehat(T)[st]), expression(paste('EBLUP(',M[st],')'))), col=c('magenta','blue', 'black'), lty=c(0,0,1), pch=c(17,16,NA), lwd=c(NA,NA,2))

}
dev.off()


#pdf('Mode_3_effort.pdf')

# for(i in 1:length(StateNo)){

	# a<-subset(ddw,st==paste(StateNo[i]))
	# b<-subset(dd0,st==paste(StateNo[i]))
	# plot(c(a$Time,a$Time,a$Time,mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3']), c((a$est), exp(M2_a[ddw$st==paste(StateNo[i])] + (sigma2_T[dat$st==paste(StateNo[i])&dat$mode_fx==3]+ var_wireless[ddw$st==paste(StateNo[i])] + cov_beta[2,2])/2), exp(log(a$est)+ FH_fit3$fit$estcoef$beta[2] + (sigma2_T[dat$st==paste(StateNo[i])&dat$mode_fx==3]+ cov_beta[2,2])/2), (mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3'])),xlab="Time",ylab="Effort",type='n')
	# lines(a$Time, exp(log(a$est)+ FH_fit3$fit$estcoef$beta[2] + (sigma2_T[dat$st==paste(StateNo[i])&dat$mode_fx==3] + cov_beta[2,2])/2),col="gray",lwd=3)
	# points(a$Time,(a$est),col="gray",pch=16,cex=0.7)
	# lines(a$Time, exp(M2_a[ddw$st==paste(StateNo[i])] + (sigma2_T[dat$st==paste(StateNo[i])&dat$mode_fx==3] + var_wireless[ddw$st==paste(StateNo[i])] + cov_beta[2,2])/2),col="black", lwd=3)
	# lines(a$Time, exp(M_b[ddw$st==paste(StateNo[i])] + mse_M[ddw$st==paste(StateNo[i])]/2),col="yellow")
	# points(mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3'],(mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3']),col='blue',pch=16,cex=1)
	# title(main=paste("Mode 3 for", StateName[i],": FIPS =",StateNo[i]))
	# legend(x='topleft', legend=c(expression(widehat(M)), expression(widehat(T)), expression(paste(widehat(T), ' + ', hat(mu))), expression(paste(widehat(T),' + ', hat(mu), ' - wireless')), 'EBLUP(M)'), col=c('blue','gray','gray','black', 'yellow'), lty=c(0,0,1,1,1), pch=c(16,16,NA,NA,NA), lwd=c(NA,NA,3,3,1))

# }
# dev.off()

# #pdf('Mode_3_effort2000.pdf')

# for(i in 1:length(StateNo)){

	# a<-subset(ddw,st==paste(StateNo[i]))
	# b<-subset(dd0,st==paste(StateNo[i]))
	# plot(c(a$Time,a$Time,a$Time,mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3']), c((a$est), exp(M2_a[ddw$st==paste(StateNo[i])] + (sigma2_T[dat$st==paste(StateNo[i])&dat$mode_fx==3]+ var_wireless[ddw$st==paste(StateNo[i])] + cov_beta[2,2])/2), exp(log(a$est)+ FH_fit3$fit$estcoef$beta[2] + (sigma2_T[dat$st==paste(StateNo[i])&dat$mode_fx==3]+ cov_beta[2,2])/2), (mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3'])),xlab="Time",ylab="Effort",type='n',xlim=c(2000,2016))
	# lines(a$Time, exp(log(a$est)+ FH_fit3$fit$estcoef$beta[2] + (sigma2_T[dat$st==paste(StateNo[i])&dat$mode_fx==3] + cov_beta[2,2])/2),col="gray",lwd=3)
	# points(a$Time,(a$est),col="gray",pch=16,cex=0.7)
	# lines(a$Time, exp(M2_a[ddw$st==paste(StateNo[i])] + (sigma2_T[dat$st==paste(StateNo[i])&dat$mode_fx==3] + var_wireless[ddw$st==paste(StateNo[i])] + cov_beta[2,2])/2),col="black", lwd=3)
	# lines(a$Time, exp(M_b[ddw$st==paste(StateNo[i])] + mse_M[ddw$st==paste(StateNo[i])]/2),col="yellow")
	# points(mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3'],(mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3']),col='blue',pch=16,cex=1)
	# title(main=paste("Mode 3 for", StateName[i],": FIPS =",StateNo[i]))
	# legend(x='topleft', legend=c(expression(widehat(M)), expression(widehat(T)), expression(paste(widehat(T), ' + ', hat(mu))), expression(paste(widehat(T),' + ', hat(mu), ' - wireless')), 'EBLUP(M)'), col=c('blue','gray','gray','black', 'yellow'), lty=c(0,0,1,1,1), pch=c(16,16,NA,NA,NA), lwd=c(NA,NA,3,3,1))

# }
# dev.off()


pdf('ShoreModelogeffortAL.pdf')
i=1
	a<-subset(ddw,st==paste(StateNo[i]))
	b<-subset(dd0,st==paste(StateNo[i]))
plot(c(a$Time,mgmail[mgmail$st==paste(StateNo[i]),]$Time),c(log(a$est), log(mgmail[mgmail$st==paste(StateNo[i]),]$est)),xlab="Time",ylab="log(Effort)",type='n')
	lines(a$Time,log(a$est)+ dsgmat3[ddw2$st==paste(StateNo[i]),mailind] %*% FH_fit3$fit$estcoef$beta[mailind],col="gray",lwd=3)
	points(a$Time,log(a$est),col="blue",pch=16,cex=0.7)
	lines(a$Time,M2_a[ddw$st==paste(StateNo[i])],col="black", lwd=3)
	lines(a$Time,M_b[ddw$st==paste(StateNo[i])],col="yellow")
	points(mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3'],log(mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3']),col='magenta',pch=17,cex=1)
	title(main='Shore Mode log(effort) for Alabama')
	legend(x='bottomright', legend=c(expression(widehat(M)[st]), expression(widehat(T)[st]), expression(paste(widehat(T)[st], ' + mail')), expression(paste(widehat(T)[st],' + mail', ' - wireless')), expression(paste('EBLUP(',M[st],')'))), col=c('magenta','blue','gray','black', 'yellow'), lty=c(0,0,1,1,1), pch=c(17,16,NA,NA,NA), lwd=c(NA,NA,3,3,1))
dev.off()

pdf('ShoreModelogeffortAL_nomailint.pdf')
i=1
	a<-subset(ddw,st==paste(StateNo[i]))
	b<-subset(dd0,st==paste(StateNo[i]))
plot(c(a$Time,mgmail[mgmail$st==paste(StateNo[i]),]$Time),c(log(a$est), log(mgmail[mgmail$st==paste(StateNo[i]),]$est)),xlab="Time",ylab="log(Effort)",type='n')
	lines(a$Time,log(a$est)+ dsgmat3_2[ddw2$st==paste(StateNo[i]),mailind] * FH_fit3_2$fit$estcoef$beta[mailind],col="gray",lwd=3)
	points(a$Time,log(a$est),col="blue",pch=16,cex=0.7)
	lines(a$Time,M2_a_2[ddw$st==paste(StateNo[i])],col="black", lwd=3)
	lines(a$Time,M_b_2[ddw$st==paste(StateNo[i])],col="yellow")
	points(mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3'],log(mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3']),col='magenta',pch=17,cex=1)
	title(main='Shore Mode log(effort) for Alabama Without Mail Interaction')
	legend(x='bottomright', legend=c(expression(widehat(M)[st]), expression(widehat(T)[st]), expression(paste(widehat(T)[st], ' + mail')), expression(paste(widehat(T)[st],' + mail', ' - wireless')), expression(paste('EBLUP(',M[st],')'))), col=c('magenta','blue','gray','black', 'yellow'), lty=c(0,0,1,1,1), pch=c(17,16,NA,NA,NA), lwd=c(NA,NA,3,3,1))
dev.off()




pdf('ShoreModeeffortAL.pdf')
a<-subset(ddw,st==paste(StateNo[i]))
	b<-subset(dd0,st==paste(StateNo[i]))
	plot(c(a$Time,a$Time,mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3']), c((a$est), exp(M_b[ddw$st==paste(StateNo[i])] + mse_M[ddw$st==paste(StateNo[i])]/2), (mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3'])),xlab="Time",ylab="Effort",type='n')
	points(a$Time,(a$est),col="blue",pch=16,cex=0.7)
	lines(a$Time, exp(M_b[ddw$st==paste(StateNo[i])] + mse_M[ddw$st==paste(StateNo[i])]/2),col="black",lwd=2)
	points(mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3'],(mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='3']),col='magenta',pch=17,cex=1)
	title(main='Shore Mode Effort for Alabama')
	legend(x='topleft', legend=c(expression(widehat(M)[st]), expression(widehat(T)[st]), expression(paste('EBLUP(',M[st],')'))), col=c('magenta','blue', 'black'), lty=c(0,0,1), pch=c(17,16,NA), lwd=c(NA,NA,2))
dev.off()






######## mode 7
stwave <- paste(dat[dat$mode_fx=='7',]$st,dat[dat$mode_fx=='7',]$wave, sep='_')


# borrow the design matrix from lm
lm_fit <- lm(Y~0+factor(stwave)+log(pop):(AL+CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless
+wireless:log(pop)
+wireless:(Wave2+Wave3+Wave4+Wave5+Wave6)
+wireless:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless:log(pop):(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+MailT
+MailT:(Wave2+Wave3+Wave4+Wave5+Wave6)
+MailT:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA),
data=dat[dat$mode_fx=='7',])

dsgmat <- model.matrix(lm_fit)
# nu_st by subtraction
uvec_s <- FH_fit7$eblup - dsgmat %*% FH_fit7$fit$estcoef$beta


ddw<-subset(dat,mode_fx==7)
ddw$Time <- as.numeric(ddw$year)+(as.numeric(ddw$wave)-1)/6
dd0<-ddw
dd0$wireless<-0*dd0$wireless
dd02 <- dd0
dd02$MailT <- 1

StateNo<-sort(as.numeric(unique(ddw$st)))
StateName<-c("AL","CT","DE","FL","GA","LA","ME","MD","MA","MS","NH","NJ","NY","NC","RI","SC","VA")


lm_fit2 <- lm(Y~0+factor(stwave)+log(pop):(AL+CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless
+wireless:log(pop)
+wireless:(Wave2+Wave3+Wave4+Wave5+Wave6)
+wireless:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless:log(pop):(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+MailT
+MailT:(Wave2+Wave3+Wave4+Wave5+Wave6)
+MailT:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA),
data=dd02)

dsgmat2 <- model.matrix(lm_fit2)

# # eblup of M with wireless on
# M_a <- cbind(1, 1, dsgmat[,-c(1,2)]) %*% FH_fit3$fit$estcoef$beta + uvec_s

# eblup of M with wireless off
M_b <- dsgmat2 %*% FH_fit7$fit$estcoef$beta + uvec_s	


mgmail$Time <- as.numeric(mgmail$year)+(as.numeric(mgmail$wave)-1)/6
	


# calculate mse of M

psi <- FH_fit7$fit$refvar
D <- sigma2[dat$mode_fx=='7']
X <- dsgmat

# mse of M

g1 <- psi * D / (psi + D)
mat <- ((psi * (dsgmat2 - dsgmat) + D * dsgmat2) / (psi + D))%*%solve(t(dsgmat)%*%diag(1/(psi + D))%*%dsgmat)%*%t((psi * (dsgmat2 - dsgmat) + D * dsgmat2) / (psi + D))
g2 <- mat[row(mat)==col(mat)]
g3 <- 2 * D^2 / (psi + D)^3 / sum(1 / (psi + D)^2)

mse_M <- g1 + g2 + 2 * g3





# 2nd predictor


ddw2 <- ddw
ddw2$MailT <- 1

lm_fit3 <- lm(Y~0+factor(stwave)+log(pop):(AL+CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless
+wireless:log(pop)
+wireless:(Wave2+Wave3+Wave4+Wave5+Wave6)
+wireless:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless:log(pop):(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+MailT
+MailT:(Wave2+Wave3+Wave4+Wave5+Wave6)
+MailT:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA),
data=ddw2)

dsgmat3 <- model.matrix(lm_fit3)

# Second predictor 
# \hat T_st + b_st'*\hat\mu - w_st*c_st'*\hat\gamma
# wireless index c(97,116:137,159:174)
# mail index 
wireind <- c(97,116:137,159:174)+1
mailind <- c(104,144:164) - 6 + 1
M2_a <- log(ddw$est) - dsgmat3[,wireind] %*% FH_fit7$fit$estcoef$beta[wireind] + dsgmat3[,mailind] %*% FH_fit7$fit$estcoef$beta[mailind]



# approximated mean square error of w_st*b_st'*gamma

# # # cov of beta
# cov_beta = solve(t(X)%*%diag(1/(psi+D))%*%X)

# cov_wireless = dsgmat[,c(25, 100:121, 159:174)] %*% cov_beta[c(25, 100:121, 159:174),c(25, 100:121, 159:174)] %*% t(dsgmat[,c(25, 100:121, 159:174)]) 
# var_wireless = split(cov_wireless, row(cov_wireless)-col(cov_wireless))$`0`










########### predictor without mail interaction

lm_fit_2 <- lm(Y~0+factor(stwave)+log(pop):(AL+CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless
+wireless:log(pop)
+wireless:(Wave2+Wave3+Wave4+Wave5+Wave6)
+wireless:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless:log(pop):(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+MailT
,
data=dat[dat$mode_fx=='7',])

dsgmat_2 <- model.matrix(lm_fit_2)
# nu_st by subtraction
uvec_s_2 <- FH_fit7_2$eblup - dsgmat_2 %*% FH_fit7_2$fit$estcoef$beta




lm_fit2_2 <- lm(Y~0+factor(stwave)+log(pop):(AL+CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless
+wireless:log(pop)
+wireless:(Wave2+Wave3+Wave4+Wave5+Wave6)
+wireless:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless:log(pop):(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+MailT
,
data=dd02)

dsgmat2_2 <- model.matrix(lm_fit2_2)



# eblup of M with wireless off
M_b_2 <- dsgmat2_2 %*% FH_fit7_2$fit$estcoef$beta + uvec_s_2	



# calculate mse of M

psi <- FH_fit7_2$fit$refvar
D <- sigma2[dat$mode_fx=='7']
X <- dsgmat_2

# mse of M

g1 <- psi * D / (psi + D)
mat <- ((psi * (dsgmat2_2 - dsgmat_2) + D * dsgmat2_2) / (psi + D))%*%solve(t(dsgmat_2)%*%diag(1/(psi + D))%*%dsgmat_2)%*%t((psi * (dsgmat2_2 - dsgmat_2) + D * dsgmat2_2) / (psi + D))
g2 <- mat[row(mat)==col(mat)]
g3 <- 2 * D^2 / (psi + D)^3 / sum(1 / (psi + D)^2)

mse_M_2 <- g1 + g2 + 2 * g3





# 2nd predictor


ddw2 <- ddw
ddw2$MailT <- 1

lm_fit3_2 <- lm(Y~0+factor(stwave)+log(pop):(AL+CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless
+wireless:log(pop)
+wireless:(Wave2+Wave3+Wave4+Wave5+Wave6)
+wireless:(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+wireless:log(pop):(CT+DE+FL+GA+LA+MA+MD+ME+MS+NC+NH+NJ+NY+RI+SC+VA)
+MailT
,
data=ddw2)

dsgmat3_2 <- model.matrix(lm_fit3_2)

# Second predictor 
# \hat T_st + b_st'*\hat\mu - w_st*c_st'*\hat\gamma
# wireless index c(97,116:153)
# mail index 
wireind <- c(97,116:153)+1
mailind <- c(104) - 6 + 1
M2_a_2 <- log(ddw$est) - dsgmat3_2[,wireind] %*% FH_fit7_2$fit$estcoef$beta[wireind] + dsgmat3_2[,mailind] * FH_fit7_2$fit$estcoef$beta[mailind]


##############








pdf('Mode_7_logeffort.pdf')

for(i in 1:length(StateNo)){

	plot(c(ddw$Time,mgmail$Time),c(log(ddw$est), log(mgmail$est)),xlab="Time",ylab="log(Effort)",type='n')
	a<-subset(ddw,st==paste(StateNo[i]))
	b<-subset(dd0,st==paste(StateNo[i]))
	lines(a$Time,log(a$est)+ dsgmat3[ddw2$st==paste(StateNo[i]),mailind] %*% FH_fit7$fit$estcoef$beta[mailind],col="gray",lwd=3)
	points(a$Time,log(a$est),col="blue",pch=16,cex=0.7)
	lines(a$Time,M2_a[ddw$st==paste(StateNo[i])],col="black", lwd=3)
	lines(a$Time,M_b[ddw$st==paste(StateNo[i])],col="yellow")
	points(mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7'],log(mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7']),col='magenta',pch=17,cex=1)
	title(main=paste("Mode 7 for", StateName[i],": FIPS =",StateNo[i]))
	legend(x='bottomright', legend=c(expression(widehat(M)[st]), expression(widehat(T)[st]), expression(paste(widehat(T)[st], ' + mail')), expression(paste(widehat(T)[st],' + mail', ' - wireless')), expression(paste('EBLUP(',M[st],')'))), col=c('magenta','blue','gray','black', 'yellow'), lty=c(0,0,1,1,1), pch=c(17,16,NA,NA,NA), lwd=c(NA,NA,3,3,1))

}
dev.off()



pdf('Mode_7_effort.pdf')

for(i in 1:length(StateNo)){

	a<-subset(ddw,st==paste(StateNo[i]))
	b<-subset(dd0,st==paste(StateNo[i]))
	plot(c(a$Time,a$Time,mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7']), c((a$est), exp(M_b[ddw$st==paste(StateNo[i])] + mse_M[ddw$st==paste(StateNo[i])]/2), (mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7'])),xlab="Time",ylab="Effort",type='n')
	points(a$Time,(a$est),col="blue",pch=16,cex=0.7)
	lines(a$Time, exp(M_b[ddw$st==paste(StateNo[i])] + mse_M[ddw$st==paste(StateNo[i])]/2),col="black",lwd=2)
	points(mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7'],(mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7']),col='magenta',pch=17,cex=1)
	title(main=paste("Mode 7 for", StateName[i],": FIPS =",StateNo[i]))
	legend(x='topleft', legend=c(expression(widehat(M)[st]), expression(widehat(T)[st]), expression(paste('EBLUP(',M[st],')'))), col=c('magenta','blue', 'black'), lty=c(0,0,1), pch=c(17,16,NA), lwd=c(NA,NA,2))

}
dev.off()




# pdf('Mode_7_logeffort2000.pdf')

# for(i in 1:length(StateNo)){

	# plot(c(ddw$Time,mgmail$Time),c(log(ddw$est), log(mgmail$est)),xlab="Time",ylab="log(Effort)",type='n',xlim=c(2000,2016))
	# a<-subset(ddw,st==paste(StateNo[i]))
	# b<-subset(dd0,st==paste(StateNo[i]))
	# lines(a$Time,log(a$est)+ FH_fit7$fit$estcoef$beta[2],col="gray",lwd=3)
	# points(a$Time,log(a$est),col="gray",pch=16,cex=0.7)
	# lines(a$Time,M2_a[ddw$st==paste(StateNo[i])],col="black", lwd=3)
	# lines(a$Time,M_b[ddw$st==paste(StateNo[i])],col="yellow")
	# points(mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7'],log(mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7']),col='blue',pch=16,cex=1)
	# title(main=paste("Mode 7 for", StateName[i],": FIPS =",StateNo[i]))
	# legend(x='bottomright', legend=c(expression(widehat(M)), expression(widehat(T)), expression(paste(widehat(T), ' + ', hat(mu))), expression(paste(widehat(T),' + ', hat(mu), ' - wireless')), 'EBLUP(M)'), col=c('blue','gray','gray','black', 'yellow'), lty=c(0,0,1,1,1), pch=c(16,16,NA,NA,NA), lwd=c(NA,NA,3,3,1))

# }
# dev.off()







# pdf('Mode_7_effort.pdf')

# for(i in 1:length(StateNo)){

	# a<-subset(ddw,st==paste(StateNo[i]))
	# b<-subset(dd0,st==paste(StateNo[i]))
	# plot(c(a$Time,a$Time,a$Time,mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7']),c((a$est), exp(M2_a[ddw$st==paste(StateNo[i])] + (sigma2_T[dat$st==paste(StateNo[i])&dat$mode_fx==7] + var_wireless[ddw$st==paste(StateNo[i])] + cov_beta[2,2])/2), exp(log(a$est)+ FH_fit7$fit$estcoef$beta[2] + (sigma2_T[dat$st==paste(StateNo[i])&dat$mode_fx==7] + cov_beta[2,2])/2), (mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7'])),xlab="Time",ylab="Effort",type='n')
	# lines(a$Time, exp(log(a$est)+ FH_fit7$fit$estcoef$beta[2] + (sigma2_T[dat$st==paste(StateNo[i])&dat$mode_fx==7] + cov_beta[2,2])/2),col="gray",lwd=3)
	# points(a$Time,(a$est),col="gray",pch=16,cex=0.7)
	# lines(a$Time, exp(M2_a[ddw$st==paste(StateNo[i])] + (sigma2_T[dat$st==paste(StateNo[i])&dat$mode_fx==7] + var_wireless[ddw$st==paste(StateNo[i])] + cov_beta[2,2])/2),col="black", lwd=3)
	# lines(a$Time, exp(M_b[ddw$st==paste(StateNo[i])] + mse_M[ddw$st==paste(StateNo[i])]/2),col="yellow")
	# points(mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7'],(mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7']),col='blue',pch=16,cex=1)
	# title(main=paste("Mode 7 for", StateName[i],": FIPS =",StateNo[i]))
	# legend(x='topleft', legend=c(expression(widehat(M)), expression(widehat(T)), expression(paste(widehat(T), ' + ', hat(mu))), expression(paste(widehat(T),' + ', hat(mu), ' - wireless')), 'EBLUP(M)'), col=c('blue','gray','gray','black', 'yellow'), lty=c(0,0,1,1,1), pch=c(16,16,NA,NA,NA), lwd=c(NA,NA,3,3,1))


# }
# dev.off()


# pdf('Mode_7_effort2000.pdf')

# for(i in 1:length(StateNo)){

	# a<-subset(ddw,st==paste(StateNo[i]))
	# b<-subset(dd0,st==paste(StateNo[i]))
	# plot(c(a$Time,a$Time,a$Time,mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7']),c((a$est), exp(M2_a[ddw$st==paste(StateNo[i])] + (sigma2_T[dat$st==paste(StateNo[i])&dat$mode_fx==7] + var_wireless[ddw$st==paste(StateNo[i])] + cov_beta[2,2])/2), exp(log(a$est)+ FH_fit7$fit$estcoef$beta[2] + (sigma2_T[dat$st==paste(StateNo[i])&dat$mode_fx==7] + cov_beta[2,2])/2), (mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7'])),xlab="Time",ylab="Effort",type='n',xlim=c(2000,2016))
	# lines(a$Time, exp(log(a$est)+ FH_fit7$fit$estcoef$beta[2] + (sigma2_T[dat$st==paste(StateNo[i])&dat$mode_fx==7] + cov_beta[2,2])/2),col="gray",lwd=3)
	# points(a$Time,(a$est),col="gray",pch=16,cex=0.7)
	# lines(a$Time, exp(M2_a[ddw$st==paste(StateNo[i])] + (sigma2_T[dat$st==paste(StateNo[i])&dat$mode_fx==7] + var_wireless[ddw$st==paste(StateNo[i])] + cov_beta[2,2])/2),col="black", lwd=3)
	# lines(a$Time, exp(M_b[ddw$st==paste(StateNo[i])] + mse_M[ddw$st==paste(StateNo[i])]/2),col="yellow")
	# points(mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7'],(mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7']),col='blue',pch=16,cex=1)
	# title(main=paste("Mode 7 for", StateName[i],": FIPS =",StateNo[i]))
	# legend(x='topleft', legend=c(expression(widehat(M)), expression(widehat(T)), expression(paste(widehat(T), ' + ', hat(mu))), expression(paste(widehat(T),' + ', hat(mu), ' - wireless')), 'EBLUP(M)'), col=c('blue','gray','gray','black', 'yellow'), lty=c(0,0,1,1,1), pch=c(16,16,NA,NA,NA), lwd=c(NA,NA,3,3,1))


# }
# dev.off()

#rm(list=setdiff(ls(),c()))


i=4
pdf('BoatModelogeffortFL.pdf')
	a<-subset(ddw,st==paste(StateNo[i]))
	b<-subset(dd0,st==paste(StateNo[i]))
plot(c(a$Time,mgmail[mgmail$st==paste(StateNo[i]),]$Time),c(log(a$est), log(mgmail[mgmail$st==paste(StateNo[i]),]$est)),xlab="Time",ylab="log(Effort)",type='n')
	lines(a$Time,log(a$est)+ dsgmat3[ddw2$st==paste(StateNo[i]),mailind] %*% FH_fit7$fit$estcoef$beta[mailind],col="gray",lwd=3)
	points(a$Time,log(a$est),col="blue",pch=16,cex=0.7)
	lines(a$Time,M2_a[ddw$st==paste(StateNo[i])],col="black", lwd=3)
	lines(a$Time,M_b[ddw$st==paste(StateNo[i])],col="yellow")
	points(mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7'],log(mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7']),col='magenta',pch=17,cex=1)
	title(main='Private Boat Mode log(effort) for Florida')
	legend(x='bottomright', legend=c(expression(widehat(M)[st]), expression(widehat(T)[st]), expression(paste(widehat(T)[st], ' + mail')), expression(paste(widehat(T)[st],' + mail', ' - wireless')), expression(paste('EBLUP(',M[st],')'))), col=c('magenta','blue','gray','black', 'yellow'), lty=c(0,0,1,1,1), pch=c(17,16,NA,NA,NA), lwd=c(NA,NA,3,3,1))
dev.off()




pdf('BoatModelogeffortFL_nomailint.pdf')
	a<-subset(ddw,st==paste(StateNo[i]))
	b<-subset(dd0,st==paste(StateNo[i]))
plot(c(a$Time,mgmail[mgmail$st==paste(StateNo[i]),]$Time),c(log(a$est), log(mgmail[mgmail$st==paste(StateNo[i]),]$est)),xlab="Time",ylab="log(Effort)",type='n')
	lines(a$Time,log(a$est)+ dsgmat3_2[ddw2$st==paste(StateNo[i]),mailind] * FH_fit7_2$fit$estcoef$beta[mailind],col="gray",lwd=3)
	points(a$Time,log(a$est),col="blue",pch=16,cex=0.7)
	lines(a$Time,M2_a_2[ddw$st==paste(StateNo[i])],col="black", lwd=3)
	lines(a$Time,M_b_2[ddw$st==paste(StateNo[i])],col="yellow")
	points(mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7'],log(mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7']),col='magenta',pch=17,cex=1)
	title(main='Private Boat Mode log(effort) for Florida Without Mail Interaction')
	legend(x='bottomright', legend=c(expression(widehat(M)[st]), expression(widehat(T)[st]), expression(paste(widehat(T)[st], ' + mail')), expression(paste(widehat(T)[st],' + mail', ' - wireless')), expression(paste('EBLUP(',M[st],')'))), col=c('magenta','blue','gray','black', 'yellow'), lty=c(0,0,1,1,1), pch=c(17,16,NA,NA,NA), lwd=c(NA,NA,3,3,1))
dev.off()






pdf('BoatModeeffortFL.pdf')
a<-subset(ddw,st==paste(StateNo[i]))
	b<-subset(dd0,st==paste(StateNo[i]))
	plot(c(a$Time,a$Time,mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7']), c((a$est), exp(M_b[ddw$st==paste(StateNo[i])] + mse_M[ddw$st==paste(StateNo[i])]/2), (mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7'])),xlab="Time",ylab="Effort",type='n')
	points(a$Time,(a$est),col="blue",pch=16,cex=0.7)
	lines(a$Time, exp(M_b[ddw$st==paste(StateNo[i])] + mse_M[ddw$st==paste(StateNo[i])]/2),col="black",lwd=2)
	points(mgmail$Time[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7'],(mgmail$est[mgmail$st==paste(StateNo[i])&mgmail$mode_fx=='7']),col='magenta',pch=17,cex=1)
	title(main='Private Boat Mode Effort for Florida')
	legend(x='topleft', legend=c(expression(widehat(M)[st]), expression(widehat(T)[st]), expression(paste('EBLUP(',M[st],')'))), col=c('magenta','blue', 'black'), lty=c(0,0,1), pch=c(17,16,NA), lwd=c(NA,NA,2))
	dev.off()







