# This piece of code reads the population data. 

# Before you start, you should download 
# st8084ts.txt
# st8590ts.txt
# st9093ts.txt
# st9499ts.txt
# st0010ts.csv
# st1115ts.csv
# st16ts.csv
# from https://github.com/Teng976/NOAA_MRIP 
# and put these files in your working directory.

# You should have a file called wireless_only_rates.sas7bdat in your directory.

# After running the code, you'll find a file called Pop.RData in your directory.


library(sas7bdat)
library(maps)

rm(list=ls())

cell <- read.sas7bdat('wireless_only_rates.sas7bdat')

fips <- state.fips[-c(20,22,23,34,36,37,38,40,53,54,56:59),c(1,5)]

pop80to84 <- read.table('st8084ts.txt')
pop85to90 <- read.table('st8590ts.txt')
pop82to89 <- cbind(pop80to84, pop85to90)[,3:10]
pop82to89[,9] <- rownames(pop82to89)
colnames(pop82to89)[9] <- 'abb'
pop82to89 <- merge(pop82to89, fips, by = 'abb')

pop82to89 <- pop82to89[pop82to89$fips%in%unique(cell$st),]

pop82to89 <- data.frame(cbind(rep(unique(cell$st),each=8),rep(1982:1989,17), c(t(pop82to89[,2:9])))) 
colnames(pop82to89) <- c('st', 'year', 'pop')

pop82to89

pop94to99 <- read.table('st9499ts.txt')
rownames(pop94to99) <- rownames(pop85to90)
pop90to93 <- read.table('st9093ts.txt')
rownames(pop90to93) <- rownames(pop85to90)
pop90to99 <- cbind(pop94to99, pop90to93[,-5])


pop90to99[,11] <- rownames(pop90to99)
colnames(pop90to99)[11] <- 'abb'
pop90to99 <- merge(pop90to99, fips, by = 'abb')

pop90to99 <- pop90to99[pop90to99$fips%in%unique(cell$st),]

pop90to99 <- data.frame(cbind(rep(unique(cell$st),each=10),rep(1999:1990,17), c(t(pop90to99[,2:11])))) 
colnames(pop90to99) <- c('st', 'year', 'pop')

pop00to10 <- read.csv('st0010ts.csv')
pop00to10 <- pop00to10[,-c(12,13)]
rownames(pop00to10) <- rownames(pop85to90)
pop00to10[,12] <- rownames(pop00to10)
colnames(pop00to10)[12] <- 'abb'
pop00to10 <- merge(pop00to10, fips, by = 'abb')
pop00to10 <- pop00to10[pop00to10$fips%in%unique(cell$st),]

pop00to10 <- data.frame(cbind(rep(unique(cell$st),each=11),rep(2000:2010,17), c(t(pop00to10[,2:12])))) 
colnames(pop00to10) <- c('st', 'year', 'pop')

pop11to15 <- read.csv('st1115ts.csv')
pop11to15[,7] <- rownames(pop85to90)
colnames(pop11to15)[7] <- 'abb'
pop11to15 <- merge(pop11to15, fips, by = 'abb')
pop11to15 <- pop11to15[pop11to15$fips%in%unique(cell$st),]
pop11to15 <- data.frame(cbind(rep(unique(cell$st),each=5),rep(2011:2015,17), c(t(pop11to15[,3:7])))) 
colnames(pop11to15) <- c('st', 'year', 'pop')


pop16 <- read.csv('st16ts.csv',header=F)
pop16[,3] <- rownames(pop85to90)
colnames(pop16)[3] <- 'abb'
pop16 <- merge(pop16, fips, by = 'abb')
pop16 <- pop16[pop16$fips%in%unique(cell$st),]
pop16 <- data.frame(cbind(rep(unique(cell$st),each=1),rep(2016,17), pop16[,3])) 
colnames(pop16) <- c('st', 'year', 'pop')


pop82to99 <- merge(pop82to89, pop90to99, all=T, by=c('st', 'year', 'pop'))
pop82to10 <- merge(pop82to99, pop00to10, all=T, by=c('st', 'year', 'pop'))
pop82to15 <- merge(pop82to10, pop11to15, all=T, by=c('st', 'year', 'pop'))
pop82to16 <- merge(pop82to15, pop16, all=T, by=c('st', 'year', 'pop'))

rm(list=setdiff(ls(),'pop82to16'))

save.image('Pop.RData')
