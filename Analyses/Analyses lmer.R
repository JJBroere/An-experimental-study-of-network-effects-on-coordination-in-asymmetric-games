library(lme4)
library(xlsx)
library(foreign)
library(stargazer)

setwd("C:/Users/Joris/Google Drive/Ph.D/PaperMadrid/Data/Masterfile")
datsPAS1 <- read.xlsx("PAsession1.xlsx", sheetIndex=1, header=F)
datsPAS2 <- read.xlsx("PAsession2.xlsx", sheetIndex=1, header=F)
datsPAS3 <- read.xlsx("PAsession3.xlsx", sheetIndex=1, header=F)
datsPAS4 <- read.xlsx("PAsession4.xlsx", sheetIndex=1, header=F)
datsPAS5 <- read.xlsx("PAsession5.xlsx", sheetIndex=1, header=F)
datsPAS6 <- read.xlsx("PAsession6.xlsx", sheetIndex=1, header=F)
datsPAS7 <- read.xlsx("PAsession7.xlsx", sheetIndex=1, header=F)
datsPAS8 <- read.xlsx("PAsession8.xlsx", sheetIndex=1, header=F)
datsPAS9 <- read.xlsx("PAsession9.xlsx", sheetIndex=1, header=F)
datsPAS10 <- read.xlsx("PAsession10.xlsx", sheetIndex=1, header=F)
datsPAS11 <- read.xlsx("PAsession11.xlsx", sheetIndex=1, header=F)
datsPAS12 <- read.xlsx("PAsession12.xlsx", sheetIndex=1, header=F)

data <- rbind(datsPAS1,datsPAS2)
data <- rbind(data,datsPAS3)
data <- rbind(data,datsPAS4)
data <- rbind(data,datsPAS5)
data <- rbind(data,datsPAS6)
data <- rbind(data,datsPAS7)
data <- rbind(data,datsPAS8)
data <- rbind(data,datsPAS9)
data <- rbind(data,datsPAS10)
data <- rbind(data,datsPAS11)
data <- rbind(data,datsPAS12)
data <- as.data.frame(data)
colnames(data) <- c("id", "blue", "Deg","type", "round")
head(data)
data$type2 <- 0
data$type2[data$type=="Azul"] <- 1
data$type2[data$type=="Blue"] <- 1
data$Change <- 0

for(i in 1:nrow(data)){
  data$Change[i] <-ifelse(data$type2[i]!=data$blue[i],1,0)
}

data$Even <- 0
data$Even[data$Deg==2] <- 1
data$Even[data$Deg==4] <- 1
data$Even[data$Deg==6] <- 1
data$Even[data$Deg==8] <- 1
data$Even[data$Deg==10] <- 1
data$Even[data$Deg==12] <- 1
data$Even[data$Deg==14] <- 1
data$Even[data$Deg==16] <- 1

data$Deg2 <- (data$Deg-min(data$Deg))/max(data$Deg)
PA <- glmer((1-Change) ~ 1 + Deg + Even + (1|id) , data=data,family=binomial)
summary(PA)
dataPA <- data


datsSWS1 <- read.xlsx("SWsession1.xlsx", sheetIndex=1, header=F)
datsSWS2 <- read.xlsx("SWsession2.xlsx", sheetIndex=1, header=F)
datsSWS3 <- read.xlsx("SWsession3.xlsx", sheetIndex=1, header=F)
datsSWS4 <- read.xlsx("SWsession4.xlsx", sheetIndex=1, header=F)
datsSWS5 <- read.xlsx("SWsession5.xlsx", sheetIndex=1, header=F)
datsSWS6 <- read.xlsx("SWsession6.xlsx", sheetIndex=1, header=F)
datsSWS7 <- read.xlsx("SWsession7.xlsx", sheetIndex=1, header=F)
datsSWS8 <- read.xlsx("SWsession8.xlsx", sheetIndex=1, header=F)
datsSWS9 <- read.xlsx("SWsession9.xlsx", sheetIndex=1, header=F)
datsSWS10 <- read.xlsx("SWsession10.xlsx", sheetIndex=1, header=F)
datsSWS11 <- read.xlsx("SWsession11.xlsx", sheetIndex=1, header=F)
datsSWS12 <- read.xlsx("SWsession12.xlsx", sheetIndex=1, header=F)
head(datsSWS7)
data <- rbind(datsSWS1,datsSWS2)
data <- rbind(data,datsSWS3)
data <- rbind(data,datsSWS4)
data <- rbind(data,datsSWS5)
data <- rbind(data,datsSWS6)
data <- rbind(data,datsSWS7)
data <- rbind(data,datsSWS8)
data <- rbind(data,datsSWS9)
data <- rbind(data,datsSWS10)
data <- rbind(data,datsSWS11)
data <- rbind(data,datsSWS12)
data <- as.data.frame(data)
colnames(data) <- c("id", "blue", "Deg", "type", "round")
head(data)
data$type2 <- 0
data$type2[data$type=="Azul"] <- 1
data$type2[data$type=="Blue"] <- 1
data$Change <- 0

for(i in 1:nrow(data)){
  data$Change[i] <-ifelse(data$type2[i]!=data$blue[i],1,0)
}

data$Even <- 0
data$Even[data$Deg==2] <- 1
data$Even[data$Deg==4] <- 1
data$Even[data$Deg==6] <- 1
data$Even[data$Deg==8] <- 1
data$Even[data$Deg==10] <- 1
data$Even[data$Deg==12] <- 1
data$Even[data$Deg==14] <- 1
data$Even[data$Deg==16] <- 1

data$Deg2 <- (data$Deg-min(data$Deg))/max(data$Deg)
SW <- glmer((1-Change) ~ 1 + Deg + Even + (1|id) , data=data,family=binomial)
summary(SW)

dataSW <- data


datsERS1 <- read.xlsx("ERsession1.xlsx", sheetIndex=1, header=F)
datsERS2 <- read.xlsx("ERsession2.xlsx", sheetIndex=1, header=F)
datsERS3 <- read.xlsx("ERsession3.xlsx", sheetIndex=1, header=F)
datsERS4 <- read.xlsx("ERsession4.xlsx", sheetIndex=1, header=F)
datsERS5 <- read.xlsx("ERsession5.xlsx", sheetIndex=1, header=F)
datsERS6 <- read.xlsx("ERsession6.xlsx", sheetIndex=1, header=F)
datsERS7 <- read.xlsx("ERsession7.xlsx", sheetIndex=1, header=F)
datsERS8 <- read.xlsx("ERsession8.xlsx", sheetIndex=1, header=F)
datsERS9 <- read.xlsx("ERsession9.xlsx", sheetIndex=1, header=F)
datsERS10 <- read.xlsx("ERsession10.xlsx", sheetIndex=1, header=F)
datsERS11 <- read.xlsx("ERsession11.xlsx", sheetIndex=1, header=F)
datsERS12 <- read.xlsx("ERsession12.xlsx", sheetIndex=1, header=F)

data <- rbind(datsERS1,datsERS2)
data <- rbind(data,datsERS3)
data <- rbind(data,datsERS4)
data <- rbind(data,datsERS5)
data <- rbind(data,datsERS6)
data <- rbind(data,datsERS7)
data <- rbind(data,datsERS8)
data <- rbind(data,datsERS9)
data <- rbind(data,datsERS10)
data <- rbind(data,datsERS11)
data <- rbind(data,datsERS12)
data <- as.data.frame(data)
colnames(data) <- c("id", "blue", "Deg", "type", "round")
head(data)
data$type2 <- 0
data$type2[data$type=="Azul"] <- 1
data$type2[data$type=="Blue"] <- 1
data$Change <- 0

for(i in 1:nrow(data)){
  data$Change[i] <-ifelse(data$type2[i]!=data$blue[i],1,0)
}

data$Even <- 0
data$Even[data$Deg==2] <- 1
data$Even[data$Deg==4] <- 1
data$Even[data$Deg==6] <- 1
data$Even[data$Deg==8] <- 1
data$Even[data$Deg==10] <- 1
data$Even[data$Deg==12] <- 1
data$Even[data$Deg==14] <- 1
data$Even[data$Deg==16] <- 1

data$Deg2 <- (data$Deg-min(data$Deg))/max(data$Deg)
ER <- glmer((1-Change) ~ 1 + Deg + Even + (1|id) , data=data,family=binomial)
summary(ER)
dataER <- data


dataAll <- rbind(dataPA, dataSW)
dataAll <- rbind(dataAll, dataER)
dataAll$netid <- 1
dataAll$netid[4801:9600] <- 2
dataAll$netid[9601:14400] <- 3
dataAll$deg3 <- dataAll$Deg2^2

head(dataAll)

All <- glmer((1-Change) ~ 1 + Deg+ Even + (1|id) + (1|netid) , data=dataAll,family=binomial)
summary(All)

stargazer(All, ER, SW, PA, single.row=TRUE,title="Regression Results")
