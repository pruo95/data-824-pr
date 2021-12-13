library(shiny)
library(shiny)
library(ggplot2)
library(Lahman)
library(dplyr)
library(tidyverse)
library(MASS)
library(gam)
library(car)
library(pROC)
library(profileModel)
library(shiny)
library(ggplot2)
library(caret)
library(nnet)


### original data scraping to get the proper datatable

hof <- data.frame(HallOfFame)
hofy <- hof[hof$inducted == "Y",]
hofy <- hofy[hofy$category == "Player",]
hofy <- hofy[,c("playerID","inducted")]
names(hofy)[names(hofy) == 'inducted'] <- 'HoF'
hofy$HoF <- 1

hofn <- hof[hof$inducted == "N",]
hofn <- hofn[hofn$category == "Player",]
hofn <- hofn[,c("playerID","inducted")]
names(hofn)[names(hofn) == 'inducted'] <- 'HoF'
hofn$HoF <- 0
hofn <- hofn[!duplicated(hofn),]

hofe <- merge(hofy,hofn,by = "playerID", all = T)

hofe$HoF <- ifelse(hofe$HoF.x == 1,1)
hofe[is.na(hofe)] <- 0
hofe <- hofe[,c("playerID","HoF")]

batting <- Batting %>% group_by(playerID) %>% summarize(G = sum(G), AB = sum(AB), R = sum(R), H = sum(H), X2B = sum(X2B), X3B = sum(X3B), HR = sum(HR),
                                                        RBI = sum(RBI), SB = sum(SB), CS = sum(CS), BB = sum(BB), SO = sum(SO),IBB = sum(IBB), HBP = sum(HBP),
                                                        SH = sum(SH), SF = sum(SF), GIDP = sum(GIDP), seasons = n(),last = max(yearID))

stats <- battingStats(data = batting, idvars = c("playerID","last","seasons"))
stats[is.na(stats)] <- 0

#eligible <- stats[stats$seasons >= 10,]
#eligible <- eligible[eligible$last <= 2012,]

awardCounts <- AwardsPlayers %>% group_by(playerID) %>% summarize(awards = n())

ASGcounts <- AllstarFull %>% group_by(playerID) %>% summarize(ASG = n())

#eligible <- merge(eligible, awardCounts, by = "playerID", all.x = T, all.y = F)
#eligible[is.na(eligible)] <- 0

position <- Appearances %>% group_by(playerID) %>% summarize(G = sum(G_all), P = sum(G_p), first = sum(G_1b), second = sum(G_2b), third = sum(G_3b),
                                                             short = sum(G_ss), left = sum(G_lf), center = sum(G_cf), right = sum(G_rf), out = sum(G_of),
                                                             dh = sum(G_dh), catch = sum(G_c))

position[is.na(position)] <- 0

position$pitcher <- apply(position[,c(3:13)],1,max)

i <- 1

for (i in 1:nrow(position)) {
  if (position[i,3] == position[i,14]) {
    position[i,14] <- 1
  } else {
    position[i,14] <- 0
  }
  i <- i+1
}

position <- position[,c("playerID","pitcher")]


# merge
eligible <- merge(hofe,stats, by = "playerID", all.x = T, all.y = F)
eligible <- merge(eligible,ASGcounts, by = "playerID", all.x = T, all.y = F)
eligible <- merge(eligible,awardCounts, by = "playerID", all.x = T, all.y = F)

eligible <- merge(eligible,position,by = "playerID", all.x = T, all.y = F)
eligible <- eligible[eligible$pitcher == 0,]

eligible <- eligible[!is.na(eligible$pitcher),]

# <- merge(eligible,hof,by = "playerID",all.x = T, all.y = F)
eligible[is.na(eligible)] <- 0

drops <- c("pitcher")

hofData <- eligible[ , !(names(eligible) %in% drops)]

hofData$HoF <- ifelse(hofData$HoF == 1, "Inducted", "Not Inducted")

hofData$HoF <- as.factor(hofData$HoF)
hofData <- hofData[!(hofData$seasons < 10),]

noPlayers <- hofData[,-1]
rownames(noPlayers) <- hofData[,1]
noPlayers$PA <- NULL
noPlayers$TB <- NULL
noPlayers$OPS <- NULL
noPlayers$SF <- NULL
noPlayers$CS <- NULL
noPlayers$IBB <- NULL
noPlayers$GIDP <- NULL
noPlayers$HoF <- as.factor(noPlayers$HoF)

noLast <- noPlayers
noLast$last <- NULL # I don't want the Last Season to be a part of the logit, but I need it for the time series viz

noLastTrim <- noLast # just a table with the predictors from the final model

noLastTrim$G <- NULL
noLastTrim$HR <- NULL
noLastTrim$SB <- NULL
noLastTrim$SO <- NULL
noLastTrim$seasons <- NULL
noLastTrim$BA <- NULL
noLastTrim$BABIP <- NULL