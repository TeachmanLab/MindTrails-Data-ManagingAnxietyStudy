rm(list=ls())
finalIdLst  <- readRDS("/Users/soniabaee/Documents/Projects/MindTrails/R34/mindtrails_6_13_2018/mindtrails_6_13_2018/intersectIds.rds")
#------------------------------------------------------
### BBSIQ -------
BBSIQ <- read.csv("/Users/soniabaee/Documents/Projects/MindTrails/R34/mindtrails_6_13_2018/mindtrails_6_13_2018/BBSIQ_recovered_Jun_13_2018.csv") 
BBSIQ <- BBSIQ[!(duplicated(BBSIQ[, c("participantRSA", "session")])),]
BBSIQ = BBSIQ %>% filter(participantRSA %in% finalIdLst)
slcSession = c("PRE","SESSION3", "SESSION6")
BBSIQ = BBSIQ %>% filter(session %in% slcSession)

#------------------------------------------------------
## separate those answer with those that they didn't answer
lenCnt <- length(apply(BBSIQ, 1, function(r) any(r %in% c(555))))
rowNumber1 <- list()
cnt <- 1
for(i in 1:lenCnt){
  if(apply(BBSIQ, 1, function(r) any(r %in% c('555')))[i]){
    rowNumber1[cnt] <- i
    cnt <- cnt + 1
  }
}
rowNumber1 <- array(as.numeric(unlist(rowNumber1)))
#------------------------------------------------------

pnBBSIQ <- BBSIQ[-rowNumber1,]
BBSIQpreferNA <- BBSIQ[rowNumber1,]

#------------------------------------------------------
## calculating threat and physical ---- 
threat <- c("visitors_bored","shop_irritating","smoke_house","friend_incompetent","jolt_burglar","party_boring","urgent_died")
physical <- c("breath_suffocate","chest_heart","confused_outofmind","dizzy_ill","heart_wrong","lightheaded_faint","vision_illness")
#------------------------------------------------------
## calculate BBSIQ score for those answer ---- 
pnthreatData <- cbind(pnBBSIQ[,threat],pnBBSIQ$session, pnBBSIQ$participantRSA)
colnames(pnthreatData)[8] <- "session"
colnames(pnthreatData)[9] <- "ID"

pnphysicalData <- cbind(pnBBSIQ[,physical],pnBBSIQ$session, pnBBSIQ$participantRSA)
colnames(pnphysicalData)[8] <- "session"
colnames(pnphysicalData)[9] <- "ID"

pnBbsiqIds = unique(pnBBSIQ$participantRSA)
BBSIQpn <- data.frame(matrix(NA, nrow = length(pnBbsiqIds)*length(slcSession), ncol = 5))
colnames(BBSIQpn) <- c("Id", "session", "BBSIQThreat", "BBSIQPhysical","negativeBBSIQ")
cnt <- 1
for(id in pnBbsiqIds){
  sessions <- pnBBSIQ[which(pnBBSIQ$participantRSA == id),]$session
  for(session in sessions){
    BBSIQpn$Id[cnt] <- id
    BBSIQpn$session[cnt] <- session 
    if(dim(pnthreatData[which(pnthreatData$ID == id & pnthreatData$session == session),])[1] != 0){
      tValue <- which(pnthreatData[which(pnthreatData$ID == id & pnthreatData$session == session),] == 555)
      if(length(tValue) != 0){
        tmpt <- pnthreatData[which(pnthreatData$ID == id & pnthreatData$session == session),][,-tValue]
        tmpt <- tmpt[,-which(names(tmpt) == 'session' | names(tmpt) == 'ID')]
        if(length(tmpt) != 0){
          if(length(tmpt) > 1)
            BBSIQpn$BBSIQThreat[cnt] <- rowMeans(tmpt,length(ngValue)*mean(as.numeric(tmpt)))
          else
            BBSIQpn$BBSIQThreat[cnt] <- mean(tmpt,length(ngValue)* tmpt)
        }
        else
          BBSIQpn$BBSIQThreat[cnt] <- NA
      }
      else
        BBSIQpn$BBSIQThreat[cnt] <- rowMeans(pnthreatData[which(pnthreatData$ID == id & pnthreatData$session == session),][,-which(names(pnthreatData) == 'session' | names(pnthreatData) == 'ID')])  
    }
    else
      BBSIQpn$BBSIQThreat[cnt] <- NA
    
    if(dim(pnphysicalData[which(pnphysicalData$ID == id & pnphysicalData$session == session),])[1] != 0){
      tValue <- which(pnphysicalData[which(pnphysicalData$ID == id & pnphysicalData$session == session),] == 555)
      if(length(tValue) != 0){
        tmpt <- pnphysicalData[which(pnphysicalData$ID == id & pnphysicalData$session == session),][,-tValue]
        tmpt <- tmpt[,-which(names(tmpt) == 'session' | names(tmpt) == 'ID')]
        if(length(tmpt) != 0){
          if(length(tmpt) > 1)
            BBSIQpn$BBSIQPhysical[cnt] <- rowMeans(tmpt,length(ngValue)*mean(as.numeric(tmpt)))
          else
            BBSIQpn$BBSIQPhysical[cnt] <- mean(tmpt,length(ngValue)* tmpt)
        }
        else
          BBSIQpn$BBSIQPhysical[cnt] <- NA
      }
      else
        BBSIQpn$BBSIQPhysical[cnt] <- rowMeans(pnphysicalData[which(pnphysicalData$ID == id & pnphysicalData$session == session),][,-which(names(pnphysicalData) == 'session' | names(pnphysicalData) == 'ID')])  
    }
    else
      BBSIQpn$BBSIQPhysical[cnt] <- NA
    
    
    BBSIQpn$negativeBBSIQ[cnt] <- mean(c(BBSIQpn$BBSIQPhysical[cnt], BBSIQpn$BBSIQThreat[cnt]))
    
    cnt <- cnt + 1
  }
}

BBSIQpn <- BBSIQpn[rowSums(is.na(BBSIQpn)) != ncol(BBSIQpn),]
BBSIQScore <- BBSIQpn[,c("Id", "session", "negativeBBSIQ")]

#------------------------------------------------------
### calculate BBSIQ score for prefer not to answer -----
threatDataPN <- cbind(BBSIQpreferNA[,threat],BBSIQpreferNA$session, BBSIQpreferNA$participantRSA)
colnames(threatDataPN)[8] <- "session"
colnames(threatDataPN)[9] <- "ID"

physicalDataPN <- cbind(BBSIQpreferNA[,physical],BBSIQpreferNA$session, BBSIQpreferNA$participantRSA)
colnames(physicalDataPN)[8] <- "session"
colnames(physicalDataPN)[9] <- "ID"

idBBSIQpn = unique(BBSIQpreferNA$participantRSA)
BBSIQpn <- data.frame(matrix(NA, nrow = length(idBBSIQpn)*length(slcSession), ncol = 5))
colnames(BBSIQpn) <- c("Id", "session", "BBSIQThreat", "BBSIQPhysical","negativeBBSIQ")
cnt <- 1
for(id in idBBSIQpn){
  sessions <- BBSIQpreferNA[which(BBSIQpreferNA$participantRSA == id),]$session
  for(session in sessions){
    BBSIQpn$Id[cnt] <- id
    BBSIQpn$session[cnt] <- session 
    if(dim(threatDataPN[which(threatDataPN$ID == id & threatDataPN$session == session),])[1] != 0){
      tValue <- which(threatDataPN[which(threatDataPN$ID == id & threatDataPN$session == session),] == 555)
      if(length(tValue) != 0){
        tmpt <- threatDataPN[which(threatDataPN$ID == id & threatDataPN$session == session),][,-tValue]
        tmpt <- tmpt[,-which(names(tmpt) == 'session' | names(tmpt) == 'ID')]
        if(length(tmpt) != 0){
          if(length(tmpt) > 1)
            BBSIQpn$BBSIQThreat[cnt] <- rowMeans(tmpt,length(tValue)*mean(as.numeric(tmpt)))
          else
            BBSIQpn$BBSIQThreat[cnt] <- mean(tmpt,length(tValue)* tmpt)
        }
        else
          BBSIQpn$BBSIQThreat[cnt] <- NA
      }
      else
        BBSIQpn$BBSIQThreat[cnt] <- rowMeans(threatDataPN[which(threatDataPN$ID == id & threatDataPN$session == session),][,-which(names(threatDataPN) == 'session' | names(threatDataPN) == 'ID')])  
    }
    else
      BBSIQpn$BBSIQThreat[cnt] <- NA
    
    if(dim(physicalDataPN[which(physicalDataPN$ID == id & physicalDataPN$session == session),])[1] != 0){
      tValue <- which(physicalDataPN[which(physicalDataPN$ID == id & physicalDataPN$session == session),] == 555)
      if(length(tValue) != 0){
        tmpt <- physicalDataPN[which(physicalDataPN$ID == id & physicalDataPN$session == session),][,-tValue]
        tmpt <- tmpt[,-which(names(tmpt) == 'session' | names(tmpt) == 'ID')]
        if(length(tmpt) != 0){
          if(length(tmpt) > 1)
            BBSIQpn$BBSIQPhysical[cnt] <- rowMeans(tmpt,length(tValue)*mean(as.numeric(tmpt)))
          else
            BBSIQpn$BBSIQPhysical[cnt] <- mean(tmpt,length(tValue)* tmpt)
        }
        else
          BBSIQpn$BBSIQPhysical[cnt] <- NA
      }
      else
        BBSIQpn$BBSIQPhysical[cnt] <- rowMeans(physicalDataPN[which(physicalDataPN$ID == id & physicalDataPN$session == session),][,-which(names(physicalDataPN) == 'session' | names(physicalDataPN) == 'ID')])  
    }
    else
      BBSIQpn$BBSIQPhysical[cnt] <- NA
    
    
    BBSIQpn$negativeBBSIQ[cnt] <- mean(c(BBSIQpn$BBSIQPhysical[cnt], BBSIQpn$BBSIQThreat[cnt]))
    
    cnt <- cnt + 1
  }
}

BBSIQpn <- BBSIQpn[rowSums(is.na(BBSIQpn)) != ncol(BBSIQpn),]
BBSIQScorepn <- BBSIQpn[,c("Id", "session", "negativeBBSIQ")]
#------------------------------------------------------
## merge BBSIQ + prefer not to answer
BBSIQProcessed <- rbind(BBSIQScorepn,BBSIQScore)

bbsiqIds = unique(BBSIQProcessed$Id)
#------------------------------------------------------
participant <- read.csv("/Users/soniabaee/Documents/Projects/MindTrails/R34/mindtrails_6_13_2018/mindtrails_6_13_2018/ParticipantExportDAO_recovered_Jun_13_2018.csv")
curParticipant = participant %>% filter(id %in% bbsiqIds)

BBSIQProcessed$cbmCondition = NA
BBSIQProcessed$primeCondition = NA

pCbmCond = curParticipant %>% filter(cbmCondition == "POSITIVE")
BBSIQProcessed[which(BBSIQProcessed$Id %in% unique(pCbmCond$id)),]$cbmCondition = "POSITIVE"
pAnxiety = pCbmCond %>% filter(prime == "ANXIETY")
BBSIQProcessed[which(BBSIQProcessed$Id %in% unique(pAnxiety$id)),]$primeCondition = "ANXIETY"
pNeutral = pCbmCond %>% filter(prime == "NEUTRAL")
BBSIQProcessed[which(BBSIQProcessed$Id %in% unique(pNeutral$id)),]$primeCondition = "NEUTRAL"

ffCbmCond = curParticipant %>% filter(cbmCondition == "FIFTY_FIFTY")
BBSIQProcessed[which(BBSIQProcessed$Id %in% unique(ffCbmCond$id)),]$cbmCondition = "FIFTY_FIFTY"
fAnxiety = ffCbmCond %>% filter(prime == "ANXIETY")
BBSIQProcessed[which(BBSIQProcessed$Id %in% unique(fAnxiety$id)),]$primeCondition = "ANXIETY"
fNeutral = ffCbmCond %>% filter(prime == "NEUTRAL")
BBSIQProcessed[which(BBSIQProcessed$Id %in% unique(fNeutral$id)),]$primeCondition = "NEUTRAL"


nCbmCond = curParticipant %>% filter(cbmCondition == "NEUTRAL")
BBSIQProcessed[which(BBSIQProcessed$Id %in% unique(nCbmCond$id)),]$cbmCondition = "No Scenario"
nAnxiety = nCbmCond %>% filter(prime == "ANXIETY")
BBSIQProcessed[which(BBSIQProcessed$Id %in% unique(nAnxiety$id)),]$primeCondition = "ANXIETY"
nNeutral = nCbmCond %>% filter(prime == "NEUTRAL")
BBSIQProcessed[which(BBSIQProcessed$Id %in% unique(nNeutral$id)),]$primeCondition = "ANXIETY"


BBSIQProcessed$condition = NA
BBSIQProcessed[which(BBSIQProcessed$cbmCondition == "FIFTY_FIFTY" & BBSIQProcessed$primeCondition == "NEUTRAL"),]$condition = "FIFTY_FIFTYNEUTRAL"
BBSIQProcessed[which(BBSIQProcessed$cbmCondition == "FIFTY_FIFTY" & BBSIQProcessed$primeCondition == "ANXIETY"),]$condition = "FIFTY_FIFTYANXIETY"
BBSIQProcessed[which(BBSIQProcessed$cbmCondition == "POSITIVE" & BBSIQProcessed$primeCondition == "NEUTRAL"),]$condition = "POSITIVENEUTRAL"
BBSIQProcessed[which(BBSIQProcessed$cbmCondition == "POSITIVE" & BBSIQProcessed$primeCondition == "ANXIETY"),]$condition = "POSITIVEANXIETY"
BBSIQProcessed[which(BBSIQProcessed$cbmCondition == "No Scenario" & BBSIQProcessed$primeCondition == "NEUTRAL"),]$condition = "NEUTRALNEUTRAL"
BBSIQProcessed[which(BBSIQProcessed$cbmCondition == "No Scenario" & BBSIQProcessed$primeCondition == "ANXIETY"),]$condition = "NONEANXIETY"

BBSIQProcessed$condition <- as.factor(BBSIQProcessed$condition)
BBSIQProcessed$primeCondition <- as.factor(BBSIQProcessed$primeCondition)
BBSIQProcessed$cbmCondition <- as.factor(BBSIQProcessed$cbmCondition)


#------------------------------------------------------

for(i in levels(BBSIQProcessed$condition)){
  tempVarString <- gsub("&","",i,fixed=TRUE)
  tempVar <- ifelse(BBSIQProcessed$condition==i,1,0)
  BBSIQProcessed <- cbind(BBSIQProcessed,tempVar)
  colnames(BBSIQProcessed)[length(BBSIQProcessed)] <- tempVarString
}


for(i in levels(BBSIQProcessed$cbmCondition)){
  tempVarString <- i
  tempVar <- ifelse(BBSIQProcessed$cbmCondition==i,1,0)
  BBSIQProcessed <- cbind(BBSIQProcessed,tempVar)
  colnames(BBSIQProcessed)[length(BBSIQProcessed)] <- tempVarString
}

for(i in levels(BBSIQProcessed$primeCondition)){
  tempVarString <- i
  tempVar <- ifelse(BBSIQProcessed$primeCondition==i,1,0)
  BBSIQProcessed <- cbind(BBSIQProcessed,tempVar)
  colnames(BBSIQProcessed)[length(BBSIQProcessed)] <- tempVarString
}


slcScoreClm = c(grep("^negativeBBSIQ", colnames(BBSIQProcessed)))
for(i in slcScoreClm){
  BBSIQProcessed <- cbind(BBSIQProcessed,ifelse(!is.na(BBSIQProcessed[,i]),1,0))
  colnames(BBSIQProcessed)[length(BBSIQProcessed)] <- paste(colnames(BBSIQProcessed)[i],"Dropout",sep="")
}


slcScoreDropClm = c(grep("^negativeBBSIQ", colnames(BBSIQProcessed)))
BBSIQLGCWide <- (reshape(data=BBSIQProcessed,
                         idvar="Id",
                         timevar="session",
                         direction="wide",
                         v.names=colnames(BBSIQProcessed)[slcScoreDropClm]))
colnames(BBSIQLGCWide)  <- gsub(".","_",colnames(BBSIQLGCWide),fixed=TRUE)
#------------------------------------------------------
write.csv(BBSIQLGCWide, "/Users/soniabaee/Documents/Projects/MindTrails/R34/consortDiagram/Data/22JanBBSIQ.csv")
