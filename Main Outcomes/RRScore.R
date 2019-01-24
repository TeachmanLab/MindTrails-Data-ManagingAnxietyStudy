rm(list=ls())
finalIdLst  <- readRDS("/Users/soniabaee/Documents/Projects/MindTrails/R34/mindtrails_6_13_2018/mindtrails_6_13_2018/intersectIds.rds")
#------------------------------------------------------
### RR rating ----
RR = read.csv("/Users/soniabaee/Documents/Projects/MindTrails/R34/mindtrails_6_13_2018/mindtrails_6_13_2018/RR_recovered_Jun_13_2018.csv") 
RR = RR[!(duplicated(RR[, c("participantRSA", "session")])),]
RR = RR %>% filter(participantRSA %in% finalIdLst)
slcSession = c("PRE","SESSION3", "SESSION6")
RR = RR %>% filter(session %in% slcSession)
#------------------------------------------------------
## separate those that answer with those that didn't answer ---- 
lenCnt <- length(apply(RR, 1, function(r) any(r %in% c('-1'))))
rowNumber <- list()
cnt <- 1
for(i in 1:lenCnt){
  if(apply(RR, 1, function(r) any(r %in% c(-1)))[i]){
    rowNumber[cnt] <- i
    cnt <- cnt + 1
  }
}
rowNumber <- array(as.numeric(unlist(rowNumber)))
pnAnswer <- RR[rowNumber,]
RR <- RR[-rowNumber,]

#------------------------------------------------------
## separate different categories of RR ----
colnamesNF <- grepl(pattern = "NF$", colnames(RR))
RRatingNF <- cbind(RR[colnamesNF == TRUE],RR$session, RR$participantRSA)
colnames(RRatingNF)[11] <- "ID"
colnames(RRatingNF)[10] <- "session"


colnamesPF <- grepl(pattern = "PF$", colnames(RR))
RRatingPF <- cbind(RR[colnamesPF == TRUE],RR$session, RR$participantRSA)
colnames(RRatingPF)[11] <- "ID"
colnames(RRatingPF)[10] <- "session"


colNamesNS <- grepl(pattern = "NS$", colnames(RR))
RRatingNS <- cbind(RR[colNamesNS == TRUE],RR$session, RR$participantRSA)
colnames(RRatingNS)[11] <- "ID"
colnames(RRatingNS)[10] <- "session"

colNamesPS <- grepl(pattern = "PS$", colnames(RR))
RRatingPS <- cbind(RR[colNamesPS == TRUE],RR$session, RR$participantRSA)
colnames(RRatingPS)[11] <- "ID"
colnames(RRatingPS)[10] <- "session"
#------------------------------------------------------
## calculate RR score with those answer all the questions ---- 
idRR <- unique(RR$participantRSA)
RR1 <- data.frame(matrix(NA, nrow = length(idRR)*length(slcSession), ncol = 6))
colnames(RR1) <- c("Id", "session", "RRNegativeFoil", "RRPositiveFoil", "RRNegative", "RRPositive")
cnt <- 1
for(id in idRR){
  sessions <- unique(RR[which(RR$participantRSA == id),]$session)
  for(session in sessions){
    RR1$Id[cnt] <- id
    RR1$session[cnt] <- session 
    if(dim(RRatingNF[which(RRatingNF$ID == id & RRatingNF$session == session),])[1] != 0){
      RR1$RRNegativeFoil[cnt] <- rowMeans(RRatingNF[which(RRatingNF$ID == id & RRatingNF$session == session),][,-which(names(RRatingNF) == 'session' | names(RRatingNF) == 'ID')])
    }
    else
      RR1$RRNegativeFoil[cnt] <- NA
    
    if(dim(RRatingPS[which(RRatingPS$ID == id & RRatingPS$session == session),])[1] != 0){
      RR1$RRPositive[cnt] <- rowMeans(RRatingPS[which(RRatingPS$ID == id & RRatingPS$session == session),][,-which(names(RRatingPS) == 'session' | names(RRatingPS) == 'ID')])
    }
    else
      RR1$RRPositive[cnt] <- NA
    
    if(dim(RRatingNS[which(RRatingNS$ID == id & RRatingNS$session == session),])[1] != 0){
      RR1$RRNegative[cnt] <- rowMeans(RRatingNS[which(RRatingNS$ID == id & RRatingNS$session == session),][,-which(names(RRatingNS) == 'session' | names(RRatingNS) == 'ID')])
    }
    else
      RR1$RRNegative[cnt] <- NA
    
    if(dim(RRatingPF[which(RRatingPF$ID == id & RRatingPF$session == session),])[1] != 0){
      RR1$RRPositiveFoil[cnt] <- rowMeans(RRatingPF[which(RRatingPF$ID == id & RRatingPF$session == session),][,-which(names(RRatingPF) == 'session' | names(RRatingPF) == 'ID')])
    }
    else
      RR1$RRPositiveFoil[cnt] <- NA
    
    cnt <- cnt + 1
  }
}

RR1 <- RR1[rowSums(is.na(RR1)) != ncol(RR1),]

#------------------------------------------------------
### calculate RR Score for prefer not to answer
RRPreferNotAnswerRR <- pnAnswer

colnamesNF <- grepl(pattern = "NF$", colnames(RRPreferNotAnswerRR))
RRatingNF <- cbind(RRPreferNotAnswerRR[colnamesNF == TRUE],RRPreferNotAnswerRR$session, RRPreferNotAnswerRR$participantRSA)
colnames(RRatingNF)[11] <- "ID"
colnames(RRatingNF)[10] <- "session"

colnamesPF <- grepl(pattern = "PF$", colnames(RRPreferNotAnswerRR))
RRatingPF <- cbind(RRPreferNotAnswerRR[colnamesPF == TRUE],RRPreferNotAnswerRR$session, RRPreferNotAnswerRR$participantRSA)
colnames(RRatingPF)[11] <- "ID"
colnames(RRatingPF)[10] <- "session"


colNamesNS <- grepl(pattern = "NS$", colnames(RRPreferNotAnswerRR))
RRatingNS <- cbind(RRPreferNotAnswerRR[colNamesNS == TRUE],RRPreferNotAnswerRR$session, RRPreferNotAnswerRR$participantRSA)
colnames(RRatingNS)[11] <- "ID"
colnames(RRatingNS)[10] <- "session"



colNamesPS <- grepl(pattern = "PS$", colnames(RRPreferNotAnswerRR))
RRatingPS <- cbind(RRPreferNotAnswerRR[colNamesPS == TRUE],RRPreferNotAnswerRR$session, RRPreferNotAnswerRR$participantRSA)
colnames(RRatingPS)[11] <- "ID"
colnames(RRatingPS)[10] <- "session"


RR1npIds = unique(RRPreferNotAnswerRR$participantRSA)

RR1np <- data.frame(matrix(NA, nrow = length(RR1npIds)*length(slcSession), ncol = 6))
colnames(RR1np) <- c("Id", "session", "RRNegativeFoil", "RRPositiveFoil", "RRNegative", "RRPositive")

cnt <- 1
for(id in RR1npIds){
  sessions <- unique(RRPreferNotAnswerRR[which(RRPreferNotAnswerRR$participantRSA == id),]$session)
  for(session in sessions){
    RR1np$Id[cnt] <- id
    RR1np$session[cnt] <- session 
    
    if(dim(RRatingNF[which(RRatingNF$ID == id & RRatingNF$session == session),])[1] != 0){
      ngValue <- which(RRatingNF[which(RRatingNF$ID == id & RRatingNF$session == session),] == -1)
      if(length(ngValue) != 0){
        tmp1 <- RRatingNF[which(RRatingNF$ID == id & RRatingNF$session == session),][,-ngValue]
        tmp1 <- tmp1[,-which(names(tmp1) == 'session' | names(tmp1) == 'ID')]
        if(length(tmp1) != 0){
          if(length(tmp1) > 1)
            RR1np$RRNegativeFoil[cnt] <- rowMeans(tmp1,length(ngValue)*mean(as.numeric(tmp1)))
          else
            RR1np$RRNegativeFoil[cnt] <- mean(tmp1,length(ngValue)* tmp1)
        }
        else
          RR1np$RRNegativeFoil[cnt] <- NA
      }
      else
        RR1np$RRNegativeFoil[cnt] <- rowMeans(RRatingNF[which(RRatingNF$ID == id & RRatingNF$session == session),][,-which(names(RRatingNF) == 'session' | names(RRatingNF) == 'ID')])  
    }
    else
      RR1np$RRNegativeFoil[cnt] < NA
    
    if(dim(RRatingPS[which(RRatingPS$ID == id & RRatingPS$session == session),])[1] != 0){
      ngValue <- which(RRatingPS[which(RRatingPS$ID == id & RRatingPS$session == session),] == -1)
      if(length(ngValue) != 0){
        tmp2 <- RRatingPS[which(RRatingPS$ID == id & RRatingPS$session == session),][,-ngValue]
        tmp2 <- tmp2[,-which(names(tmp2) == 'session' | names(tmp2) == 'ID')]
        if(length(tmp2) != 0){
          if(length(tmp2) > 1)
            RR1np$RRPositive[cnt] <- rowMeans(tmp2,length(ngValue)*mean(as.numeric(tmp2)))
          else
            RR1np$RRPositive[cnt] <- mean(tmp2,length(ngValue)* tmp2)
        }
        
        else
          RR1np$RRPositive[cnt] <- NA
      }
      else
        RR1np$RRPositive[cnt] <- rowMeans(RRatingPS[which(RRatingPS$ID == id & RRatingPS$session == session),][,-which(names(RRatingPS) == 'session' | names(RRatingPS) == 'ID')])  
    }
    else
      RR1np$RRPositive[cnt] <- NA
    
    if(dim(RRatingNS[which(RRatingNS$ID == id & RRatingNS$session == session),])[1] != 0){
      ngValue <- which(RRatingNS[which(RRatingNS$ID == id & RRatingNS$session == session),] == -1)
      if(length(ngValue) != 0){
        tmp3 <- RRatingNS[which(RRatingNS$ID == id & RRatingNS$session == session),][,-ngValue]
        tmp3 <- tmp3[,-which(names(tmp3) == 'session' | names(tmp3) == 'ID')]
        if(length(tmp3) != 0){
          if(length(tmp3) > 1)
            RR1np$RRNegative[cnt] <- rowMeans(tmp3,length(ngValue)*mean(as.numeric(tmp3)))
          else
            RR1np$RRNegative[cnt] <- mean(tmp3,length(ngValue)* tmp3)
        }
        else
          RR1np$RRNegative[cnt] <- NA
      }else
        RR1np$RRNegative[cnt] <- rowMeans(RRatingNS[which(RRatingNS$ID == id & RRatingNS$session == session),][,-which(names(RRatingNS) == 'session' | names(RRatingNS) == 'ID')])  
    }
    else
      RR1np$RRNegative[cnt] <- NA
    
    if(dim(RRatingPF[which(RRatingPF$ID == id & RRatingPF$session == session),])[1] != 0){
      ngValue <- which(RRatingPF[which(RRatingPF$ID == id & RRatingPF$session == session),] == -1)
      if(length(ngValue) != 0){
        tmp4 <- RRatingPF[which(RRatingPF$ID == id & RRatingPF$session == session),][,-ngValue]
        tmp4 <- tmp4[,-which(names(tmp4) == 'session' | names(tmp4) == 'ID')]
        if(length(tmp4) != 0){
          if(length(tmp4) > 1)
            RR1np$RRPositiveFoil[cnt] <- rowMeans(tmp4,length(ngValue)*mean(as.numeric(tmp4)))
          else
            RR1np$RRPositiveFoil[cnt] <- mean(tmp4,length(ngValue)* tmp4)
        }
        else
          RR1np$RRPositiveFoil[cnt] <- NA
      }
      else
        RR1np$RRPositiveFoil[cnt] <- rowMeans(RRatingPF[which(RRatingPF$ID == id & RRatingPF$session == session),][,-which(names(RRatingPF) == 'session' | names(RRatingPF) == 'ID')])  
    }
    else
      RR1np$RRPositiveFoil[cnt] <- NA
    
    cnt <- cnt + 1
  }
}


RR1np <- RR1np[rowSums(is.na(RR1np)) != ncol(RR1np),]
#------------------------------------------------------
RRProcessed <- rbind(RR1,RR1np)
rrIds = unique(RRProcessed$Id)
#------------------------------------------------------
participant <- read.csv("/Users/soniabaee/Documents/Projects/MindTrails/R34/mindtrails_6_13_2018/mindtrails_6_13_2018/ParticipantExportDAO_recovered_Jun_13_2018.csv")
curParticipant = participant %>% filter(id %in% rrIds)

RRProcessed$cbmCondition = NA
RRProcessed$primeCondition = NA

pCbmCond = curParticipant %>% filter(cbmCondition == "POSITIVE")
RRProcessed[which(RRProcessed$Id %in% unique(pCbmCond$id)),]$cbmCondition = "POSITIVE"
pAnxiety = pCbmCond %>% filter(prime == "ANXIETY")
RRProcessed[which(RRProcessed$Id %in% unique(pAnxiety$id)),]$primeCondition = "ANXIETY"
pNeutral = pCbmCond %>% filter(prime == "NEUTRAL")
RRProcessed[which(RRProcessed$Id %in% unique(pNeutral$id)),]$primeCondition = "NEUTRAL"

ffCbmCond = curParticipant %>% filter(cbmCondition == "FIFTY_FIFTY")
RRProcessed[which(RRProcessed$Id %in% unique(ffCbmCond$id)),]$cbmCondition = "FIFTY_FIFTY"
fAnxiety = ffCbmCond %>% filter(prime == "ANXIETY")
RRProcessed[which(RRProcessed$Id %in% unique(fAnxiety$id)),]$primeCondition = "ANXIETY"
fNeutral = ffCbmCond %>% filter(prime == "NEUTRAL")
RRProcessed[which(RRProcessed$Id %in% unique(fNeutral$id)),]$primeCondition = "NEUTRAL"


nCbmCond = curParticipant %>% filter(cbmCondition == "NEUTRAL")
RRProcessed[which(RRProcessed$Id %in% unique(nCbmCond$id)),]$cbmCondition = "No Scenario"
nAnxiety = nCbmCond %>% filter(prime == "ANXIETY")
RRProcessed[which(RRProcessed$Id %in% unique(nAnxiety$id)),]$primeCondition = "ANXIETY"
nNeutral = nCbmCond %>% filter(prime == "NEUTRAL")
RRProcessed[which(RRProcessed$Id %in% unique(nNeutral$id)),]$primeCondition = "ANXIETY"


RRProcessed$condition = NA
RRProcessed[which(RRProcessed$cbmCondition == "FIFTY_FIFTY" & RRProcessed$primeCondition == "NEUTRAL"),]$condition = "FIFTY_FIFTYNEUTRAL"
RRProcessed[which(RRProcessed$cbmCondition == "FIFTY_FIFTY" & RRProcessed$primeCondition == "ANXIETY"),]$condition = "FIFTY_FIFTYANXIETY"
RRProcessed[which(RRProcessed$cbmCondition == "POSITIVE" & RRProcessed$primeCondition == "NEUTRAL"),]$condition = "POSITIVENEUTRAL"
RRProcessed[which(RRProcessed$cbmCondition == "POSITIVE" & RRProcessed$primeCondition == "ANXIETY"),]$condition = "POSITIVEANXIETY"
RRProcessed[which(RRProcessed$cbmCondition == "No Scenario" & RRProcessed$primeCondition == "NEUTRAL"),]$condition = "NEUTRALNEUTRAL"
RRProcessed[which(RRProcessed$cbmCondition == "No Scenario" & RRProcessed$primeCondition == "ANXIETY"),]$condition = "NONEANXIETY"

RRProcessed$condition = as.factor(RRProcessed$condition)
RRProcessed$primeCondition = as.factor(RRProcessed$primeCondition, levels = c("NEUTRAL", "ANXIETY"))
RRProcessed$cbmCondition = as.factor(RRProcessed$cbmCondition, levels = c("No Scenario", "FIFTY_FIFTY", "POSITIVE"))

#------------------------------------------------------
for(i in levels(RRProcessed$condition)){
  tempVarString <- gsub("&","",i,fixed=TRUE)
  tempVar <- ifelse(RRProcessed$condition==i,1,0)
  RRProcessed <- cbind(RRProcessed,tempVar)
  colnames(RRProcessed)[length(RRProcessed)] <- tempVarString
}

for(i in levels(RRProcessed$cbmCondition)){
  tempVarString <- i
  tempVar <- ifelse(RRProcessed$cbmCondition==i,1,0)
  RRProcessed <- cbind(RRProcessed,tempVar)
  colnames(RRProcessed)[length(RRProcessed)] <- tempVarString
}

for(i in levels(RRProcessed$primeCondition)){
  tempVarString <- i
  tempVar <- ifelse(RRProcessed$primeCondition==i,1,0)
  RRProcessed <- cbind(RRProcessed,tempVar)
  colnames(RRProcessed)[length(RRProcessed)] <- tempVarString
}

scoreClm = c(grep("^RRNegative", colnames(RRProcessed)), grep("^RRPositive", colnames(RRProcessed)))
for(i in scoreClm){
  RRProcessed <- cbind(RRProcessed,ifelse(!is.na(RRProcessed[,i]),1,0))
  colnames(RRProcessed)[length(RRProcessed)] <- paste(colnames(RRProcessed)[i],"Dropout",sep="")
}

scoreDropClm = c(grep("^RRNegative", colnames(RRProcessed)), grep("^RRPositive", colnames(RRProcessed)))

RRProcessedLGCWide <- (reshape(data=RRProcessed,
                               idvar="Id",
                               timevar="session",
                               direction="wide",
                               v.names=colnames(RRProcessed)[scoreDropClm]))
colnames(RRProcessedLGCWide) <- gsub(".","_",colnames(RRProcessedLGCWide),fixed=TRUE)

View(RRProcessedLGCWide)
#------------------------------------------------------
write.csv(RRProcessedLGCWide, "/Users/soniabaee/Documents/Projects/MindTrails/R34/consortDiagram/Data/22JanRR.csv")
