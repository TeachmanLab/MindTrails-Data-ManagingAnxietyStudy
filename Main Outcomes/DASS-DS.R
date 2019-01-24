rm(list=ls())
finalIdLst  <- readRDS("/Users/soniabaee/Documents/Projects/MindTrails/R34/mindtrails_6_13_2018/mindtrails_6_13_2018/intersectIds.rds")
#------------------------------------------------------
## reading DASSDS raw data -----
DASSDS <- read.csv("/Users/soniabaee/Documents/Projects/MindTrails/R34/mindtrails_6_13_2018/mindtrails_6_13_2018/DASS21_DS_recovered_6_13_2018_edited.csv") 
DASSDS <- DASSDS[!(duplicated(DASSDS[, c("participantRSA", "session")])),]
DASSDS = DASSDS %>% filter(participantRSA %in% finalIdLst)
slcSession = c("PRE","SESSION3", "SESSION6")
DASSDS = DASSDS %>% filter(session %in% slcSession)
#------------------------------------------------------
## selecting rows that prefer not to answer ----
lenCnt <- length(apply(DASSDS, 1, function(r) any(r %in% c('-1'))))
rowNumber <- list()
cnt <- 1
for(i in 1:lenCnt){
  if(apply(DASSDS, 1, function(r) any(r %in% c('-1')))[i]){
    rowNumber[cnt] <- i
    cnt <- cnt + 1
  }
}
rowNumber <- array(as.numeric(unlist(rowNumber)))

DASSDSPreferNotAnswer <- DASSDS[rowNumber,]
DASSDS <- DASSDS[-rowNumber, ]

#------------------------------------------------------
### calculate OASIS score for those that contain prefer not to answer ----
DASSDSPreferNotAnswer <- DASSDSPreferNotAnswer[,c(1,3,4,6,7,8,9,10,11)]
pnaIds <- unique(DASSDSPreferNotAnswer$participantRSA)

DASSDSnp <- data.frame(matrix(NA, nrow = length(pnaIds)*3, ncol = 3))
colnames(DASSDSnp) <- c('Id','session','DassDS')

cnt <- 1
for(id in pnaIds){
  sessions <- DASSDSPreferNotAnswer[which(DASSDSPreferNotAnswer$participantRSA == id),]$session
  for(session in sessions){
    DASSDSnp$Id[cnt] <- id
    DASSDSnp$session[cnt] <- session 
    if(dim(DASSDSPreferNotAnswer[which(DASSDSPreferNotAnswer$participantRSA == id & DASSDSPreferNotAnswer$session == session),])[1] != 0){
      ngValue <- which(DASSDSPreferNotAnswer[which(DASSDSPreferNotAnswer$participantRSA == id & DASSDSPreferNotAnswer$session == session),] == -1)
      if(length(ngValue) != 0){
        tmp1 <- DASSDSPreferNotAnswer[which(DASSDSPreferNotAnswer$participantRSA == id & DASSDSPreferNotAnswer$session == session),][,-ngValue]
        tmp1 <- tmp1[,-which(names(tmp1) == 'session' | names(tmp1) == 'participantRSA')]
        if(length(tmp1) != 0){
          if(length(tmp1) > 1)
            DASSDSnp$DassDS[cnt] <- 2*sum(tmp1)
          else
            DASSDSnp$DassDS[cnt] <- tmp1
        }
        else
          DASSDSnp$DassDS[cnt] <- NA
      }
      else
        DASSDSnp$DassDS[cnt] <- 2*sum(DASSDSPreferNotAnswer$blue[DASSDSPreferNotAnswer$participantRSA == id & DASSDSPreferNotAnswer$session == session],DASSDSPreferNotAnswer$difficult[DASSDSPreferNotAnswer$participantRSA == id & DASSDSPreferNotAnswer$session == session],DASSDSPreferNotAnswer$hopeless[DASSDSPreferNotAnswer$participantRSA == id & DASSDSPreferNotAnswer$session == session],DASSDSPreferNotAnswer$meaningless[DASSDSPreferNotAnswer$participantRSA == id & DASSDSPreferNotAnswer$session == session],DASSDSPreferNotAnswer$noenthusiastic[DASSDSPreferNotAnswer$participantRSA == id & DASSDSPreferNotAnswer$session == session],DASSDSPreferNotAnswer$nopositive[DASSDSPreferNotAnswer$participantRSA == id & DASSDSPreferNotAnswer$session == session],DASSDSPreferNotAnswer$noworth[DASSDSPreferNotAnswer$participantRSA == id & DASSDSPreferNotAnswer$session == session])
      
    }
    else
      DASSDSnp$DassDS[cnt] < NA
    
    
    
    cnt <- cnt + 1
  }
}

DASSDSnp = DASSDSnp[rowSums(is.na(DASSDSnp)) != ncol(DASSDSnp),]

#------------------------------------------------------
### calculate OASIS score for those that answer all the questions ----
aDASSDSIds = unique(DASSDS$participantRSA)

DASSDSNew <- data.frame(matrix(NA, nrow = length(aDASSDSIds)*3, ncol = 3))
colnames(DASSDSNew) <- c('Id','session','DassDS')
cnt <- 1
for(id in finalIdLst){
  sessions <- DASSDS$session[DASSDS$participantRSA == id]
  for(session in sessions){
    DASSDSNew$Id[cnt] <- id
    DASSDSNew$session[cnt] <- session
    DASSDSNew$DassDS[cnt] <- 2*sum(DASSDS$blue[DASSDS$participantRSA == id & DASSDS$session == session],DASSDS$difficult[DASSDS$participantRSA == id & DASSDS$session == session],DASSDS$hopeless[DASSDS$participantRSA == id & DASSDS$session == session],DASSDS$meaningless[DASSDS$participantRSA == id & DASSDS$session == session],DASSDS$noenthusiastic[DASSDS$participantRSA == id & DASSDS$session == session],DASSDS$nopositive[DASSDS$participantRSA == id & DASSDS$session == session],DASSDS$noworth[DASSDS$participantRSA == id & DASSDS$session == session])
    cnt <- cnt + 1
  }
}


DASSDSNew <- DASSDSNew[complete.cases(DASSDSNew),]

#------------------------------------------------------
## merging OASIS with prefer not to answer OASIS ----
newDASS <- rbind(DASSDSNew,DASSDSnp)
DASSDSIds = unique(newDASS$Id)
#------------------------------------------------------
participant <- read.csv("/Users/soniabaee/Documents/Projects/MindTrails/R34/mindtrails_6_13_2018/mindtrails_6_13_2018/ParticipantExportDAO_recovered_Jun_13_2018.csv")
curParticipant = participant %>% filter(id %in% DASSDSIds)

newDASS$cbmCondition = NA
newDASS$primeCondition = NA

pCbmCond = curParticipant %>% filter(cbmCondition == "POSITIVE")
newDASS[which(newDASS$Id %in% unique(pCbmCond$id)),]$cbmCondition = "POSITIVE"
pAnxiety = pCbmCond %>% filter(prime == "ANXIETY")
newDASS[which(newDASS$Id %in% unique(pAnxiety$id)),]$primeCondition = "ANXIETY"
pNeutral = pCbmCond %>% filter(prime == "NEUTRAL")
newDASS[which(newDASS$Id %in% unique(pNeutral$id)),]$primeCondition = "NEUTRAL"

ffCbmCond = curParticipant %>% filter(cbmCondition == "FIFTY_FIFTY")
newDASS[which(newDASS$Id %in% unique(ffCbmCond$id)),]$cbmCondition = "FIFTY_FIFTY"
fAnxiety = ffCbmCond %>% filter(prime == "ANXIETY")
newDASS[which(newDASS$Id %in% unique(fAnxiety$id)),]$primeCondition = "ANXIETY"
fNeutral = ffCbmCond %>% filter(prime == "NEUTRAL")
newDASS[which(newDASS$Id %in% unique(fNeutral$id)),]$primeCondition = "NEUTRAL"


nCbmCond = curParticipant %>% filter(cbmCondition == "NEUTRAL")
newDASS[which(newDASS$Id %in% unique(nCbmCond$id)),]$cbmCondition = "No Scenario"
nAnxiety = nCbmCond %>% filter(prime == "ANXIETY")
newDASS[which(newDASS$Id %in% unique(nAnxiety$id)),]$primeCondition = "ANXIETY"
nNeutral = nCbmCond %>% filter(prime == "NEUTRAL")
newDASS[which(newDASS$Id %in% unique(nNeutral$id)),]$primeCondition = "ANXIETY"


newDASS$condition = NA
newDASS[which(newDASS$cbmCondition == "FIFTY_FIFTY" & newDASS$primeCondition == "NEUTRAL"),]$condition = "FIFTY_FIFTYNEUTRAL"
newDASS[which(newDASS$cbmCondition == "FIFTY_FIFTY" & newDASS$primeCondition == "ANXIETY"),]$condition = "FIFTY_FIFTYANXIETY"
newDASS[which(newDASS$cbmCondition == "POSITIVE" & newDASS$primeCondition == "NEUTRAL"),]$condition = "POSITIVENEUTRAL"
newDASS[which(newDASS$cbmCondition == "POSITIVE" & newDASS$primeCondition == "ANXIETY"),]$condition = "POSITIVEANXIETY"
newDASS[which(newDASS$cbmCondition == "No Scenario" & newDASS$primeCondition == "NEUTRAL"),]$condition = "NEUTRALNEUTRAL"
newDASS[which(newDASS$cbmCondition == "No Scenario" & newDASS$primeCondition == "ANXIETY"),]$condition = "NONEANXIETY"

newDASS$condition <- as.factor(newDASS$condition)
newDASS$primeCondition <- as.factor(newDASS$primeCondition, levels = c("NEUTRAL", "ANXIETY"))
newDASS$cbmCondition <- as.factor(newDASS$cbmCondition, levels = c("No Scenario", "FIFTY_FIFTY", "POSITIVE"))

#------------------------------------------------------
for(i in levels(newDASS$condition)){
  tempVarString <- gsub("&","",i,fixed=TRUE)
  tempVar <- ifelse(newDASS$condition==i,1,0)
  newDASS <- cbind(newDASS,tempVar)
  colnames(newDASS)[length(newDASS)] <- tempVarString
}

for(i in levels(newDASS$cbmCondition)){
  tempVarString <- i
  tempVar <- ifelse(newDASS$cbmCondition==i,1,0)
  newDASS <- cbind(newDASS,tempVar)
  colnames(newDASS)[length(newDASS)] <- tempVarString
}


for(i in levels(newDASS$primeCondition)){
  tempVarString <- i
  tempVar <- ifelse(newDASS$primeCondition==i,1,0)
  newDASS <- cbind(newDASS,tempVar)
  colnames(newDASS)[length(newDASS)] <- tempVarString
}

slcScoreClm = c(grep("^DassDS", colnames(newDASS)))
for(i in slcScoreClm){
  newDASS <- cbind(newDASS,ifelse(!is.na(newDASS[,i]),1,0))
  colnames(newDASS)[length(newDASS)] <- paste(colnames(newDASS)[i],"Dropout",sep="")
}

slcScoreDropClm = c(grep("^DassDS", colnames(newDASS)))
DASSDSLGCWide <- (reshape(data=newDASS,
                         idvar="Id",
                         timevar="session",
                         direction="wide",
                         v.names=colnames(newDASS)[slcScoreDropClm]))

colnames(DASSDSLGCWide) <- gsub(".","_",colnames(DASSDSLGCWide),fixed=TRUE)

colnames(DASSDSLGCWide)
View(DASSDSLGCWide)

#------------------------------------------------------
write.csv(DASSDSLGCWide, "/Users/soniabaee/Documents/Projects/MindTrails/R34/consortDiagram/Data/22JanDASS_DS.csv")


