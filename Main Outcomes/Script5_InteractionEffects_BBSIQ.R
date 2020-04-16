# R34SEMModel.R
# Date: Feb 13, 2018 8:03:12 AM 2018
#
# Author: M. Joseph Meyer
###############################################################################
#
# Revision History
# Feb 13, 2018 8:03:12 AM 2018: R34SEMModel.R was created.
#
#
################################################################################
install.packages("OpenMx")
library(OpenMx)
require(OpenMx)

dir.name <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir.name)
dir()

BBSIQNeg_modelData <- read.csv("R34_FinalData_New.csv")
BBSIQNeg_modelData[] <- lapply(BBSIQNeg_modelData, as.numeric)

manifests<-c("negativeBBSIQ_PRE","negativeBBSIQ_SESSION3","negativeBBSIQ_SESSION6","FIFTY_FIFTYANXIETY","NONEANXIETY","POSITIVEANXIETY","POSITIVENEUTRAL","FIFTY_FIFTYNEUTRAL")
latents<-c("Intercept","Slope")

##Model where Ha = all 6 groups differ
BBSIQmodelAllDiff <- mxModel("R34_BBSIQ_ModelAllDiff", 
                        type="RAM",
                        manifestVars = manifests,
                        latentVars = latents,
                        mxPath(from="Intercept",to=c("negativeBBSIQ_PRE","negativeBBSIQ_SESSION3","negativeBBSIQ_SESSION6"), free=c(FALSE,FALSE,FALSE), value=c(1.0,1.0,1.0) , arrows=1, label=c("Intercept__negativeBBSIQ_PRE","Intercept__negativeBBSIQ_SESSION3","Intercept__negativeBBSIQ_SESSION6") ),
                        mxPath(from="Slope",to=c("negativeBBSIQ_PRE","negativeBBSIQ_SESSION3"), free=c(FALSE,FALSE), value=c(-2.0,-1.0) , arrows=1, label=c("Slope__negativeBBSIQ_PRE","Slope__negativeBBSIQ_SESSION3") ),
                        #mxPath(from="Dropout",to=c("Intercept"), free=c(TRUE), value=c(0.8) , arrows=1, label=c("Dropout__Intercept") ),
                        mxPath(from="one",to=c("Intercept","Slope"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("const__Intercept","const__Slope") ),
                        mxPath(from="FIFTY_FIFTYANXIETY",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("FIFTY_FIFTYANXIETY__Slope","FIFTY_FIFTYANXIETY__Intercept") ),
                        mxPath(from="NONEANXIETY",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("NONEANXIETY__Slope","NONEANXIETY__Intercept") ),
                        mxPath(from="POSITIVEANXIETY",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("POSITIVEANXIETY__Slope","POSITIVEANXIETY__Intercept") ),
                        mxPath(from="POSITIVENEUTRAL",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("POSITIVENEUTRAL__Slope","POSITIVENEUTRAL__Intercept") ),
                        mxPath(from="FIFTY_FIFTYNEUTRAL",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("FIFTY_FIFTYNEUTRAL__Slope","FIFTY_FIFTYNEUTRAL__Intercept") ),
                        mxPath(from="negativeBBSIQ_PRE",to=c("negativeBBSIQ_PRE"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_negativeBBSIQ_PRE") ),
                        mxPath(from="negativeBBSIQ_SESSION3",to=c("negativeBBSIQ_SESSION3"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_negativeBBSIQ_SESSION3") ),
                        mxPath(from="negativeBBSIQ_SESSION6",to=c("negativeBBSIQ_SESSION6"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_negativeBBSIQ_SESSION6") ),
                        mxPath(from="Intercept",to=c("Intercept"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_Intercept") ),
                        mxPath(from="Slope",to=c("Slope"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_Slope") ),
                        #mxPath(from="Dropout",to=c("Dropout"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_Dropout") ),
                        mxPath(from="FIFTY_FIFTYANXIETY",to=c("FIFTY_FIFTYANXIETY"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_FIFTY_FIFTYANXIETY") ),
                        mxPath(from="NONEANXIETY",to=c("NONEANXIETY"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_NONEANXIETY") ),
                        mxPath(from="POSITIVEANXIETY",to=c("POSITIVEANXIETY"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_POSITIVEANXIETY") ),
                        mxPath(from="POSITIVENEUTRAL",to=c("POSITIVENEUTRAL"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_POSITIVENEUTRAL") ),
                        mxPath(from="FIFTY_FIFTYNEUTRAL",to=c("FIFTY_FIFTYNEUTRAL"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_FIFTY_FIFTYNEUTRAL") ),
                        mxPath(from="one",to=c("negativeBBSIQ_PRE","negativeBBSIQ_SESSION3","negativeBBSIQ_SESSION6","FIFTY_FIFTYANXIETY","NONEANXIETY","POSITIVEANXIETY","POSITIVENEUTRAL","FIFTY_FIFTYNEUTRAL"), free=F, value=0, arrows=1),
                        mxData(BBSIQNeg_modelData, type = "raw")
);

resultBBSIQmodelAllDiff <- mxRun(BBSIQmodelAllDiff)
summary(resultBBSIQmodelAllDiff) #summary table

###### PART A ######: ACTIVE GROUPS VS. NO TRAINING CONTROL GROUPS

#### PosAnx = NoTrainNeut
BBSIQmodPosAnxSameNonNeut <-mxModel(BBSIQmodelAllDiff,
                   mxPath(from="POSITIVEANXIETY",to=c("Slope","Intercept"), free=c(FALSE,FALSE), 
                          value=c(0.0,0.0), arrows=1, label=c("POSITIVEANXIETY__Slope","POSITIVEANXIETY__Intercept") ))
resultPosAnxSameNoneNeut <- mxRun(BBSIQmodPosAnxSameNonNeut)
mxCompare(resultBBSIQmodelAllDiff, resultPosAnxSameNoneNeut)

#### PosAnx = NoTrainAnx
BBSIQmodelPosAnxNoneAnxSame <- mxModel("BBSIQmodelPosAnxNoneAnxSame", 
                                       type="RAM",
                                       manifestVars = manifests,
                                       latentVars = latents,
                                       mxPath(from="Intercept",to=c("negativeBBSIQ_PRE","negativeBBSIQ_SESSION3","negativeBBSIQ_SESSION6"), free=c(FALSE,FALSE,FALSE), value=c(1.0,1.0,1.0) , arrows=1, label=c("Intercept__negativeBBSIQ_PRE","Intercept__negativeBBSIQ_SESSION3","Intercept__negativeBBSIQ_SESSION6") ),
                                       mxPath(from="Slope",to=c("negativeBBSIQ_PRE","negativeBBSIQ_SESSION3"), free=c(FALSE,FALSE), value=c(-2.0,-1.0) , arrows=1, label=c("Slope__negativeBBSIQ_PRE","Slope__negativeBBSIQ_SESSION3") ),
                                       #mxPath(from="Dropout",to=c("Intercept"), free=c(TRUE), value=c(0.8) , arrows=1, label=c("Dropout__Intercept") ),
                                       mxPath(from="one",to=c("Intercept","Slope"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("const__Intercept","const__Slope") ),
                                       mxPath(from="FIFTY_FIFTYANXIETY",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("FIFTY_FIFTYANXIETY__Slope","FIFTY_FIFTYANXIETY__Intercept") ),
                                       mxPath(from="NONEANXIETY",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("SAME__Slope","SAME__Intercept") ),
                                       mxPath(from="POSITIVEANXIETY",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("SAME__Slope","SAME__Intercept") ),
                                       mxPath(from="POSITIVENEUTRAL",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("POSITIVENEUTRAL__Slope","POSITIVENEUTRAL__Intercept") ),
                                       mxPath(from="FIFTY_FIFTYNEUTRAL",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("FIFTY_FIFTYNEUTRAL__Slope","FIFTY_FIFTYNEUTRAL__Intercept") ),
                                       mxPath(from="negativeBBSIQ_PRE",to=c("negativeBBSIQ_PRE"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_negativeBBSIQ_PRE") ),
                                       mxPath(from="negativeBBSIQ_SESSION3",to=c("negativeBBSIQ_SESSION3"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_negativeBBSIQ_SESSION3") ),
                                       mxPath(from="negativeBBSIQ_SESSION6",to=c("negativeBBSIQ_SESSION6"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_negativeBBSIQ_SESSION6") ),
                                       mxPath(from="Intercept",to=c("Intercept"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_Intercept") ),
                                       mxPath(from="Slope",to=c("Slope"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_Slope") ),
                                       #mxPath(from="Dropout",to=c("Dropout"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_Dropout") ),
                                       mxPath(from="FIFTY_FIFTYANXIETY",to=c("FIFTY_FIFTYANXIETY"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_FIFTY_FIFTYANXIETY") ),
                                       mxPath(from="NONEANXIETY",to=c("NONEANXIETY"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_NONEANXIETY") ),
                                       mxPath(from="POSITIVEANXIETY",to=c("POSITIVEANXIETY"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_POSITIVEANXIETY") ),
                                       mxPath(from="POSITIVENEUTRAL",to=c("POSITIVENEUTRAL"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_POSITIVENEUTRAL") ),
                                       mxPath(from="FIFTY_FIFTYNEUTRAL",to=c("FIFTY_FIFTYNEUTRAL"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_FIFTY_FIFTYNEUTRAL") ),
                                       mxPath(from="one",to=c("negativeBBSIQ_PRE","negativeBBSIQ_SESSION3","negativeBBSIQ_SESSION6","FIFTY_FIFTYANXIETY","NONEANXIETY","POSITIVEANXIETY","POSITIVENEUTRAL","FIFTY_FIFTYNEUTRAL"), free=F, value=0, arrows=1),
                                       mxData(BBSIQNeg_modelData, type = "raw")
);
resultBBSIQmodelPosAnxNoneAnxSame <- mxRun(BBSIQmodelPosAnxNoneAnxSame)
mxCompare(resultBBSIQmodelAllDiff,resultBBSIQmodelPosAnxNoneAnxSame)

#### PosNeut = NoTrainNeutal
BBSIQmodPosNeutSameNoneNeut <-mxModel(BBSIQmodelAllDiff,
                                      mxPath(from="POSITIVENEUTRAL",to=c("Slope","Intercept"), free=c(FALSE,FALSE), 
                                             value=c(0.0,0.0), arrows=1, label=c("POSITIVENEUTRAL__Slope","POSITIVENEUTRAL__Intercept") ))
resultBBSIQmodPosNeutSameNoneNeut <- mxRun(BBSIQmodPosNeutSameNoneNeut)
mxCompare(resultBBSIQmodelAllDiff,resultBBSIQmodPosNeutSameNoneNeut)


#### PosNeut = NoTrainAnx
BBSIQmodelPosNeutNoTrainAnxSame <- mxModel("BBSIQmodelPosNeutNoTrainAnxSame", 
                                           type="RAM",
                                           manifestVars = manifests,
                                           latentVars = latents,
                                           mxPath(from="Intercept",to=c("negativeBBSIQ_PRE","negativeBBSIQ_SESSION3","negativeBBSIQ_SESSION6"), free=c(FALSE,FALSE,FALSE), value=c(1.0,1.0,1.0) , arrows=1, label=c("Intercept__negativeBBSIQ_PRE","Intercept__negativeBBSIQ_SESSION3","Intercept__negativeBBSIQ_SESSION6") ),
                                           mxPath(from="Slope",to=c("negativeBBSIQ_PRE","negativeBBSIQ_SESSION3"), free=c(FALSE,FALSE), value=c(-2.0,-1.0) , arrows=1, label=c("Slope__negativeBBSIQ_PRE","Slope__negativeBBSIQ_SESSION3") ),
                                           #mxPath(from="Dropout",to=c("Intercept"), free=c(TRUE), value=c(0.8) , arrows=1, label=c("Dropout__Intercept") ),
                                           mxPath(from="one",to=c("Intercept","Slope"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("const__Intercept","const__Slope") ),
                                           mxPath(from="FIFTY_FIFTYANXIETY",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("FIFTY_FIFTYANXIETY__Slope","FIFTY_FIFTYANXIETY__Intercept") ),
                                           mxPath(from="NONEANXIETY",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("SAME__Slope","SAME__Intercept") ),
                                           mxPath(from="POSITIVEANXIETY",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("POSITIVEANXIETY__Slope","POSITIVEANXIETY__Intercept") ),
                                           mxPath(from="POSITIVENEUTRAL",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("SAME__Slope","SAME__Intercept") ),
                                           mxPath(from="FIFTY_FIFTYNEUTRAL",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("FIFTY_FIFTYNEUTRAL__Slope","FIFTY_FIFTYNEUTRAL__Intercept") ),
                                           mxPath(from="negativeBBSIQ_PRE",to=c("negativeBBSIQ_PRE"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_negativeBBSIQ_PRE") ),
                                           mxPath(from="negativeBBSIQ_SESSION3",to=c("negativeBBSIQ_SESSION3"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_negativeBBSIQ_SESSION3") ),
                                           mxPath(from="negativeBBSIQ_SESSION6",to=c("negativeBBSIQ_SESSION6"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_negativeBBSIQ_SESSION6") ),
                                           mxPath(from="Intercept",to=c("Intercept"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_Intercept") ),
                                           mxPath(from="Slope",to=c("Slope"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_Slope") ),
                                           #mxPath(from="Dropout",to=c("Dropout"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_Dropout") ),
                                           mxPath(from="FIFTY_FIFTYANXIETY",to=c("FIFTY_FIFTYANXIETY"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_FIFTY_FIFTYANXIETY") ),
                                           mxPath(from="NONEANXIETY",to=c("NONEANXIETY"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_NONEANXIETY") ),
                                           mxPath(from="POSITIVEANXIETY",to=c("POSITIVEANXIETY"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_POSITIVEANXIETY") ),
                                           mxPath(from="POSITIVENEUTRAL",to=c("POSITIVENEUTRAL"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_POSITIVENEUTRAL") ),
                                           mxPath(from="FIFTY_FIFTYNEUTRAL",to=c("FIFTY_FIFTYNEUTRAL"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_FIFTY_FIFTYNEUTRAL") ),
                                           mxPath(from="one",to=c("negativeBBSIQ_PRE","negativeBBSIQ_SESSION3","negativeBBSIQ_SESSION6","FIFTY_FIFTYANXIETY","NONEANXIETY","POSITIVEANXIETY","POSITIVENEUTRAL","FIFTY_FIFTYNEUTRAL"), free=F, value=0, arrows=1),
                                           mxData(BBSIQNeg_modelData, type = "raw")
);
resultBBSIQmodelPosNeutNoTrainAnxSame<- mxRun(BBSIQmodelPosNeutNoTrainAnxSame)
mxCompare(resultBBSIQmodelAllDiff,resultBBSIQmodelPosNeutNoTrainAnxSame)

#### 5050Anx = NoTrainNeut
BBSIQmod5050AnxSameNoneNeut <-mxModel(BBSIQmodelAllDiff,
                             mxPath(from="FIFTY_FIFTYANXIETY",to=c("Slope","Intercept"), free=c(FALSE,FALSE), 
                                    value=c(0.0,0.0), arrows=1, label=c("FIFTY_FIFTYANXIETY__Slope","FIFTY_FIFTYANXIETY__Intercept") ))
resultBBSIQmod5050AnxSameNoneNeut <- mxRun(BBSIQmod5050AnxSameNoneNeut)
mxCompare(resultBBSIQmodelAllDiff,resultBBSIQmod5050AnxSameNoneNeut)

#### 5050Neut = NoTrainNeut
BBSIQmod5050NeutSameNoneNeut  <-mxModel(BBSIQmodelAllDiff,
                              mxPath(from="FIFTY_FIFTYNEUTRAL",to=c("Slope","Intercept"), free=c(FALSE,FALSE), 
                                     value=c(0.0,0.0), arrows=1, label=c("FIFTY_FIFTYNEUTRAL__Slope","FIFTY_FIFTYNEUTRAL__Intercept") ))
resultBBSIQmod5050NeutSameNoneNeut <- mxRun(BBSIQmod5050NeutSameNoneNeut)
mxCompare(resultBBSIQmodelAllDiff,resultBBSIQmod5050NeutSameNoneNeut)

#### 5050Anx = NoTrainAnx
BBSIQmodel5050AnxNoneAnxSame <- mxModel("BBSIQmodel5050AnxNoneAnxSame", 
                                        type="RAM",
                                        manifestVars = manifests,
                                        latentVars = latents,
                                        mxPath(from="Intercept",to=c("negativeBBSIQ_PRE","negativeBBSIQ_SESSION3","negativeBBSIQ_SESSION6"), free=c(FALSE,FALSE,FALSE), value=c(1.0,1.0,1.0) , arrows=1, label=c("Intercept__negativeBBSIQ_PRE","Intercept__negativeBBSIQ_SESSION3","Intercept__negativeBBSIQ_SESSION6") ),
                                        mxPath(from="Slope",to=c("negativeBBSIQ_PRE","negativeBBSIQ_SESSION3"), free=c(FALSE,FALSE), value=c(-2.0,-1.0) , arrows=1, label=c("Slope__negativeBBSIQ_PRE","Slope__negativeBBSIQ_SESSION3") ),
                                        #mxPath(from="Dropout",to=c("Intercept"), free=c(TRUE), value=c(0.8) , arrows=1, label=c("Dropout__Intercept") ),
                                        mxPath(from="one",to=c("Intercept","Slope"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("const__Intercept","const__Slope") ),
                                        mxPath(from="FIFTY_FIFTYANXIETY",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("SAME__Slope","SAME__Intercept") ),
                                        mxPath(from="NONEANXIETY",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("SAME__Slope","SAME__Intercept") ),
                                        mxPath(from="POSITIVEANXIETY",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("POSITIVEANXIETY__Slope","POSITIVEANXIETY__Intercept") ),
                                        mxPath(from="POSITIVENEUTRAL",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("POSITIVENEUTRAL__Slope","POSITIVENEUTRAL__Intercept") ),
                                        mxPath(from="FIFTY_FIFTYNEUTRAL",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("FIFTY_FIFTYNEUTRAL__Slope","FIFTY_FIFTYNEUTRAL__Intercept") ),
                                        mxPath(from="negativeBBSIQ_PRE",to=c("negativeBBSIQ_PRE"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_negativeBBSIQ_PRE") ),
                                        mxPath(from="negativeBBSIQ_SESSION3",to=c("negativeBBSIQ_SESSION3"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_negativeBBSIQ_SESSION3") ),
                                        mxPath(from="negativeBBSIQ_SESSION6",to=c("negativeBBSIQ_SESSION6"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_negativeBBSIQ_SESSION6") ),
                                        mxPath(from="Intercept",to=c("Intercept"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_Intercept") ),
                                        mxPath(from="Slope",to=c("Slope"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_Slope") ),
                                        #mxPath(from="Dropout",to=c("Dropout"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_Dropout") ),
                                        mxPath(from="FIFTY_FIFTYANXIETY",to=c("FIFTY_FIFTYANXIETY"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_FIFTY_FIFTYANXIETY") ),
                                        mxPath(from="NONEANXIETY",to=c("NONEANXIETY"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_NONEANXIETY") ),
                                        mxPath(from="POSITIVEANXIETY",to=c("POSITIVEANXIETY"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_POSITIVEANXIETY") ),
                                        mxPath(from="POSITIVENEUTRAL",to=c("POSITIVENEUTRAL"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_POSITIVENEUTRAL") ),
                                        mxPath(from="FIFTY_FIFTYNEUTRAL",to=c("FIFTY_FIFTYNEUTRAL"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_FIFTY_FIFTYNEUTRAL") ),
                                        mxPath(from="one",to=c("negativeBBSIQ_PRE","negativeBBSIQ_SESSION3","negativeBBSIQ_SESSION6","FIFTY_FIFTYANXIETY","NONEANXIETY","POSITIVEANXIETY","POSITIVENEUTRAL","FIFTY_FIFTYNEUTRAL"), free=F, value=0, arrows=1),
                                        mxData(BBSIQNeg_modelData, type = "raw")
);
resultBBSIQmodel5050AnxNoneAnxSame <- mxRun(BBSIQmodel5050AnxNoneAnxSame)
mxCompare(resultBBSIQmodelAllDiff,resultBBSIQmodel5050AnxNoneAnxSame)

#### 5050Neut = NoTrainAnx
BBSIQmodel5050NeutNoneAnxSame <- mxModel("BBSIQmodel5050NeutNoneAnxSame", 
                                         type="RAM",
                                         manifestVars = manifests,
                                         latentVars = latents,
                                         mxPath(from="Intercept",to=c("negativeBBSIQ_PRE","negativeBBSIQ_SESSION3","negativeBBSIQ_SESSION6"), free=c(FALSE,FALSE,FALSE), value=c(1.0,1.0,1.0) , arrows=1, label=c("Intercept__negativeBBSIQ_PRE","Intercept__negativeBBSIQ_SESSION3","Intercept__negativeBBSIQ_SESSION6") ),
                                         mxPath(from="Slope",to=c("negativeBBSIQ_PRE","negativeBBSIQ_SESSION3"), free=c(FALSE,FALSE), value=c(-2.0,-1.0) , arrows=1, label=c("Slope__negativeBBSIQ_PRE","Slope__negativeBBSIQ_SESSION3") ),
                                         #mxPath(from="Dropout",to=c("Intercept"), free=c(TRUE), value=c(0.8) , arrows=1, label=c("Dropout__Intercept") ),
                                         mxPath(from="one",to=c("Intercept","Slope"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("const__Intercept","const__Slope") ),
                                         mxPath(from="FIFTY_FIFTYANXIETY",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("FIFTY_FIFTYANXIETY__Slope","FIFTY_FIFTYANXIETY__Intercept") ),
                                         mxPath(from="NONEANXIETY",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("SAME__Slope","SAME__Intercept") ),
                                         mxPath(from="POSITIVEANXIETY",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("POSITIVEANXIETY__Slope","POSITIVEANXIETY__Intercept") ),
                                         mxPath(from="POSITIVENEUTRAL",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("POSITIVENEUTRAL__Slope","POSITIVENEUTRAL__Intercept") ),
                                         mxPath(from="FIFTY_FIFTYNEUTRAL",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("SAME__Slope","SAME__Intercept") ),
                                         mxPath(from="negativeBBSIQ_PRE",to=c("negativeBBSIQ_PRE"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_negativeBBSIQ_PRE") ),
                                         mxPath(from="negativeBBSIQ_SESSION3",to=c("negativeBBSIQ_SESSION3"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_negativeBBSIQ_SESSION3") ),
                                         mxPath(from="negativeBBSIQ_SESSION6",to=c("negativeBBSIQ_SESSION6"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_negativeBBSIQ_SESSION6") ),
                                         mxPath(from="Intercept",to=c("Intercept"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_Intercept") ),
                                         mxPath(from="Slope",to=c("Slope"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_Slope") ),
                                         #mxPath(from="Dropout",to=c("Dropout"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_Dropout") ),
                                         mxPath(from="FIFTY_FIFTYANXIETY",to=c("FIFTY_FIFTYANXIETY"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_FIFTY_FIFTYANXIETY") ),
                                         mxPath(from="NONEANXIETY",to=c("NONEANXIETY"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_NONEANXIETY") ),
                                         mxPath(from="POSITIVEANXIETY",to=c("POSITIVEANXIETY"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_POSITIVEANXIETY") ),
                                         mxPath(from="POSITIVENEUTRAL",to=c("POSITIVENEUTRAL"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_POSITIVENEUTRAL") ),
                                         mxPath(from="FIFTY_FIFTYNEUTRAL",to=c("FIFTY_FIFTYNEUTRAL"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_FIFTY_FIFTYNEUTRAL") ),
                                         mxPath(from="one",to=c("negativeBBSIQ_PRE","negativeBBSIQ_SESSION3","negativeBBSIQ_SESSION6","FIFTY_FIFTYANXIETY","NONEANXIETY","POSITIVEANXIETY","POSITIVENEUTRAL","FIFTY_FIFTYNEUTRAL"), free=F, value=0, arrows=1),
                                         mxData(BBSIQNeg_modelData, type = "raw")
);
resultBBSIQmodel5050NeutNoneAnxSame <- mxRun(BBSIQmodel5050NeutNoneAnxSame)
mxCompare(resultBBSIQmodelAllDiff,resultBBSIQmodel5050NeutNoneAnxSame)

###### PART B ######: ACTIVE GROUPS VS. EACH OTHER

#### PosAnx = 5050Anx
BBSIQmodelPosAnx5050AnxSame <- mxModel("BBSIQmodelPosAnx5050AnxSame", 
                                       type="RAM",
                                       manifestVars = manifests,
                                       latentVars = latents,
                                       mxPath(from="Intercept",to=c("negativeBBSIQ_PRE","negativeBBSIQ_SESSION3","negativeBBSIQ_SESSION6"), free=c(FALSE,FALSE,FALSE), value=c(1.0,1.0,1.0) , arrows=1, label=c("Intercept__negativeBBSIQ_PRE","Intercept__negativeBBSIQ_SESSION3","Intercept__negativeBBSIQ_SESSION6") ),
                                       mxPath(from="Slope",to=c("negativeBBSIQ_PRE","negativeBBSIQ_SESSION3"), free=c(FALSE,FALSE), value=c(-2.0,-1.0) , arrows=1, label=c("Slope__negativeBBSIQ_PRE","Slope__negativeBBSIQ_SESSION3") ),
                                       #mxPath(from="Dropout",to=c("Intercept"), free=c(TRUE), value=c(0.8) , arrows=1, label=c("Dropout__Intercept") ),
                                       mxPath(from="one",to=c("Intercept","Slope"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("const__Intercept","const__Slope") ),
                                       mxPath(from="FIFTY_FIFTYANXIETY",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("SAME__Slope","SAME__Intercept") ),
                                       mxPath(from="NONEANXIETY",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("NONEANXIETY__Slope","NONEANXIETY__Intercept") ),
                                       mxPath(from="POSITIVEANXIETY",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("SAME__Slope","SAME__Intercept") ),
                                       mxPath(from="POSITIVENEUTRAL",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("POSITIVENEUTRAL__Slope","POSITIVENEUTRAL__Intercept") ),
                                       mxPath(from="FIFTY_FIFTYNEUTRAL",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("FIFTY_FIFTYNEUTRAL__Slope","FIFTY_FIFTYNEUTRAL__Intercept") ),
                                       mxPath(from="negativeBBSIQ_PRE",to=c("negativeBBSIQ_PRE"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_negativeBBSIQ_PRE") ),
                                       mxPath(from="negativeBBSIQ_SESSION3",to=c("negativeBBSIQ_SESSION3"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_negativeBBSIQ_SESSION3") ),
                                       mxPath(from="negativeBBSIQ_SESSION6",to=c("negativeBBSIQ_SESSION6"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_negativeBBSIQ_SESSION6") ),
                                       mxPath(from="Intercept",to=c("Intercept"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_Intercept") ),
                                       mxPath(from="Slope",to=c("Slope"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_Slope") ),
                                       #mxPath(from="Dropout",to=c("Dropout"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_Dropout") ),
                                       mxPath(from="FIFTY_FIFTYANXIETY",to=c("FIFTY_FIFTYANXIETY"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_FIFTY_FIFTYANXIETY") ),
                                       mxPath(from="NONEANXIETY",to=c("NONEANXIETY"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_NONEANXIETY") ),
                                       mxPath(from="POSITIVEANXIETY",to=c("POSITIVEANXIETY"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_POSITIVEANXIETY") ),
                                       mxPath(from="POSITIVENEUTRAL",to=c("POSITIVENEUTRAL"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_POSITIVENEUTRAL") ),
                                       mxPath(from="FIFTY_FIFTYNEUTRAL",to=c("FIFTY_FIFTYNEUTRAL"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_FIFTY_FIFTYNEUTRAL") ),
                                       mxPath(from="one",to=c("negativeBBSIQ_PRE","negativeBBSIQ_SESSION3","negativeBBSIQ_SESSION6","FIFTY_FIFTYANXIETY","NONEANXIETY","POSITIVEANXIETY","POSITIVENEUTRAL","FIFTY_FIFTYNEUTRAL"), free=F, value=0, arrows=1),
                                       mxData(BBSIQNeg_modelData, type = "raw")
);
resultBBSIQmodelPosAnx5050AnxSame<- mxRun(BBSIQmodelPosAnx5050AnxSame)
mxCompare(resultBBSIQmodelAllDiff,resultBBSIQmodelPosAnx5050AnxSame)

#### PosNeut = 5050Anx
BBSIQmodelPosNeut5050AnxSame <- mxModel("BBSIQmodelPosNeut5050AnxSame", 
                                       type="RAM",
                                       manifestVars = manifests,
                                       latentVars = latents,
                                       mxPath(from="Intercept",to=c("negativeBBSIQ_PRE","negativeBBSIQ_SESSION3","negativeBBSIQ_SESSION6"), free=c(FALSE,FALSE,FALSE), value=c(1.0,1.0,1.0) , arrows=1, label=c("Intercept__negativeBBSIQ_PRE","Intercept__negativeBBSIQ_SESSION3","Intercept__negativeBBSIQ_SESSION6") ),
                                       mxPath(from="Slope",to=c("negativeBBSIQ_PRE","negativeBBSIQ_SESSION3"), free=c(FALSE,FALSE), value=c(-2.0,-1.0) , arrows=1, label=c("Slope__negativeBBSIQ_PRE","Slope__negativeBBSIQ_SESSION3") ),
                                       #mxPath(from="Dropout",to=c("Intercept"), free=c(TRUE), value=c(0.8) , arrows=1, label=c("Dropout__Intercept") ),
                                       mxPath(from="one",to=c("Intercept","Slope"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("const__Intercept","const__Slope") ),
                                       mxPath(from="FIFTY_FIFTYANXIETY",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("SAME__Slope","SAME__Intercept") ),
                                       mxPath(from="NONEANXIETY",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("NONEANXIETY__Slope","NONEANXIETY__Intercept") ),
                                       mxPath(from="POSITIVEANXIETY",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("POSITIVEANXIETY__Slope","POSITIVEANXIETY__Intercept") ),
                                       mxPath(from="POSITIVENEUTRAL",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("SAME__Slope","SAME__Intercept") ),
                                       mxPath(from="FIFTY_FIFTYNEUTRAL",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("FIFTY_FIFTYNEUTRAL__Slope","FIFTY_FIFTYNEUTRAL__Intercept") ),
                                       mxPath(from="negativeBBSIQ_PRE",to=c("negativeBBSIQ_PRE"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_negativeBBSIQ_PRE") ),
                                       mxPath(from="negativeBBSIQ_SESSION3",to=c("negativeBBSIQ_SESSION3"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_negativeBBSIQ_SESSION3") ),
                                       mxPath(from="negativeBBSIQ_SESSION6",to=c("negativeBBSIQ_SESSION6"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_negativeBBSIQ_SESSION6") ),
                                       mxPath(from="Intercept",to=c("Intercept"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_Intercept") ),
                                       mxPath(from="Slope",to=c("Slope"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_Slope") ),
                                       #mxPath(from="Dropout",to=c("Dropout"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_Dropout") ),
                                       mxPath(from="FIFTY_FIFTYANXIETY",to=c("FIFTY_FIFTYANXIETY"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_FIFTY_FIFTYANXIETY") ),
                                       mxPath(from="NONEANXIETY",to=c("NONEANXIETY"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_NONEANXIETY") ),
                                       mxPath(from="POSITIVEANXIETY",to=c("POSITIVEANXIETY"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_POSITIVEANXIETY") ),
                                       mxPath(from="POSITIVENEUTRAL",to=c("POSITIVENEUTRAL"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_POSITIVENEUTRAL") ),
                                       mxPath(from="FIFTY_FIFTYNEUTRAL",to=c("FIFTY_FIFTYNEUTRAL"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_FIFTY_FIFTYNEUTRAL") ),
                                       mxPath(from="one",to=c("negativeBBSIQ_PRE","negativeBBSIQ_SESSION3","negativeBBSIQ_SESSION6","FIFTY_FIFTYANXIETY","NONEANXIETY","POSITIVEANXIETY","POSITIVENEUTRAL","FIFTY_FIFTYNEUTRAL"), free=F, value=0, arrows=1),
                                       mxData(BBSIQNeg_modelData, type = "raw")
);
resultBBSIQmodelPosNeut5050AnxSame<- mxRun(BBSIQmodelPosNeut5050AnxSame)
mxCompare(resultBBSIQmodelAllDiff,resultBBSIQmodelPosNeut5050AnxSame)

#### PosAnx = 5050Neut
BBSIQmodelPosAnx5050NeutSame <- mxModel("BBSIQmodelPosAnx5050AnxSame", 
                                       type="RAM",
                                       manifestVars = manifests,
                                       latentVars = latents,
                                       mxPath(from="Intercept",to=c("negativeBBSIQ_PRE","negativeBBSIQ_SESSION3","negativeBBSIQ_SESSION6"), free=c(FALSE,FALSE,FALSE), value=c(1.0,1.0,1.0) , arrows=1, label=c("Intercept__negativeBBSIQ_PRE","Intercept__negativeBBSIQ_SESSION3","Intercept__negativeBBSIQ_SESSION6") ),
                                       mxPath(from="Slope",to=c("negativeBBSIQ_PRE","negativeBBSIQ_SESSION3"), free=c(FALSE,FALSE), value=c(-2.0,-1.0) , arrows=1, label=c("Slope__negativeBBSIQ_PRE","Slope__negativeBBSIQ_SESSION3") ),
                                       #mxPath(from="Dropout",to=c("Intercept"), free=c(TRUE), value=c(0.8) , arrows=1, label=c("Dropout__Intercept") ),
                                       mxPath(from="one",to=c("Intercept","Slope"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("const__Intercept","const__Slope") ),
                                       mxPath(from="FIFTY_FIFTYANXIETY",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("FIFTY_FIFTYANXIETY__Slope","FIFTY_FIFTYANXIETY__Intercept") ),
                                       mxPath(from="NONEANXIETY",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("NONEANXIETY__Slope","NONEANXIETY__Intercept") ),
                                       mxPath(from="POSITIVEANXIETY",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("SAME__Slope","SAME__Intercept") ),
                                       mxPath(from="POSITIVENEUTRAL",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("POSITIVENEUTRAL__Slope","POSITIVENEUTRAL__Intercept") ),
                                       mxPath(from="FIFTY_FIFTYNEUTRAL",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("SAME__Slope","SAME__Intercept") ),
                                       mxPath(from="negativeBBSIQ_PRE",to=c("negativeBBSIQ_PRE"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_negativeBBSIQ_PRE") ),
                                       mxPath(from="negativeBBSIQ_SESSION3",to=c("negativeBBSIQ_SESSION3"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_negativeBBSIQ_SESSION3") ),
                                       mxPath(from="negativeBBSIQ_SESSION6",to=c("negativeBBSIQ_SESSION6"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_negativeBBSIQ_SESSION6") ),
                                       mxPath(from="Intercept",to=c("Intercept"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_Intercept") ),
                                       mxPath(from="Slope",to=c("Slope"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_Slope") ),
                                       #mxPath(from="Dropout",to=c("Dropout"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_Dropout") ),
                                       mxPath(from="FIFTY_FIFTYANXIETY",to=c("FIFTY_FIFTYANXIETY"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_FIFTY_FIFTYANXIETY") ),
                                       mxPath(from="NONEANXIETY",to=c("NONEANXIETY"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_NONEANXIETY") ),
                                       mxPath(from="POSITIVEANXIETY",to=c("POSITIVEANXIETY"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_POSITIVEANXIETY") ),
                                       mxPath(from="POSITIVENEUTRAL",to=c("POSITIVENEUTRAL"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_POSITIVENEUTRAL") ),
                                       mxPath(from="FIFTY_FIFTYNEUTRAL",to=c("FIFTY_FIFTYNEUTRAL"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_FIFTY_FIFTYNEUTRAL") ),
                                       mxPath(from="one",to=c("negativeBBSIQ_PRE","negativeBBSIQ_SESSION3","negativeBBSIQ_SESSION6","FIFTY_FIFTYANXIETY","NONEANXIETY","POSITIVEANXIETY","POSITIVENEUTRAL","FIFTY_FIFTYNEUTRAL"), free=F, value=0, arrows=1),
                                       mxData(BBSIQNeg_modelData, type = "raw")
);
resultBBSIQmodelPosAnx5050NeutSame<- mxRun(BBSIQmodelPosAnx5050NeutSame)
mxCompare(resultBBSIQmodelAllDiff,resultBBSIQmodelPosAnx5050NeutSame)


#### PosNeut = 5050Neut 
BBSIQmodelPosNeut5050NeutSame <- mxModel("BBSIQmodelPosNeut5050NeutSame", 
                                        type="RAM",
                                        manifestVars = manifests,
                                        latentVars = latents,
                                        mxPath(from="Intercept",to=c("negativeBBSIQ_PRE","negativeBBSIQ_SESSION3","negativeBBSIQ_SESSION6"), free=c(FALSE,FALSE,FALSE), value=c(1.0,1.0,1.0) , arrows=1, label=c("Intercept__negativeBBSIQ_PRE","Intercept__negativeBBSIQ_SESSION3","Intercept__negativeBBSIQ_SESSION6") ),
                                        mxPath(from="Slope",to=c("negativeBBSIQ_PRE","negativeBBSIQ_SESSION3"), free=c(FALSE,FALSE), value=c(-2.0,-1.0) , arrows=1, label=c("Slope__negativeBBSIQ_PRE","Slope__negativeBBSIQ_SESSION3") ),
                                        #mxPath(from="Dropout",to=c("Intercept"), free=c(TRUE), value=c(0.8) , arrows=1, label=c("Dropout__Intercept") ),
                                        mxPath(from="one",to=c("Intercept","Slope"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("const__Intercept","const__Slope") ),
                                        mxPath(from="FIFTY_FIFTYANXIETY",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("FIFTY_FIFTYANXIETY__Slope","FIFTY_FIFTYANXIETY__Intercept") ),
                                        mxPath(from="NONEANXIETY",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("NONEANXIETY__Slope","NONEANXIETY__Intercept") ),
                                        mxPath(from="POSITIVEANXIETY",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("POSITIVEANXIETY__Slope","POSITIVEANXIETY__Intercept") ),
                                        mxPath(from="POSITIVENEUTRAL",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("SAME__Slope","SAME__Intercept") ),
                                        mxPath(from="FIFTY_FIFTYNEUTRAL",to=c("Slope","Intercept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("SAME__Slope","SAME__Intercept") ),
                                        mxPath(from="negativeBBSIQ_PRE",to=c("negativeBBSIQ_PRE"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_negativeBBSIQ_PRE") ),
                                        mxPath(from="negativeBBSIQ_SESSION3",to=c("negativeBBSIQ_SESSION3"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_negativeBBSIQ_SESSION3") ),
                                        mxPath(from="negativeBBSIQ_SESSION6",to=c("negativeBBSIQ_SESSION6"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_negativeBBSIQ_SESSION6") ),
                                        mxPath(from="Intercept",to=c("Intercept"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_Intercept") ),
                                        mxPath(from="Slope",to=c("Slope"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_Slope") ),
                                        #mxPath(from="Dropout",to=c("Dropout"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_Dropout") ),
                                        mxPath(from="FIFTY_FIFTYANXIETY",to=c("FIFTY_FIFTYANXIETY"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_FIFTY_FIFTYANXIETY") ),
                                        mxPath(from="NONEANXIETY",to=c("NONEANXIETY"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_NONEANXIETY") ),
                                        mxPath(from="POSITIVEANXIETY",to=c("POSITIVEANXIETY"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_POSITIVEANXIETY") ),
                                        mxPath(from="POSITIVENEUTRAL",to=c("POSITIVENEUTRAL"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_POSITIVENEUTRAL") ),
                                        mxPath(from="FIFTY_FIFTYNEUTRAL",to=c("FIFTY_FIFTYNEUTRAL"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_FIFTY_FIFTYNEUTRAL") ),
                                        mxPath(from="one",to=c("negativeBBSIQ_PRE","negativeBBSIQ_SESSION3","negativeBBSIQ_SESSION6","FIFTY_FIFTYANXIETY","NONEANXIETY","POSITIVEANXIETY","POSITIVENEUTRAL","FIFTY_FIFTYNEUTRAL"), free=F, value=0, arrows=1),
                                        mxData(BBSIQNeg_modelData, type = "raw")
);
resultBBSIQmodelPosNeut5050NeutSame<- mxRun(BBSIQmodelPosNeut5050NeutSame)
mxCompare(resultBBSIQmodelAllDiff,resultBBSIQmodelPosNeut5050NeutSame)

