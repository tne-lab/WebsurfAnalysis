### Script analyses reliability of decision threshold over repeated sessions of Websurf task
### Can easily be adapted to assess reliability of other parameters
### Calculating ICCs for first two sessions and using cross validation to assess reliability of all recorded sessions
#for each participant

#Loading packages and data
rm(list = ls())
library(xlsx)
library(ggplot2)
library(lmerTest)
library(BayesFactor)
library(r2glmm)

projectFolder <- "/Volumes/GoogleDrive/My Drive/Websurf/"
dataPath <- paste0(projectFolder, "Data/Processed/thetas.xlsx")
dat <- read.xlsx(dataPath, 1)

structureDat <- function(data, yVar){
  #looping over participants to get the data from their first two sessions
  datBL <- list()
  datICC_Animal <- list()
  datICC_Dance <- list()
  datICC_Fail <- list()
  datICC_Landscape <- list()
  IDs <- levels(as.factor(dat$IDVec))
  for(p in 1:length(IDs)){
    datP <- dat[dat$IDVec == IDs[p],]
    runs <- levels(as.factor(datP$runID))
    datBL[[p]] <- rbind(datP[datP$runID == runs[1],], datP[datP$runID == runs[2],]) 
    #structring data for ICC later
    datICC_Animal[[p]] <- cbind(datBL[[p]][datBL[[p]]$Category == "Animal",][1,yVar], datBL[[p]][datBL[[p]]$Category == "Animal",][2,yVar])
    datICC_Dance[[p]] <- cbind(datBL[[p]][datBL[[p]]$Category == "Dance",][1,yVar], datBL[[p]][datBL[[p]]$Category == "Dance",][2,yVar])
    datICC_Fail[[p]] <- cbind(datBL[[p]][datBL[[p]]$Category == "Fail",][1,yVar], datBL[[p]][datBL[[p]]$Category == "Fail",][2,yVar])
    datICC_Landscape[[p]] <- cbind(datBL[[p]][datBL[[p]]$Category == "Landscape",][1,yVar], datBL[[p]][datBL[[p]]$Category == "Landscape",][2,yVar])
  }
  datBL <<- do.call(rbind, datBL)
  datICC_Animal <<- do.call(rbind, datICC_Animal)
  datICC_Dance <<- do.call(rbind, datICC_Dance)
  datICC_Fail <<- do.call(rbind, datICC_Fail)
  datICC_Landscape <<- do.call(rbind, datICC_Landscape)
}

structureDat(dat, "theta")

# running ICCs
icc(datICC_Animal)
icc(datICC_Dance)
icc(datICC_Fail)
icc(datICC_Landscape)

# Below cross validating all sessions for each participant by removing one session at a time to predict theta
IDs <- levels(as.factor(dat$IDVec))
IDs <- IDs[-c(7,13)]
for(p in 1:length(IDs)){
  #Cross-validate the model, leaving out one runID at a time:
  datCV <- dat[dat$IDVec == IDs[p],] #get the current participant's data
  runs <- levels(as.factor(datCV$runID)) #make sure we treat the runs as a factor
  if(length(runs) == 1){ #this handles participants who only completed one run 
    next 
  }
  for(run in 1:length(runs)){ #Looping over each of the participant's runs
    train <- datCV[datCV$runID != runs[run],] #iteratively remove a session, remaining data is for training
    test <- datCV[datCV$runID == runs[run],] #use the removed session as the test data
    fit <- lm(theta ~ Category, data = datCV[datCV$runID != runs[run],]) #LM for theta of current run by category
    predTheta <- predict(fit, newdata=test) #predict the thetas
    datCV$predictedTheta[datCV$runID == runs[run]] <- predTheta #save the predictions for the current run
  } #end run loop
  dat$predictedTheta[dat$IDVec == IDs[p]] <- datCV$predictedTheta #add all predictions to the data frame 
}#end participant loop

dat <- na.omit(dat) #removing NAs produced by participants who only completed one session 

#compare predicted theta values against observed values
Full_model_lmer = lmer(formula = theta ~ predictedTheta + (1|IDVec), data = dat) 
anova(Full_model_lmer) 
#r2 gives accuracy of model
r2beta(Full_model_lmer, method = 'kr')

#plot predicted thresholds by observed thresholds with linear trend 
plotTheta <- ggplot(data = dat, aes(x = predictedTheta, y = theta))+
  geom_point()+
  geom_smooth(method = lm, level = .95)+ #add linear trend line with CI
  xlab("Predicted theta")+
  ylab("Observed theta")+
  theme_classic()
plotTheta  




