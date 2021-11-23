### Function looks at the sequence of choices to determine regret-inducing trials.
### Regret inducing trials are defined as trials where participants chose to skip a high-value offer 
# in the previous trial, and are presented with a low-value offer in the current trial. 
### Based on Steiner and Redish's (2014) paper in rodents, we expect that in regret inducing trials, 
# participants will be more likely to wait for the low-value offer. 
### Function runs at the experimental session level - e.g. will need to loop over repeated sessions

#Required packages
require(ggplot2)

WS_Regret <- function(dat2use){
  dat2use$regretTrial <-  NA
  #loop over trials in run
  for(trial in 1:nrow(dat2use)){
    if(trial == 1){ #move to next trial if current trial is the first of the session
      dat2use$regretTrial[trial] <- 0
      next
    }
    curTrial <- dat2use[trial,]
    prevTrial <- dat2use[trial-1,]
    if(prevTrial$Choices == 0 & prevTrial$offerValue > 0 & curTrial$offerValue < 0){ #if participant skipped a good offer in the previous trial and the current offer is low value
      dat2use$regretTrial[trial] <- 1 #code current trial as a regret inducing trial
    } else{
      dat2use$regretTrial[trial] <- 0 #otherwise, current trial is not regret inducing
    }
  }
  
  regretTrials <- dat2use[dat2use$regretTrial == 1,]
  regretTrialsWait <- regretTrials[regretTrials$Choices == 1,]
  percRegretWait <- (nrow(regretTrialsWait)/nrow(regretTrials))*100
  
  regretTrials$Decision <- ifelse(regretTrials$Choices == 1, "Stay", "Skip")
  
  plotRegret <- ggplot(data = regretTrials, aes(x = offerValue, y = Decision, color = Choices))+
    geom_point()+
    theme_classic()+
    theme(legend.position = "none")
  
  Names <- c("regretTrials", "plotGaus", "plotDiff", "RT_Diffs")
  
  Reg_Out <- list(regretTrials,
                  regretTrialsWait,
                  percRegretWait,
                  plotRegret)
  names(Reg_Out) <- Names
  
  return(Reg_Out)
  
}
