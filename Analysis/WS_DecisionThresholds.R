########################################################################################################################
# This script is extracting parameters from processed Websurf data 
# Written Nov 2021 by Aaron McInnes for the Widge lab Websurf project
# Decision thresholds are calculated using an iterative heaviside step function which I adapted from Annie Haynos' code


########################################################################################################################

#starting up
rm(list = ls())
options(scipen=999)

# Load packages
library(tidyverse)
library(ggplot2)
library(patchwork)
library(gridExtra) 
library(RColorBrewer)
library(xlsx)

#set to 0 if you don't want to save the plot files, otherwise 1
savePlots <-  0

#Change path to where .csv files are saved
dataPath <- "/Volumes/GoogleDrive/My Drive/Websurf/Data/Processed/"
savePath <- paste0(dataPath, "Plots/") #Folder to store plots (may need to make the directory)
setwd(dataPath)

#Function below handles plotting 
#set colours for plotting
colorPal <- brewer.pal(4, "Set2")
#function for plotting heaviside functions 
doHWplot <- function(data2plot, th){ #data for current category, theta
  curveDat <- matrix(NA, length(seq(3, 30, by = .1)), 2)
  curveDat[,1] <- seq(3, 30, by = .1)
  curveDat[,2] = ifelse(seq(3, 30, by = .1) < th, y=1, 0)
  if(all(data2plot$Choices == 1)){ #This keeps threshold at 1 if all trials are stays. This is just an aesthetic choice
    curveDat[,2] = 1
  }
  currentCat <- paste0(data2plot$Category[1], " Category")
  ggplot(data = data2plot, aes(x = Delays, y = Choices))+
    geom_point(color = color2use)+
    geom_line(data = as.data.frame(curveDat), aes(x = curveDat[,1], y = curveDat[,2]))+
    coord_cartesian(ylim = c(0,1))+
    scale_y_continuous(breaks = c(0,1), labels = c("Skip", "Stay"))+
    labs(title = paste0("ParticipantID: ", data2plot$IDVec[1], ", RunID: ", data2plot$runID[1], ", ", currentCat))+
    xlab("Delay")+
    ylab("Choice")+
    theme_classic()+
    theme(plot.title = element_text(size = 6))
}

###do not change anything below this point###

#creating empty lists for storing the plots and data 
plotRunsList <-  list()
plotParticipantsList <- list()
datOut <- list()

#get data processed using the python COMPAS processing script. Each file is a new participant
files <- list.files()
files <- files[grepl("_WS_data.csv", files)] #get processed files

for(p in 1:length(files)){ #participant loop starts here
  dat <- read.csv(paste0(dataPath, files[p])) #loop over each participant and read their data 
  dat <- dat[!is.na(dat$Category),] #I believe I have fixed issue where some trials do not extract category but check this
  dat <- dat[-11] #Don't need this column 
  dat$theta <- NA
  dat$trial <- NA
  
  # Loop through experimental sessions
  plotRunsList <- list() #empty list for each participant loop
  runNumbers <- levels(as.factor(dat$runID)) #get the current participant's session numbers, used for loop below
  for(run in 1:length(runNumbers)){ #runID loop starts here
    #loop over each run completed by the current participant and grab the data
    datRun <- dat[dat$runID == runNumbers[run],]
    cats <- levels(as.factor(datRun$Category))
    
    Thetas <- list() #empty list for current un's decision thresholds
    ########################################################################################################################
    ########################################################################################################################
    # Loop through categories here
    for(cat in 1:length(cats)){ #looping over each category
      datCat <- datRun[datRun$Category == cats[cat],] #get data for first category
      
      ########################################################################################################################
      ########################################################################################################################
      ### Create vectors of heaviside thresholds using expected value ###
      
      Theta <- matrix(NA, nrow = length(na.omit(datCat$Delays)), ncol = 1) #initialise vector of Theta for category in this participant's run 
      #looping over trials in current run to calculate decision threshold using an iterative heaviside
      for (Trial in 1:length(na.omit(datCat$Delays))){ 
        x_Cat <- na.omit(datCat$Delays)[-Trial] # all the animal delays a given subject encountered (not everyone encounters all delays), minus the current trial
        x_Cat <- sort(x_Cat, decreasing = T)
        uniqueX_Cat <- sort(unique(x_Cat), decreasing = T) # all unique x values, sorted ascending
        dX_Cat <- diff(uniqueX_Cat) # difference between consecutive x values
        dX_Cat <- c(dX_Cat, dX_Cat[1]) # to make dX as big as x
        Cat <- cbind(na.omit(datCat$Delays)[-Trial], datCat$Choices[-Trial])
        Cat <- Cat[order(Cat[,1],  decreasing = T),]
        y_Cat <- Cat[,2] # actual choices sorted by delay
        potentialThetas_Cat <- sort(unique(c(uniqueX_Cat, uniqueX_Cat-dX_Cat/2, uniqueX_Cat+dX_Cat/2)), decreasing = T); # list of potential values for theta: unique x's, all the in-betweens, the half-distance below the lowest x, and the half-distance above the largest x.
        potentialThetas_Cat <- 1:30
        SSresid_Cat <- matrix(NA, length(potentialThetas_Cat)) # set up vector of residuals
        yhat_Cat <- {}
        
        for (iTheta_Cat in 1:length(potentialThetas_Cat)){ # for each potential theta
          theta_Cat <- potentialThetas_Cat[iTheta_Cat]
          yhat_Cat[x_Cat<theta_Cat] <- 1 # predicted stay/go will be 1 where delay is below potential threshold
          yhat_Cat[x_Cat>=theta_Cat] <- 0 # 0 where delay is equal or above threshold
          yhat_Cat[x_Cat==theta_Cat] <- 0.5; # predicted stay/go will be 0.5 where x is exactly equal to theta
          dev_Cat <- y_Cat - yhat_Cat; # deviation of observed from predicted
          SSresid_Cat[iTheta_Cat,] <- dev_Cat%*%as.matrix(dev_Cat, ncol = 1) # this is a vector trick for getting the sum of squares. The first term is a horizontal list, the second is a vertical list. Because of the way vector multiplication works, the result is the same as sum(dev.^2), but much faster.
        }
        
        idMin_Cat <- which(SSresid_Cat == min(SSresid_Cat), arr.ind=TRUE)
        Theta[Trial] = potentialThetas_Cat[idMin_Cat[[1]]]; # the best-fitting value of theta is that which gave the minimum SSresid.
      }  #end trial loop
      
      #set colour for each category and plot
      if(datCat$Category[1] == "Animal"){
        color2use <- colorPal[1]
        plotAnimal <- doHWplot(datCat, mean(Theta))
      }else if(datCat$Category[1] == "Dance"){
        color2use <- colorPal[2]
        plotDance <- doHWplot(datCat, mean(Theta))
      }else if(datCat$Category[1] == "Fail"){
        color2use <- colorPal[3]
        plotFail <- doHWplot(datCat, mean(Theta))
      }else if(datCat$Category[1] == "Landscape"){
        color2use <- colorPal[4]
        plotLandscape <- doHWplot(datCat, mean(Theta))
      }
      
      dat$theta[dat$Category == cats[cat] & dat$runID == as.numeric(runNumbers[run])] <- mean(Theta) #adding threshold to data frame
    } #end category loop
    
    plotRun <- (plotAnimal + plotDance)/(plotFail + plotLandscape) #Combine plots into one
    
    dat$offerValue <- dat$theta - dat$Delays #Calculating value of each offer relative to decision threshold
    dat$trial[dat$runID == runNumbers[run]] <- 1:nrow(dat[dat$runID == runNumbers[run],])
    dat$runNum[dat$runID == runNumbers[run]] <- run
    #saving plots for each individual run 
    if(savePlots == 1){ #save plots if needed - using value set at top of script
      pdf(paste0(savePath, "individualRuns/ParticipantID-", datRun$IDVec[1], "_RunID-", datRun$runID[1], ".pdf"),
          width = 6, height = 6)
      invisible(print(plotRun))
      dev.off()
    }
    
    plotRunsList[[run]] <- plotRun
  }#end run loop
  
  #saving plots for each individual participant
  if(savePlots == 1){ #save plots if needed - using value set at top of script
    pdf(paste0(savePath, "individualParticipants/ParticipantID-", dat$IDVec[1], "_AllRuns", ".pdf"),
        width = 6, height = 6)
    invisible(lapply(plotRunsList, print))
    dev.off()
  }
  
  plotParticipantsList[[p]] <- plotRunsList
  datOut[[p]] <- dat
} #End participant loop

#save all plots
if(savePlots == 1){ #save plots if needed - using value set at top of script
  pdf(paste0(savePath, "AllParticipants", ".pdf"),
      width = 6, height = 6)
  invisible(lapply(plotParticipantsList, print))
  dev.off()
}

################ Saving Data ########################################################################
#Data from all participants - we have thetas now 
datOut <- do.call(rbind, datOut)

#normalise decision RTs
datOut$logRT <- log(datOut$ChoiceRTs)

setwd(dataPath)
saveFile <- paste0(dataPath, "allDataProcessed.xlsx")
write.xlsx(datOut, saveFile)

#get each participant's theta values and remove repeated rows
datShort <- datOut %>% distinct(Category, runID, .keep_all = TRUE) 
datShort <- subset(datShort, select = c(IDVec, runID, runNum, Category, theta, CatRank, AvgRatings))
saveFile <- paste0(dataPath, "thetas.xlsx")
write.xlsx(datShort, saveFile)


