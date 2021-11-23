###### Function handles analysis of decision reaction times. Data should be processed using the 
### python COMPAS processing script and WS_DecisionThresholds.R script.
### The data frame being analysed should have RTs normalised before running the function
### The script also needs to use the fitGaus.R function to analyse rule-based decision making

#Required packages
require(xlsx)
require(ggplot2)
require(Metrics)
require(patchwork)
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/fitGaus.R"))

WS_RT <- function(dat2use, splitHalf){ #provide the normalised data and whether you want to split the session into 1st and 2nd
  #half for analysis (0 = don't split, 1 = split data)
  if(exists("splitHalf") == FALSE){
    splitHalf <- 0
  }
  
  #Plot GAM fit 
  if(splitHalf == 0){
    plotGAM <- ggplot(data = dat2use, aes(x = offerValue, y = logRT))+
      geom_smooth(method = gam, formula = y ~ s(x))+
      xlab("Offer value")+
      ylab("Log decision reaction time")+
      theme_classic()
  } else if(splitHalf == 1){
    halfLoop <- floor((max(dat2use[[p]]$LoopN))/2) #Finds half-way point (number of times all categories have been repeated, divided by two, rounding down)
    dat2use[[p]]$half <- ifelse(dat2use[[p]]$LoopN <= halfLoop, 1, 2)
    plotGAM <- ggplot(data = dat2use, aes(x = offerValue, y = logRT))+
      geom_smooth(method = gam, formula = y ~ s(x, bs = "cr"))+
      facet_wrap(~half)+
      xlab("Offer value")+
      ylab("Log decision reaction time")+
      theme_classic()
  } else {
    break
  }
  
  datGaus <- fitGaus(dat2use)$tab
  plotGaus <- ggplot(data = dat2use, aes(x = offerValue, y = logRT))+
    annotate("rect", xmin = -30, xmax = 0, ymin = -Inf, ymax = Inf,
             alpha = .1, fill = "red")+
    annotate("rect", xmin = 0, xmax = 30, ymin = -Inf, ymax = Inf,
             alpha = .1, fill = "green")+
    geom_smooth(method = gam, formula = y ~ s(x, bs = "cr"))+
    geom_line(data = datGaus, aes(x = x, y = gausRT),  linetype = 2, color = "red")+
    coord_cartesian(ylim = c(6.5, 7.8))+
    labs(title = "1st session")+
    theme_classic()+
    theme(axis.title.x = element_blank(),)
  
  ## Estimate emergence of rule-based behaviour
  fitTab <- fitGaus2(dat2use)$tab
  tolRT <- fitTab$gausRT 
  sortRT <- dat2use[order(dat2use$offerValue),]
  if(any(is.finite(sortRT$logRT) == FALSE)){
    rmRow <- which(is.finite(sortRT$logRT) == FALSE)
    sortRT <- sortRT[-rmRow,]
  }
  
  sortRT$expRT <- tolRT
  sortRT$metThreshold <- ifelse(sortRT$logRT < sortRT$expRT, 1, 0)
  
  unsortRT <- sortRT[order(as.numeric(as.character(sortRT$NA.))),]
  unsortRT$offerGroup <- ifelse(unsortRT$offerValue < 1, "bad_offer", "good_offer")
  unsortRT$diff <- unsortRT$logRT - unsortRT$expRT
  
  
  plotDiff <- ggplot(unsortRT, aes(x = trial, y = diff)) +
    geom_point() +
    geom_smooth(method = gam, formula = y ~ s(x, bs = "cr"))+
    facet_wrap(~offerGroup)+
    geom_hline(yintercept = 0, color = "red")+
    ylab("Reaction time difference (observed - optimal)")+
    theme_classic()
  
  Names <- c("plotGam", "plotGaus", "plotDiff", "RT_Diffs")
  
  RT_Out <- list(plotGam,
                 plotGaus,
                 plotDiff,
                 unsortRT)
  names(RT_Out) <- Names
  
  return(RT_Out)
  
}
