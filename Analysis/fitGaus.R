###### Function fits a Gausian-like curve to GAM model fitted to Websurf decision RT data. 
### Low error between "optimal" Gausian curve and observed data indicates stronger rule-based decision making

require(mgcv)
require(scales)

fitGaus2 <- function(dat2use){
  
  #dat2use <- dat2use[order(dat2use$offerValue),]
  if(any(is.finite(dat2use$logRT) == FALSE)){
    rmRow <- which(is.finite(dat2use$logRT) == FALSE)
    dat2use <- dat2use[-rmRow,]
  }
  model <- gam(logRT ~ s(offerValue, bs = "cs"), data = dat2use)
  fitting <- predict(model, se = T)
  gamFit <- data.frame(fit = fitting$fit, se = fitting$se.fit, logRT = dat2use$logRT, offerValue = dat2use$offerValue)
  
  # plot.fit <- ggplot(data = gamFit, aes(x = offerValue, y = fit)) +
  #   geom_line() +
  #   geom_ribbon(aes(ymin = fit - se, ymax = fit + se), alpha = 0.5)+
  #   theme_classic()
  # plot.fit
  
  thetaInd <- which(abs(gamFit$offerValue - 0)==min(abs(gamFit$offerValue-0)))[1]
  thetaRT <- gamFit$fit[thetaInd]
  
  gamFit <- gamFit[order(gamFit$offerValue),]
  gaus <- dnorm(gamFit$offerValue, mean = 0, sd = 10)
  gaus <- rescale(gaus, to = c(min(fitting$fit), thetaRT))
  
  gausTab <- as.data.frame(cbind(x = gamFit$offerValue, logRT = gamFit$fit, se = gamFit$se, gausRT = gaus))
  
  plot <- ggplot(data = gausTab, aes(x = x, y = logRT))+
    annotate("rect", xmin = -30, xmax = 0, ymin = -Inf, ymax = Inf,
             alpha = .1, fill = "red")+
    annotate("rect", xmin = 0, xmax = 30, ymin = -Inf, ymax = Inf,
             alpha = .1, fill = "green")+
    scale_x_continuous(name = "Offer value", limits = c(-30, 30), breaks = c(-30, 0, 30))+
    ylab("Log-normalised reaction time")+
    geom_line(size = 2)+
    geom_line(y = gausTab$gausRT, color = "red", size = 1.5, linetype = 2)+
    theme_classic()
  

  Names <- c("tab", "thetaInd", "thetaRT", "plot")
  
  fitOut <- list(gausTab,
                 thetaInd,
                 thetaRT,
                 plot)
  names(fitOut) <- Names
  
  return(fitOut)
  
} 

