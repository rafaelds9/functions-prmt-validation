QQPlots <- function(my_data, goldStd, altMethod){

  #Q-Q plot of the Gold Standard's measurements
  qqnorm(my_data[[1]], pch = 18, col ='Blue', frame = TRUE, ylab = paste("Sample Quantities: ", goldStd))
  qqline(my_data[[1]], col = "black", lwd = 2)
  
  #Q-Q plot of the Alternative Method's measurements
  qqnorm(my_data[[2]], pch = 18, col ='Blue', frame = TRUE, ylab = paste("Sample Quantities: ", altMethod))
  qqline(my_data[[2]], col = "black", lwd = 2)
  
  #Q-Q plot of the (Gold Standard - Alternative Method) measurements
  qqnorm(my_data[[1]] - my_data[[2]], pch = 18, col ='Blue', frame = TRUE, ylab = paste("Sample Quantities: ", goldStd, " - ",altMethod))
  qqline(my_data[[1]] - my_data[[2]], col = "black", lwd = 2)
  
}



shapiroWilk <- function(my_data){
  
  dif<- my_data[[1]] - my_data[[2]]
  res<-shapiro.test(dif)
  print(res)
}



spearmanCor <- function(my_data, parameterName, goldStd, altMethod){
  
  correlation <- cor(my_data[1], my_data[2], method = "spearman")
  correlation <- round(correlation,2)
  
  #plots the paired data
  plot(my_data[[1]], my_data[[2]], xlab = paste(parameterName, goldStd), ylab = paste(parameterName, altMethod), pch=18, col = "Blue") 
  
  #plots the identity line
  #a = intercept, b=slope
  abline(a=0, b=1)
  
  rhoText <- paste('Spearman Rho = ', toString(correlation))
  
  #Inserts Spearman's rho on the plot
  text((par("usr")[[2]]+par("usr")[[1]])/2, par("usr")[[1]]+1, rhoText, col = 'Red')
  
  correlation
  
}



pairedtTest <- function(my_data, goldStd, altMethod){

  result <- t.test(my_data[[1]], my_data[[2]], paired = TRUE, alternative = "two.sided")
  print(result)

}



blandAlt <- function(my_data, parameterName, goldStd, altMethod){

  library(BlandAltmanLeh)
  baParameters <-bland.altman.plot(my_data[[1]], my_data[[2]], graph.sys = "base",
                 xlab = paste(parameterName, "Means [", parameterName, goldStd, "&", altMethod, "]"), 
                 ylab = paste(parameterName, "Differences [", parameterName, goldStd, "&", altMethod, "]"), 
                 conf.int = 0.95, silent = FALSE, sunflower = TRUE)

  #Prints bias
  text((par("usr")[[2]]+par("usr")[[1]])/2,baParameters$mean.diffs+0.08,paste('Bias = ', toString(round(baParameters$mean.diffs,2))), col='red')
  
  #Prints the text over the lines (MEAN + 1.96SD)
  text((par("usr")[[2]]+par("usr")[[1]])/2,baParameters$lines[3]+0.1,'Mean + 1.96SD', col='red')
  
  #Prints the text over the lines (MEAN - 1.96SD)
  text((par("usr")[[2]]+par("usr")[[1]])/2,baParameters$lines[1]+0.1,'Mean - 1.96SD', col='red')

}


