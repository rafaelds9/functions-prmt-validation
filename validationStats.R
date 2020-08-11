#' Generates 3 Q-Q plots (Gold Standard's, Alternative method's and the difference between the former and latter)
#' 
#'  @param my_data A Nx2 dataframe with the obs. of the gold standard and alternative method respectively
#'  @param goldStd A string corresponding to the Gold Standard's name
#'  @param altMethod A string corresponding to the alternative method's name
#'  @example 
#'  QQPlots(my_data, "Gold Standard", "Alternative Method")
#'  
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


#' Performs the Shapiro Wilk test using the difference between the gold standard and alternative method's values and prints the result.
#' 
#'  @param my_data A Nx2 dataframe with the obs. of the gold standard and alternative method respectively
#'  @example 
#'  shapiroWilk(my_data)
#'  
shapiroWilk <- function(my_data){
  
  dif<- my_data[[1]] - my_data[[2]]
  res<-shapiro.test(dif)
  print(res)
}


#' Calculates the correlation (spearman) between the gold standard and the alternative method, also plots the paired values against the identity line
#' 
#'  @param my_data A Nx2 dataframe with the obs. of the gold standard and alternative method respectively
#'  @param parameterName A string corresponding to the name of the parameter that is being compared.
#'  @param goldStd A string corresponding to the Gold Standard's name
#'  @param altMethod A string corresponding to the alternative method's name
#'  @return Spearman's correlation between the gold standard and the alternative method.
#'  @example 
#'  spearmanCor(my_data, "Parameter", "Gold Standard", "Alternative Method")
#'  
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


#' Performs the Student's t-test of the observations paired to check if they're equivalent and prints the result
#' 
#'  @param my_data A Nx2 dataframe with the obs. of the gold standard and alternative method respectively
#'  @param goldStd A string corresponding to the Gold Standard's name
#'  @param altMethod A string corresponding to the alternative method's name
#'  @example 
#'  pairedtTest(my_data, "Gold Standard", "Alternative Method")
#'  
pairedtTest <- function(my_data, goldStd, altMethod){

  result <- t.test(my_data[[1]], my_data[[2]], paired = TRUE, alternative = "two.sided")
  print(result)

}


#' Generates a Bland-Altman plot of the observations
#' 
#'  @param my_data A Nx2 dataframe with the obs. of the gold standard and alternative method respectively
#'  @param parameterName A string corresponding to the name of the parameter that is being compared.
#'  @param goldStd A string corresponding to the Gold Standard's name
#'  @param altMethod A string corresponding to the alternative method's name
#'  @example 
#'  blandAlt(my_data, "Parameter", "Gold Standard", "Alternative Method")
#'  
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


