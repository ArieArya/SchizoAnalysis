suppressPackageStartupMessages({
  library(logging)
  library(tidyverse)
})

#' The width and height of exported plots.
FIGSIZE = c(1000, 1000)

DATA_PATH = "..."

LoadData <- function(data_path, verbose=TRUE) {
  stopifnot(file.exists(data_path))
  if (verbose)
    loginfo(sprintf("\tLoading %s", data_path))
  data <- read.csv(data_path)
  return (data)
}

Part1TreatmentGroup <- function(){
  df_A = LoadData(paste(DATA_PATH,"/Study_A.csv", sep=""))
  df_B = LoadData(paste(DATA_PATH,"/Study_B.csv", sep=""))
  df_C = LoadData(paste(DATA_PATH,"/Study_C.csv", sep=""))
  df_D = LoadData(paste(DATA_PATH,"/Study_D.csv", sep=""))
  df = rbind(df_A, df_B, df_C, df_D)
  
  # Select only the patients in the treatment group
  df = filter(df, TxGroup == "Treatment")
  
  # Select the useful columns
  df = df[c("VisitDay", "PANSS_Total")]
  
  # Print out the Scatterplot
  visitDay = df$VisitDay
  panssTotal = df$PANSS_Total
  png(paste(DATA_PATH, "/Part 1/Part1Treatment.png", sep=""), width=FIGSIZE[1], height=FIGSIZE[2])
  plot(x = visitDay, y = panssTotal, main="PANSS Total vs. Day for Treatment Group", xlab="Day of Visit", ylab="PANSS Total", col="dodgerblue2")
  
  # Obtain a Linear Fit 
  linear_fit = lm(panssTotal~visitDay)
  print(summary(linear_fit))
  abline(linear_fit, col="red")
  dev.off()
  
  # Print the gradient of the Regression Line
  print(coef(linear_fit))
}

Part1ControlGroup <- function(){
  df_A = LoadData(paste(DATA_PATH,"/Study_A.csv", sep=""))
  df_B = LoadData(paste(DATA_PATH,"/Study_B.csv", sep=""))
  df_C = LoadData(paste(DATA_PATH,"/Study_C.csv", sep=""))
  df_D = LoadData(paste(DATA_PATH,"/Study_D.csv", sep=""))
  df = rbind(df_A, df_B, df_C, df_D)
  print(head(df, 5))
  
  # Select only the patients in the control group
  df = filter(df, TxGroup == "Control")
  
  # Select the useful columns
  df = df[c("VisitDay", "PANSS_Total")]
  
  # Print out the Scatterplot
  visitDay = df$VisitDay
  panssTotal = df$PANSS_Total
  png(paste(DATA_PATH, "/Part 1/Part1Control.png", sep=""), width=FIGSIZE[1], height=FIGSIZE[2])
  plot(x = visitDay, y = panssTotal, main="PANSS Total vs. Day for Control Group", xlab="Day of Visit", ylab="PANSS Total", col="dodgerblue2")
  
  # Obtain a Linear Fit 
  linear_fit = lm(panssTotal~visitDay)
  print(summary(linear_fit))
  abline(linear_fit, col="red")
  dev.off()
  
  # Print the gradient of the Regression Line
  print(coef(linear_fit))
}

Part1TreatmentGroupCombinedPlots <- function(){
  df_A = LoadData(paste(DATA_PATH,"/Study_A.csv", sep=""))
  df_B = LoadData(paste(DATA_PATH,"/Study_B.csv", sep=""))
  df_C = LoadData(paste(DATA_PATH,"/Study_C.csv", sep=""))
  df_D = LoadData(paste(DATA_PATH,"/Study_D.csv", sep=""))
  df = rbind(df_A, df_B, df_C, df_D)
  
  # Select only the patients in the treatment group
  df = filter(df, TxGroup == "Treatment")
  
  # Select the corresponding columns
  visitDay = df$VisitDay
  
  # Loop over the 30 symptoms
  png(paste(DATA_PATH, "/Part 1/Part1TreatmentCombinedPlots.png", sep=""), width=FIGSIZE[1], height=FIGSIZE[2])
  par(mfrow=c(5, 6))
  for (i in 0:29){
    symptom_score = df[,9+i]
    plot(x = visitDay, y = symptom_score, xlab="Day of Visit", ylab=colnames(df[9+i]), col="dodgerblue2", las=1)
  }
  dev.off()
  
}
