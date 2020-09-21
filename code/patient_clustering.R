suppressPackageStartupMessages({
  library(logging)
  library(tidyverse)
  library(factoextra)
  library(cluster)
  library("scatterplot3d")
  library(car)
  library(rgl)
  library(FactoMineR)
  library(corrplot)
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

Part2SegmentationBy30Symptoms <- function(){
  df_A = LoadData(paste(DATA_PATH,"/Study_A.csv", sep=""))
  df_B = LoadData(paste(DATA_PATH,"/Study_B.csv", sep=""))
  df_C = LoadData(paste(DATA_PATH,"/Study_C.csv", sep=""))
  df_D = LoadData(paste(DATA_PATH,"/Study_D.csv", sep=""))
  df_E = LoadData(paste(DATA_PATH,"/Study_E.csv", sep=""))
  df = rbind(df_A, df_B, df_C, df_D)
  
  # Select only readings with day of visit 0
  df = filter(df, VisitDay == 0)
  
  # Select the useful columns
  df = subset(df, select = -c(Study, Country, SiteID, RaterID, AssessmentID, TxGroup, LeadStatus, VisitDay, PANSS_Total))
  
  # Remove duplicate rows with the same PatientID (multiple assessments on same day)
  df = df[!duplicated(df$PatientID), ]
  rownames(df) <- df$PatientID
  df$PatientID <- NULL
  df = scale(df)
  
  # Find optimum k with Elbow Method
  km.elbow = fviz_nbclust(df, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2)
  print(km.elbow)
  
  # Find optimum k with Silhouette Method
  km.silh = fviz_nbclust(df, kmeans, method = "silhouette")
  print(km.silh)
  
  # Use k = 2. Iterate
  opt.km_result = kmeans(df, 2, nstart = 25)
  
  min_wss = as.numeric(unlist(opt.km_result[5]))
  print(typeof(min_wss))
  for(i in 1:20){
    curr.km_result = kmeans(df, 2, nstart = 25)
    curr_wss = as.numeric(unlist(curr.km_result[5]))
    if(curr_wss < min_wss){
      opt.km_result = curr.km_result
      min_wss = curr_wss
    }
  }
  print(opt.km_result)

  fviz_cluster(opt.km_result, data = df)
}

Part2SegmentationBy3Categories <- function(){
  df_A = LoadData(paste(DATA_PATH,"/Study_A.csv", sep=""))
  df_B = LoadData(paste(DATA_PATH,"/Study_B.csv", sep=""))
  df_C = LoadData(paste(DATA_PATH,"/Study_C.csv", sep=""))
  df_D = LoadData(paste(DATA_PATH,"/Study_D.csv", sep=""))
  df_E = LoadData(paste(DATA_PATH,"/Study_E.csv", sep=""))
  df = rbind(df_A, df_B, df_C, df_D, df_E)
  
  # Select only readings with day of visit 0
  df = filter(df, VisitDay == 0)
  
  # Obtain columns based on sum of symptoms in each category
  P_Total <- df$P1+df$P2+df$P3+df$P4+df$P5+df$P6+df$P7
  N_Total <- df$N1+df$N2+df$N3+df$N4+df$N5+df$N6+df$N7
  G_Total <- df$G1+df$G2+df$G3+df$G4+df$G5+df$G6+df$G7+df$G8+df$G9+df$G10+df$G11+df$G12+df$G13+df$G15+df$G16
  
  df = subset(df, select = c(PatientID))
  
  symptom_df = data.frame(P_Total, N_Total, G_Total)
  df = cbind(df, symptom_df)
              
  # Remove duplicate rows with the same PatientID (multiple assessments on same day)
  df = df[!duplicated(df$PatientID), ]
  rownames(df) <- df$PatientID
  df$PatientID <- NULL
  # df = data.frame(scale(df))
  
  # Plot out 3d scatterplot
  scatterplot3d(x=df$P_Total, y=df$N_Total, z=df$G_Total, xlab="P Total", ylab="N Total", zlab="G Total", color="deepskyblue4")
  
  # Find optimum k with Elbow Method
  km.elbow = fviz_nbclust(df, kmeans, method = "wss") + geom_vline(xintercept = 5, linetype = 2)
  print(km.elbow)
  
  # Use k = 5 clusters
  opt.km_result = kmeans(df, 5, nstart = 25)
  min_wss = as.numeric(unlist(opt.km_result[5]))
  
  for(i in 1:20){
    curr.km_result = kmeans(df, 5, nstart = 25)
    curr_wss = as.numeric(unlist(curr.km_result[5]))
    if(curr_wss < min_wss){
      opt.km_result = curr.km_result
      min_wss = curr_wss
    }
  }
  
  df$cluster = factor(opt.km_result$cluster)
  scatterplot3d(x=df$P_Total, y=df$N_Total, z=df$G_Total, xlab="P Total", ylab="N Total", zlab="G Total", 
                color=rainbow(5)[opt.km_result$cluster])
  
  scatter3d(x=df$P_Total, y=df$N_Total, z=df$G_Total, surface=FALSE, point.col=rainbow(5)[opt.km_result$cluster],
            xlab="P Total", ylab="N Total", zlab="G Total")
  
  print(opt.km_result[2])
}

Part2SegmentationByPCA <- function(){
  df_A = LoadData(paste(DATA_PATH,"/Study_A.csv", sep=""))
  df_B = LoadData(paste(DATA_PATH,"/Study_B.csv", sep=""))
  df_C = LoadData(paste(DATA_PATH,"/Study_C.csv", sep=""))
  df_D = LoadData(paste(DATA_PATH,"/Study_D.csv", sep=""))
  df_E = LoadData(paste(DATA_PATH,"/Study_E.csv", sep=""))
  df = rbind(df_A, df_B, df_C, df_D)
  
  # Select only readings with day of visit 0
  df = filter(df, VisitDay == 0)
  
  # Select the columns with the symptoms
  df = subset(df, select = -c(Study, Country, SiteID, RaterID, AssessmentID, TxGroup, LeadStatus, VisitDay, PANSS_Total))
  
  # Remove duplicate rows with the same PatientID (multiple assessments on same day)
  df = df[!duplicated(df$PatientID), ]
  rownames(df) <- df$PatientID
  df$PatientID <- NULL
  df = data.frame(scale(df))
  
  # Plot the correlation matrix
  corr_mtx = cor(df)
  corrplot(corr_mtx, type = "upper", order = "hclust", 
           tl.col = "black", tl.srt = 45)
  
  # Obtain Principle Components
  pc <- princomp(df)
  plot(pc, col="dodgerblue3")
  print(summary(pc))
  
  # Choose the number of PC that reaches 60% of total variance
  pc <- prcomp(df)
  var_list = summary(pc)$importance[2,]
  print(var_list)
  pc_length = 0
  cur_var_prop = 0
  for(i in 1:length(var_list)){
    cur_var_prop = cur_var_prop + var_list[i]
    if(cur_var_prop >= 0.60){
      pc_length = i
      break
    }
  }
  print(pc_length)
  
  # Select the first i principle components to represent more than 60% variance
  sel_comp = data.frame(pc$x[,1:i])
  plot(sel_comp)
  
  # Find optimum k with Elbow Method
  km.elbow = fviz_nbclust(sel_comp, kmeans, method = "wss") + geom_vline(xintercept = 5, linetype = 2)
  print(km.elbow)
  
  # use k = 5 clusters
  km_result = kmeans(df, 5, nstart = 25) 
  plot(sel_comp, col=km_result$clust)
  
  # Split into separate dataframes corresponding to cluster
  cluster_df = data.frame(km_result$cluster)
  cl1_df = filter(cluster_df, km_result.cluster == 1)
  cl2_df = filter(cluster_df, km_result.cluster == 2)
  cl3_df = filter(cluster_df, km_result.cluster == 3)
  cl4_df = filter(cluster_df, km_result.cluster == 4)
  cl5_df = filter(cluster_df, km_result.cluster == 5)
  
  # Filter into main dataframe to obtain the symptom scores
  df_1 <- subset(df, rownames(df) %in% rownames(cl1_df))
  df_2 <- subset(df, rownames(df) %in% rownames(cl2_df))
  df_3 <- subset(df, rownames(df) %in% rownames(cl3_df))
  df_4 <- subset(df, rownames(df) %in% rownames(cl4_df))
  df_5 <- subset(df, rownames(df) %in% rownames(cl5_df))
  
  # Obtain the means of all symptom scores in each cluster
  res.means_df = rbind(sapply(df_1, mean), sapply(df_2, mean), sapply(df_3, mean),
                 sapply(df_4, mean), sapply(df_5, mean))
  rownames(res.means_df) <- c("C1 Mean","C2 Mean","C3 Mean","C4 Mean","C5 Mean")
  print(res.means_df)
}

  
