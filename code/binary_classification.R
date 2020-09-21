suppressPackageStartupMessages({
  library(logging)
  library(tree)
  library(gbm)
  library(glmnet)
  library(randomForest)
  library(class)
  library(data.table)
  library(mltools)
  library(MLmetrics)
})

FIGSIZE = c(1000, 1000)

DATA_PATH = "..."

LoadData <- function(data_path, verbose=TRUE) {
  stopifnot(file.exists(data_path))
  data <- read.csv(data_path)
  return (data)
}

Part4 <- function(){
  df_A = LoadData(paste(DATA_PATH,"/Study_A.csv", sep=""))
  df_B = LoadData(paste(DATA_PATH,"/Study_B.csv", sep=""))
  df_C = LoadData(paste(DATA_PATH,"/Study_C.csv", sep=""))
  df_D = LoadData(paste(DATA_PATH,"/Study_D.csv", sep=""))
  df_E = LoadData(paste(DATA_PATH,"/Study_E.csv", sep=""))
  df = rbind(df_A, df_B, df_C, df_D)
  
  # Set Flagged or CS as 1, Passed to 0
  df$LeadStatus[df$LeadStatus == "Assign to CS"] = 1
  df$LeadStatus[df$LeadStatus == "Flagged"] = 1
  df$LeadStatus[df$LeadStatus == "Passed"] = 0
  df = subset(df, select=-c(Study, PatientID, Country, SiteID, RaterID, AssessmentID))
  df$TxGroup = as.factor(df$TxGroup)
  df_E = subset(df_E, select=-c(Study, PatientID, SiteID, Country, RaterID, AssessmentID))
  df_E$TxGroup = as.factor(df_E$TxGroup)
  df_E$LeadStatus = replicate(nrow(df_E), -1)

  smp_size = 0.8*nrow(df)
  train_index = sample(nrow(df), smp_size)
  
  data.train = df[train_index, ]
  data.test = df[-train_index, ]
  
  # Fit using Boosting
  boost_fit = gbm(LeadStatus ~., data=data.train, cv.folds=10, bag.fraction = 0.75, distribution="bernoulli", n.trees=1000, shrinkage=0.05)
  boost_prob = predict(boost_fit, data.test, n.trees = 1000, type = "response")
  boost_pred = ifelse(boost_prob > 0.5, 1, 0)
  print(table(data.test$LeadStatus, boost_pred))
  boost_acc = sum(data.test$LeadStatus == boost_pred)/nrow(data.test)
  print(paste("Boost accuracy: ", boost_acc))
  
  # Fit using Logistic Regression
  log_fit = glm(as.factor(LeadStatus) ~., data=data.train, family = binomial)
  log_prob = predict(log_fit, data.test, type="response")
  log_pred = ifelse(log_prob > 0.5, 1, 0)
  print(table(data.test$LeadStatus, log_pred))
  log_acc = sum(data.test$LeadStatus == log_pred)/nrow(data.test)
  print(paste("Logistic Regression accuracy: ", log_acc))
  
  # Plots the test accuracy against mtry for Random Forest
  num_pred = ncol(data.train)-1
  test_accuracy = 1:num_pred
  for(i in 1:num_pred){
    print(paste("iteration: ", i, "/ ", num_pred))
    rf_fit = randomForest(as.factor(LeadStatus) ~., data=data.train, mtry = i, ntree=1000)
    rf_prob = predict(rf_fit, data.test, ntree=1000, type="prob")
    rf_prob = rf_prob[,2]
    rf_pred = ifelse(rf_prob > 0.5, 1, 0)
    rf_acc = sum(data.test$LeadStatus == rf_pred)/nrow(data.test)
    print(paste("rf accuracy: ", rf_acc))
    test_accuracy[i] = rf_acc
  }
  plot(1:num_pred, test_accuracy, type="b", xlab="mtry", ylab="Test Accuracy")
  
  # Fit using Random Forest
  rf_fit = randomForest(as.factor(LeadStatus) ~., data=data.train, ntree=1000, max.depth=5, min.node.size=10, splitrule=Gini,
                           replace=TRUE, sampsize=nrow(data.train), importance=TRUE, mtry=7)
  rf_prob = predict(rf_fit, data.test, ntree=1000, type="prob")
  rf_prob = rf_prob[,2]
  rf_pred = ifelse(rf_prob > 0.5, 1, 0)
  rf_acc = sum(data.test$LeadStatus == rf_pred)/nrow(data.test)
  print(paste("Random Forest accuracy: ", rf_acc))

  # Boosting Final Fit on study E
  fin_boost_fit = gbm(LeadStatus ~., data=df, cv.folds=10, bag.fraction = 0.75, distribution="bernoulli", n.trees=1000, shrinkage=0.05)
  fin_boost_prob = predict(fin_boost_fit, df_E, n.trees = 1000, type = "response") # type response to output probabilities
  write.csv(fin_boost_prob, file = paste(DATA_PATH, "/Part 4/boost_result.csv", sep=""), row.names=FALSE)

  # Random Forest Final Fit on study E
  fin_rf_fit = randomForest(as.factor(LeadStatus) ~., data=df, ntree=1000, max.depth=5, min.node.size=10, splitrule=Gini,
                            replace=TRUE, sampsize=nrow(data.train), importance=TRUE)
  fin_rf_prob = predict(fin_rf_fit, df_E, ntree=1000, type="prob")
  fin_rf_prob = fin_rf_prob[,2]
  write.csv(fin_rf_prob, file = paste(DATA_PATH, "/Part 4/rf_result.csv", sep=""), row.names=FALSE)
}


