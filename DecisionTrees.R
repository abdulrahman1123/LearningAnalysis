library(DAAG)
library(party)

library(mlbench)
library(caret)
library(pROC)
library(tree)


library(dplyr)
library(rpart.plot)
library(rpart)

path <- 'https://raw.githubusercontent.com/guru99-edu/R-Programming/master/titanic_data.csv'
data_frame <-read.csv(path)
data_frame$sex = factor(data_frame$sex, levels = c("female","male"))
data_frame$age = as.numeric(data_frame$age)
data_frame$fare = as.numeric(data_frame$fare)
data_frame$embarked = factor(data_frame$embarked)
data_frame$pclass = factor(data_frame$pclass, levels = c(1, 2, 3), labels = c('Upper', 'Middle', 'Lower'))
data_frame$survived = factor(data_frame$survived, levels = c(0, 1), labels = c('No', 'Yes'))
data_frame = na.omit(data_frame)


show_decision_tree = function(data_frame, IVs, DV, perc = 0.8, dc_method = "class", min_split = 4,
                              min_bucket = 2, max_depth = 3, c_p = 0, use_control = FALSE){
  #hyperparameters
  control = rpart.control(minsplit = min_split, # min number of observations before the algorith splits
                          minbucket = min_bucket, # min number of observations in the final node
                          maxdepth = max_depth, # max depth of a node (with the root being node 0)
                          cp = c_p)
  
  
  
  data_frame = data_frame[,c(IVs,DV)]
  
  train_inds = sample(1:nrow(data_frame), size = perc*nrow(data_frame))
  train = data_frame[train_inds,]
  test = data_frame[-train_inds,]
  
  
  if (use_control){
    fit = rpart(survived~., data = train, method = dc_method, control = control) # you can use class for classification, and anova for regression
  } else {
    fit = rpart(survived~., data = train, method = dc_method)
  }
  
  
  rpart.plot(fit, extra = "auto") # extra is for extra information, refer to: https://cran.r-project.org/web/packages/rpart.plot/rpart.plot.pdf
  
  # make prediction
  predict_unseen = predict(fit, test, type = "class")
  pred_table = table(predicted = predict_unseen, actual = test[[DV]])
  Accuracy = mean(predict_unseen == test[[DV]])
  
  return(list(pred_table, Accuracy))
  
}

show_decision_tree(data_frame = data_frame, IV = c("pclass", "sex", "age", "sibsp", "parch", "fare", "embarked"), DV= "survived",
                   perc = 0.8, dc_method = "class", min_split = 4, 
                   min_bucket = 2, max_depth = 3, c_p = 0, use_control = TRUE)
