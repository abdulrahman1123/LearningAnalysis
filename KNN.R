library(class)

df = iris
perc = 0.9
DV = "Species"
IVs = c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")

show_knn = function(df, DV, IVs, perc){
  nor <-function(x) {(x -min(x))/(max(x)-min(x))}
  
  data_norm <- as.data.frame(lapply(df[,IVs], nor))
  
  train_inds = sample(1:nrow(df), size = perc*nrow(df))
  iris_train = data_norm[train_inds,] 
  iris_test = data_norm[-train_inds,] 
  
  ##extract 5th column of train dataset because it will be used as 'cl' argument in knn function.
  train_category = df[train_inds,DV]
  
  test_category <- df[-train_inds,DV]
  
  predicted = knn(iris_train,iris_test,cl=train_category,k=13)
  
  tab = table(predicted,test_category)
  Accuracy = mean(pr == test_category)
  
  return(list(tab, Accuracy))
}
