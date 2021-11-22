library(e1071)
library(ggplot2)
library(sf)
make_grid = function(x,n = 75){  
  ## This function only creates a range of dots
  # These dots will be colored according to the predicted value based on our data
  x1 = seq(from = min(x[,1])-0.5, to = max(x[,1])+0.5, length = n)
  x2 = seq(from = min(x[,2])-0.5, to = max(x[,2])+0.5, length = n)
  
  new_df = expand.grid(X1 = x1, X2 = x2)
  colnames(new_df) = colnames(x)[1:2]
  
  return(new_df)
}

convert_to_sf = function(new_df){
  # The next part converts the grid data frame to a group of polygons of class sf
  pre_raster_df = new_df
  pre_raster_df = cbind(pre_raster_df, cat = rep(1L, nrow(pre_raster_df)),stringsAsFactors = FALSE)
  
  # formula coordinates will be based on the colnames of the data input (e.g. ~X1+X2)
  cor_form = as.formula(paste0("~",colnames(new_df)[1],"+", colnames(new_df)[2]))
  coordinates(pre_raster_df) = cor_form
  gridded(pre_raster_df) <- TRUE
  
  # create raster df
  raster_df <- raster(pre_raster_df)
  
  # create spatial polygons
  sp_df = rasterToPolygons(raster_df, dissolve = TRUE)
  
  # convert polygons of class sf
  sf_polygons = st_as_sf(sp_df)
  
  return(sf_polygons)
}

res = 75
m_type = "linear"

data_dir = "C:/Users/jsawa/Documents/GitHub/LearningAnalysis/data/iris.csv"
dat = read.csv(data_dir)
dat$Y = factor(dat$Y)

 
# create a test and train data sets

train_inds = sample(1:150, size = 0.8*nrow(dat))
train = dat[train_inds,]
test = dat[-train_inds,]

tt = train[,c(1,2,5)]

linear = svm(Y ~ X0+X1, data = tt, kernel = "linear", cost = 1, scale = TRUE)
radial = svm(Y ~ X0+X1, data = tt, kernel = "radial", cost = 1, scale = TRUE)
polynomial = svm(Y ~ X0+X1, data = tt, kernel = "polynomial", degree = 3, cost = 1, scale = TRUE)

if (m_type == "linear"){
  model = linear
} else if (m_type == "radial"){
  model = radial
} else if (m_type == "polynomial"){
  model = polynomial
}


grid = make_grid(train, n = res)
preds = predict(model, grid)
predicted_df = data.frame(X1 = grid[,1], X2 = grid[,2], Y=preds)
sf_polygons = convert_to_sf(predicted_df)


# plot the model
g_aft = ggplot()+
  geom_sf(data = sf_polygons, mapping= aes(x=NULL,y=NULL,group = Y,fill = Y), alpha = 0.25)+
  scale_fill_gradientn(breaks = 1:5,colors = c("#00A550","#Eb4C42","#0087BD"))+
  geom_point(data = train,mapping = aes(x = X0, y = X1,color = Y),size = 3)+
  scale_color_manual(values = c("#00A550","#Eb4C42","#0087BD"))+
  ggtitle("Data points of the variables X0 and X1", subtitle = "Original Data with Predicted Values")+
  MinimalTheme


g_aft
# predictability
preds_linear = predict(linear, test)
preds_radial = predict(radial, test)
preds_polynomial = predict(polynomial, test)
print(paste0("Accuracy of prediction for linear kernel is ",round(100*mean(preds_linear== test$Y),digits = 1),"%"))
print(paste0("Accuracy of prediction for radial kernel is ",round(100*mean(preds_radial== test$Y),digits = 1),"%"))
print(paste0("Accuracy of prediction for polynomial kernel is ",round(100*mean(preds_polynomial== test$Y),digits = 1),"%"))


