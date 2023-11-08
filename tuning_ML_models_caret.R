# dostrajanie modelu
# model tuning

#################################
#Grid Search: Automatic Grid


# ensure results are repeatable
set.seed(7)
# load the library
library(caret)
# load the dataset
data(iris)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(Species~., data=iris, method="lvq", trControl=control, tuneLength=5)
# summarize the model
print(model)

#######################################
# ensure results are repeatable
set.seed(7)
# load the library
library(caret)
# load the dataset
data(iris)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# design the parameter tuning grid
grid <- expand.grid(size=c(5,10,20,50), k=c(1,2,3,4,5))
# train the model
model <- train(Species~., data=iris, method="lvq", trControl=control, tuneGrid=grid)
# summarize the model
print(model)


#########################################
#Data Pre-Processing
set.seed(7)
# load the library
library(caret)
# load the dataset
data(iris)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(Species~., data=iris, method="lvq", preProcess="scale", trControl=control, tuneLength=5)
# summarize the model
print(model)



###################################################

# ensure results are repeatable
set.seed(7)
# configure multicore
library(doMC)
registerDoMC(cores=4)
# load the library
library(caret)
# load the dataset
data(iris)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(Species~., data=iris, method="lvq", trControl=control, tuneLength=5)
# summarize the model
print(model)



########################################
#Visualization of Performance

# ensure results are repeatable
set.seed(7)
# load the library
library(caret)
# load the dataset
data(iris)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# design the parameter tuning grid
grid <- expand.grid(size=c(5,10,15,20,25,30,35,40,45,50), k=c(3,5))
# train the model
model <- train(Species~., data=iris, method="lvq", trControl=control, tuneGrid=grid)
# summarize the model
print(model)
# plot the effect of parameters on accuracy
plot(model)

