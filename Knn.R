#----metoda kNN----

head(iris)

# zrób wykresy rozrzutu wybranych par zmiennych numerycznych, serie względem zmiennej Species

plot(data=iris,Sepal.Length~Petal.Length,col=Species)
plot(data=iris,Sepal.Width~Petal.Width,col=Species)
plot(data=iris,Petal.Length~Petal.Width,col=Species)
plot(data=iris,Sepal.Length~Sepal.Width,col=Species)

# model kNN
library(caret)
model_kNN <- knn3(x = iris[,-5], # macierz zmiennych objaśniających
                  y = iris[,5],  # zmienna celu
                  k=3            # liczba sąsiadów
                  )


predict(model_kNN, iris[,-5], type = "class")
predict(model_kNN, iris[,-5], type = "prob")



# jaki jest odsetek trafnych przewidywań ogółem i w podziale na gatunki? 



# macierz pomyłek
tab=table(predicted=predict(model_kNN, iris[,-5], type = "class"),true=iris$Species)
tab

# accuracy
sum(diag(tab))/nrow(iris)



# znajdź k, które maksymalizuje accuracy





# stwórz wykres accuracy w zależności od k





# odpowiedź

k=50
acckNN=rep(0,k)
for(i in 1:k)
{
  model_kNN <- knn3(x = iris[,-5],y = iris[,5],k=i)
  tab <- table(predicted=predict(model_kNN, iris[,-5], type = "class"),true=iris$Species)
  acckNN[i] <- sum(diag(tab))/nrow(iris)
}  

acckNN
plot(y = acckNN,x=1:k,type="b",xlab="k")



# kNN z wykorzystaniem funkcji train
set.seed(1)
names(getModelInfo())
modelLookup('knn')

model_kNN <- train(x = iris[,-5],
                   y = iris[,5],
                   method = "knn",
                   preProcess = NULL, # scale?
                   metric = "Accuracy",
                   tuneGrid = data.frame(k=1:149))

plot(model_kNN)
plot(model_kNN,metric = "Kappa") #Cohen's Kappa 

model_kNN$bestTune

predict(model_kNN, iris[,-5], type = "raw") # odpowiednik 'class' dla funkcji knn3
predict(model_kNN, iris[,-5], type = "prob")

# confusion matrix
tab=table(predicted=predict(model_kNN, iris[,-5], type = "raw"),true=iris$Species)
tab/nrow(iris)
sum(diag(tab))/nrow(iris)

# confusion matrix z pakietu caret
caret::confusionMatrix(data=predict(model_kNN, iris[,-5], type = "raw"),
                       reference=iris$Species)

caret::confusionMatrix(model_kNN)


# do którego gatunku zakwalifikowałbyś kwiat?
x=c(6.1,3.0,4.4,1.5)
x










# odpowiedź

x=data.frame(matrix(x,nrow = 1))
colnames(x)=colnames(iris[,-5])


predict(model_kNN,newdata = x, type = "prob")
predict(model_kNN,newdata = x, type = "raw")



