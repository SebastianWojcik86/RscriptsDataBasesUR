library(dplyr)
sonar <- JOUSBoost::sonar
sonar$y <- recode(sonar$y,'1'=1,'-1'=0)
table(sonar$y)


#----pakiet gmb----
library(gbm)

sonargbm <- gbm(y~.,
                data=sonar,
                distribution = 'bernoulli',    # 'bernoulli' dla zmiennej binarnej 0-1
                                               # 'gaussian' albo 'laplace' dla zmiennej ciągłej
                                               # 'coxph' dla zmiennej klasy 'Surv'
                                               # 'multinomial' dla zmiennej factor
                                               # 'poisson' dla zmiennej całkowitej (counts) 
                n.trees = 100,                 # liczba iteracji
                interaction.depth = 1,         # 1 - model addytywny, 2 - interakcje pierwszego stopnia
                cv.folds = 0,                  # liczba k w k-krotnej walidacji krzyżowej; można dać 0 i pominąć walidację
                shrinkage = 0.1,               # learning rate (szybkość uczenia się, zbieżności)
                )

summary(sonargbm)

# diagnostyka modelu funckją gbm.perf
gbm.perf(sonargbm, method = "OOB") # out-of-bag (OOB)

sonargbm <- gbm(y~.,data=sonar,
                train.fraction = 0.8) # uczymy model na 80% danych a testujemy na 20%; domyślnie 1 
        
gbm.perf(sonargbm, method = 'test') # test metodą heldout 


sonargbm <- gbm(y~.,data=sonar,
                cv.folds = 5) # liczba k w k-krotnej walidacji krzyżowej 

gbm.perf(sonargbm, method = 'cv') # walidacja krzyżowa 

# gbm.perf(sonargbm, method = 'cv') # test metodą heldout 
best.cv <- gbm.perf(sonargbm, method = 'cv')
best.oob <- gbm.perf(sonargbm, method = 'OOB')

# względny wpływ zmiennych
par(mfrow = c(1, 2))
summary(sonargbm, n.trees = best.cv)          # using first tree
summary(sonargbm, n.trees = best.oob)

# data frame z opisem drzewka
print(pretty.gbm.tree(sonargbm, i.tree = sonargbm$n.trees))
print(pretty.gbm.tree(sonargbm, i.tree = best.cv))



#----caret----

library(caret)
library(doParallel)

data <- iris
indexTrain <- sample(nrow(data),round(nrow(data)*0.9),  replace = F)
gbmTrain <- data[indexTrain,] # pozostale elementy zbioru zostawimy dla testu

# tworzymy siatkę strojenia
# upewnij się, że nazywasz zmienne tak samo, jak hiperparametry w wywoływanej funkcji. 

grid <- expand.grid(n.trees = c(100,150), 
                    interaction.depth=c(1:3), 
                    shrinkage=c(0.01,0.05,0.1),
                    n.minobsinnode=20)


# do oceny modelu wykorzystamy metodę powtarzanej k-krotnej  walidacji krzyżowej z k = 5
# aby umożliwić równoległe obliczanie ustawiamy allowParallel = T
ctrl <- trainControl(method = "repeatedcv",number = 5, repeats = 2, allowParallel = T)

# rejestruje rdzenie równoległe
registerDoParallel(detectCores()-1)

# estymacja modelu
set.seed(124) #dla odtworzenia wyników
GBMModel <- train(Species~.,
                  data = gbmTrain,
                  method = "gbm", 
                  trControl = ctrl, 
                  tuneGrid = grid)

# podsumowanie modelu
print(GBMModel)

summary(GBMModel) # względny wpływ -  średni względny wzrost błędu predykcji, gdy wartości zmiennej zostaną losowo wymieszane (przestawiona kolejność)

plot(GBMModel) # accuracy z metody walidacji przy różnych parametrach

GBMModel$bestTune

# macierz pomyłek
confusionMatrix(GBMModel)

# predykcja
predict(GBMModel,newdata = data[-indexTrain,],type = 'raw')
predict(GBMModel,newdata = data[-indexTrain,],type = 'prob')

table(true = data$Species[-indexTrain],predicted = predict(GBMModel,newdata = data[-indexTrain,],type = 'raw'))
