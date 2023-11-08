

#----Random Forest----
library(randomForest)
library(dplyr)

# wykorzystując pakiet randomForest można wybrać tak parametry funkcji randomForest, że
# użyjemy Random Subspaces, Bagging, Pasting


irisRF <- randomForest(x = iris[,-5], # zmienne objaśniające
                       y = iris[,5], # zmienna objaśniana (dyskretna lub ciągła)
                       ntree = 200, #liczba drzew
                       nodesize=1, #minimalna liczba obiektów w liściu
                       replace = T, #czy losować ze zwracaniem
                       sampsize = nrow(iris), #jeżeli losujemy ze zwracaniem to dajemy cały zbiór, jeżeli bez to domyślnie jest 63,2% wielkości zbioru
                       mtry=2, # liczba zmiennych losowana do tworzenia podziału w węźle
                       do.trace=T #pozwala śledzić OOB dla skumulowanych drzew
)


# ważność zmiennych
varImpPlot(irisRF,n.var = 4,sort = T) 

# zdolność do dyksryminacji obiektów w zależności od wartości zmiennej
partialPlot(irisRF,pred.data = iris,x.var = Petal.Width, "versicolor")
partialPlot(irisRF,pred.data = iris,x.var = Petal.Width, "virginica")
partialPlot(irisRF,pred.data = iris,x.var = Petal.Width, "setosa")

versicolor=partialPlot(irisRF,pred.data = iris,x.var = Petal.Length, "versicolor",col="green")
virginica=partialPlot(irisRF,pred.data = iris,x.var = Petal.Length, "virginica",col="blue")
setosa=partialPlot(irisRF,pred.data = iris,x.var = Petal.Length, "setosa",col="red")
plot(versicolor,type="l",lwd=2,col="green",ylim=c(-15,15),xlab="Petal Length",ylab="Importance")
lines(virginica,lwd=2,col="blue")
lines(setosa,lwd=2,col="red")
legend(x=5.5,y=-2,legend = c("setosa","versicolor","virginica"),col=c("red","green","blue"),lty = 1,lwd=3,cex = 0.7)


# pojedyncze drzewa
getTree(irisRF,1,labelVar = T)

# OOB ogółem oraz dla każdej z  klas
plot(irisRF)
legend(x="topright",
       legend = colnames(plot(irisRF)),
       col=c("black","red","green","blue"),
       lty = 1,
       lwd=3,
       cex = 0.8)


plot(margin(irisRF)) # różnica między odsetkiem poprawnym predykcji i błędnych predykcji

predict(irisRF)


sum(diag(table(predict(irisRF),iris$Species)))/nrow(iris)
diag(table(predict(irisRF),iris$Species))/table(iris$Species)

# ------------------------------ Zadanie -----------------

# załaduj plik 'Boosting - Loans.xlsx'
# zmienna zależna to Loan_Status
# przeglądnij zmienne objaśniające grupując względem Loan_Status: wykorzystaj boxploty oraz tabele liczności
# które zmiennej mogą być dobrymi predyktorami Loan_Status?

# zastosuj algorytm random forest do predykcji zmiennej Loan_Status
# liczba drzew: 200
# liczba zmiennych losowana do tworzenia podziału w węźle: 5
# oceń model






data<-read.csv(url('https://datahack-prod.s3.ap-south-1.amazonaws.com/train_file/train_u6lujuX_CVtuZ9i.csv'))
setwd("//vmfrze01/dsm/=Sebastian=/UR/Analiza danych w systemie R/Machine learning/Ensamble")
library(openxlsx)
data<-read.xlsx('Boosting - Loans.xlsx')

# sprawdź za pomocą agregacji i wykresów, które zmienne najlepiej objaśniają zmienną Loan_Status
boxplot(ApplicantIncome~Loan_Status, data=data)
boxplot(LoanAmount~Loan_Status, data=data)
table(data$Gender,data$Loan_Status)
table(data$Married,data$Loan_Status)
table(data$Education,data$Loan_Status)
table(data$Credit_History,data$Loan_Status)
table(data$Property_Area,data$Loan_Status)

# model RandomForest 


data$Loan_ID=NULL
data$Loan_Status <- as.factor(data$Loan_Status)
data <- data %>% filter(!is.na(Loan_Status))
data <- data %>% filter(rowSums(is.na(data))==0)


loansRF <- randomForest(Loan_Status~., data = data,mtry = 5,ntree = 200)
loansRF <- randomForest(Loan_Status~., data = data,mtry = 5,ntree = 200,na.action = na.omit)

loanPredict=predict(loansRF,data)

# ważność zmiennych
varImpPlot(loansRF,n.var = 11,sort = T) 

# zdolność do dyksryminacji obiektów w zależności od wartości zmiennej
partialPlot(loansRF,pred.data = data,x.var = ApplicantIncome, "Y",col='green')
partialPlot(loansRF,pred.data = data,x.var = ApplicantIncome, "N",col="red")

Yes=partialPlot(loansRF,pred.data = data,x.var = ApplicantIncome, "Y",col='green')
No=partialPlot(loansRF,pred.data = data,x.var = ApplicantIncome, "N",col='red')

plot(Yes,type="l",lwd=2,col="green",ylim=c(-0.5,.5),xlab="ApplicantIncome",ylab="Importance")
lines(No,lwd=2,col="red")
legend(x=60000,y=0.1,legend = c("Yes","No"),col=c("green","red"),lty = 1,lwd=3)


partialPlot(loansRF,pred.data = data,x.var = LoanAmount, "Y",col='green')
partialPlot(loansRF,pred.data = data,x.var = LoanAmount, "N",col="red")

Yes=partialPlot(loansRF,pred.data = data,x.var = LoanAmount, "Y",col='green')
No=partialPlot(loansRF,pred.data = data,x.var = LoanAmount, "N",col='red')

plot(Yes,type="l",lwd=2,col="green",ylim=c(-0.8,.8),xlab="LoanAmount",ylab="Importance")
lines(No,lwd=2,col="red")
legend(x='bottomright',legend = c("Yes","No"),col=c("green","red"),lty = 1,lwd=3)

(table(predicted = loanPredict,true = data$Loan_Status))/nrow(data)

# oceń Loan_Status dla 
# -żonatego mężczyzny z dwójką dzieci, 
# -wyższym wykształceniem pracującym na etacie o zarobkach 4100 bez historii kredytowej
# -dla pożyczki na kwotę 240 na okres 360 na mieszkanie w mieście

newdata=data.frame(Gender="Male",
                   Married='Yes',
                   Dependents='2',
                   Education='Graduate',
                   Self_Employed='No',
                   ApplicantIncome=4100,
                   CoapplicantIncome=0,
                   LoanAmount=240,
                   Loan_Amount_Term=360,
                   Credit_History=0,
                   Property_Area='Urban'
) 

predict(loansRF,newdata) 
