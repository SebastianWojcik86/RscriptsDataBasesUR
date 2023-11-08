if(!require('JOUSBoost')) {
  install.packages('JOUSBoost')
  library('JOUSBoost')
}


# Opisać i wizualizacja zbioru circle_data ----

#' circle_data 
#' zbiór punktów losowanych na płaszczyźnie z kwadratu o wymiariach 2xouter_r - lista X
#' zbiór wartości ze zbioru (-1,1) - lista y
#' 
#' współrzędne X losowane są z rozkładu jednostajnego z przedziału (-outer_r,outer_r) - domyślnie (-28,28)
#' współrzędne y są losowane z rozkładu Bernoulliego 
#' p = 1 (r < inner_r) + (outer_r - r)/(outer_r - inner_r) * ((inner_r < r) & (r < outer_r))
#' 
#' Jeśli punkt leży poza zewnętrzym okregiem - wartość= -1
#' Jeśli punkt leży w wewnętrzym okręgu - wartość = 1
#' Jeśli punkt leży w pierścieniu wartość -1,1 zależy od połozenia z tym, że im bliżej środka tym prawdopodobieństwo, że wartość będzie równe 1 rośnie



# Generowanie danych 
set.seed(111)
inner_r = 8
outer_r = 28
dat = circle_data(inner_r = 8, outer_r = 28, n = 500)
train_index = sample(1:500, 400) #losowanie 400 elementów do zbioru uczącego

# Wizualizacja prawdopodobieństwa zakwalifikowania do wartości -1 lub 1
library(ggplot2)
df=data.frame(x=dat$X[,1],y=dat$X[,2],p=dat$p)

ggplot(df)+
  geom_point(aes(x,y,col=p))+
  scale_color_continuous(type = "viridis")+
  theme_bw()

# Wizualizacja zakwalifikowania z dołączonymi wraz z okręgami
df=data.frame(x=dat$X[,1],y=dat$X[,2],p=as.factor(dat$y)) # ramka ze zmienną celu skonewrtowaną na factor

library(ggforce)
legend_title <- "y"

ggplot(df,aes(x,y,col=p))+geom_point()+scale_colour_manual(legend_title,values=c("-1"="red","1"="green"))+
  theme_bw()+
  geom_circle(aes(x0 = 0, y0 = 0, r = inner_r),inherit.aes = FALSE)+
  geom_circle(aes(x0 = 0, y0 = 0, r = outer_r),inherit.aes = FALSE)


# ---- Model adaboost ----

df=data.frame(x=dat$X[,1],y=dat$X[,2],p=dat$p)

ada = adaboost(                             
  dat$X[train_index,],                      #Macierz współrzędnych
  dat$y[train_index],                       #Wektor odpowiedzi -1 lub 1 - wymagane zakwalifikowanie w adaboost
  tree_depth = 2,                           #Ustalona "głębokość drzewa"
  n_rounds = 200,                           #Ilość rund/iteracji
  verbose = TRUE                            #Czy pokazać iteracje
  #control = NULL                           #lista kontrolująca właściwości dopasowania drzew decyzyjnych
  
                                                #control = rpart::rpart.control(minsplit = 0, minbucket = 1, cp = -1,
                                                #maxcompete = 0, maxsurrogate = 0, usesurrogate = 0, xval = 0, maxdepth = tree_depth)
                                                
  )

# Wyniki modelu ----

ada$alphas # wagi drzewek.Wagi obliczone w dopasowaniu adaboost
ada$confusion_matrix # macierz pomyłek
ada$trees # podgląd pojedynczych drzewek. Drzewa zbudowane w każdej rundzie wzmocnienia.

# predykcja
print(ada)                                      #zwraca głębokość drzewa, ilość wykonanych rund oraz macierz pomyłek
yhat_ada = predict(ada, dat$X[-train_index,])   #zwraca wartości wg modelu ada dla zbioru który nie był ujety w zbiorze uczącym

# misclassification rate
mean(dat$y[-train_index] != yhat_ada)           #zwraca odsetek błędnej klasyfikacji

# Miary na podstawie macierzy pomyłek (w innych pakietach) ----

library(caret)
caret::confusionMatrix(as.factor(predict(ada, dat$X[train_index,])),as.factor(dat$y[train_index]))
library(gmodels)
gmodels::CrossTable(as.factor(predict(ada, dat$X[train_index,])),as.factor(dat$y[train_index]))

# Miary wyszczególnienie ----

 TP=ada$confusion_matrix[1]                     #True Positive
 FP=ada$confusion_matrix[2]                     #False Positive
 FN=ada$confusion_matrix[3]                     #False Negative
 TN=ada$confusion_matrix[4]                     #True Negative
 
 P=TP+FN                                        #Positive rzeczywisty stan
 N=FP+TN                                        #Negative rzeczywisty stan
 
 PP=TP+FP                                       #Positive przewidywany stan
 FF=FN+TN                                       #Negative przewidywany stan
 
 Prevalence=P/(P+N)                             #Prevalence
 ACC=(TP+TN)/(P+N)                              #Accuracy
 
 TPR=TP/P                                       #True Positive Rate lub Recall lub Sensitivity lub Hit rate
 TNR=TN/N                                       #True Negative Rate lub Specificity lub selectivity
 BA=(TPR+TNR)/2                                 #Balanced Accuracy
 
 PPV=TP/(TP+FP)                                 #Positive Predictive Value lub Precision
 NPV=TN/(TN+FN)                                 #Negative Predictive Value
 FNR=FN/P                                       #Miss Rate lub False Negative Rate
 FPR=FP/N                                       #Fall-out lub False Positive Rate
 FDR=FP/(FP+TP)                                 #False Discovery Rate
 FOR=FN/(FN+TN)                                 #False Omission Rate
 LR_plus=TPR/FPR                                #Positive likelihood ratio
 LR_minus=FNR/TNR                               #Negative likelihood ratio
 PT=sqrt(FPR)/(sqrt(TPR)+sqrt(FPR))             #Prevalence threshold (PT)
 TS=TP/(TP+FN+FP)                               #Threat score (TS) lub critical success index (CSI) lub Jaccard index 
 
 F1_score=(2*PPV*TPR)/(PPV+TPR)                 #Średnia harmoniczna precyzji i czułości. 
                                                #Im bliższa jest jedynki, tym lepiej to świadczy o algorytmie klasyfikującym. 
                                                #W najlepszym przypadku przyjmuje wartość 1, kiedy mamy do czynienia z idealną czułością i precyzją. 
 MCC=(TP*TN-FP*FN)/
   (sqrt(TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))        #phi coefficient (φ or rφ) or Matthews correlation coefficient (MCC)
                                                #Dokładność jest bardzo wrażliwa w przypadku niezrównoważonych klas. 
                                                #Precyzja (PPV), czułość (TPR), F1-score są niesymetryczne. 
                                                #I jak tu wybrać coś, co w miarę obiektywnie powie nam, czy dobrze udało nam się dokonać klasyfikacji? 
                                                #Tutaj na pomoc przychodzi współczynnik korelacji Matthews. 
                                                #Jeżeli wynosi 1, to znaczy, że nasz model perfekcyjnie klasyfikuje wszystko do prawidłowej kategorii.
                                                #Wartość -1 oznacza, że wszystko zostało zaliczone do niepoprawnej kategorii. 
                                                #zero - wynik byśmy mogli otrzymać rzucając monetą albo klasyfikując w inny absolutnie losowy sposób.
 
 FM=sqrt((TP/(TP+FP))*(TP/(TP+FN)))             #Fowlkes–Mallows index
 BM=TPR+TNR-1                                   #Informedness lub Bookmaker informedness lub Statystyka J Youdena
 MK=PPV+NPV-1                                   #Markedness (MK)
 DOR=LR_plus/LR_minus                           #Diagnostic odds ratio (DOR)

# Funkcja: Miary na podstawie macierzy pomyłek ----
 
 Miary_Macierzy_pomyłek=function(confusion_matrix)   #To samo co wyżej, ale w postaci funkcji. Bedzie przydatna później w ćwiczeniu
 {
   TP=confusion_matrix[1]                     
   FP=confusion_matrix[2]                     
   FN=confusion_matrix[3]                     
   TN=confusion_matrix[4]                     
   P=TP+FN                                       
   N=FP+TN                                      
   PP=TP+FP                                     
   FF=FN+TN                                       
   Prevalence=P/(P+N)                            
   ACC=(TP+TN)/(P+N)                            
   TPR=TP/P                                       
   TNR=TN/N                                       
   BA=(TPR+TNR)/2                                 
   PPV=TP/(TP+FP)                                 
   NPV=TN/(TN+FN)                                
   FNR=FN/P                                       
   FPR=FP/N                                       
   FDR=FP/(FP+TP)                                 
   FOR=FN/(FN+TN)                                
   LR_plus=TPR/FPR                               
   LR_minus=FNR/TNR                               
   PT=sqrt(FPR)/(sqrt(TPR)+sqrt(FPR))            
   TS=TP/(TP+FN+FP)                               
   F1_score=(2*PPV*TPR)/(PPV+TPR)                  
   MCC=(TP*TN-FP*FN)/(sqrt(TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))       
   FM=sqrt((TP/(TP+FP))*(TP/(TP+FN)))             
   BM=TPR+TNR-1                                  
   MK=PPV+NPV-1                                  
   DOR=LR_plus/LR_minus                          
   
   wyniki=list(
     "Prevalence"=Prevalence,
     "Accuracy"=ACC,                       
     "True Positive Rate"=TPR,                                   
     "True Negative Rate"=TNR,                                     
     "Balanced Accuracy"=BA,                               
     "Positive Predictive Value"=PPV,                                 
     "Negative Predictive Value"=NPV,                            
     "Miss Rate lub False"=FNR,                                   
     "False Positive Rate"=FPR,                                      
     "False Discovery Rate"=FDR,                               
     "False Omission Rate"=FOR,                              
     "Positive likelihood ratio"=LR_plus,                       
     "Negative likelihood ratio"=LR_minus,                             
     "Prevalence threshold"=PT,             
     "Threat score"=TS,                            
     "Harmonic mean of precision"=F1_score, 
     "Matthews correlation coefficient"=MCC,
     "Fowlkes–Mallows index"=FM,
     "Bookmaker informedness"=BM,
     "Markedness"=MK,
     "Diagnostic odds ratio"=DOR
   )
   
   return(wyniki)
 }
 
# Inne dane w pakiecie JOUSBoost? ----
        #' Pakiet zawiera również zbiór danych: 
        #'                       friedman_data = function (n = 500, d = 10, gamma = 10)
        #' który jest generowany w następujacy sposób:
        #' najpierw losowanan jest tablica liczb n wierszy i d kolum (d>5). Na podstwie 6 pierwszych kolum generowana jest wartość
        #' log_odds = gamma razy (1 - X[, 1] + X[, 2] - X[, 3] + X[, 4] - X[, 5] + X[, 6]) razy rowSums(X[, 1:6])
        #' Wartość ta jest wykorzystywana do obliczenia wartości p = exp(log_odds)/(1 + exp(log_odds)).
        #' Ostatecznie generowany jest n elementowy wektor y bedący wynikiem symulacji ile będzie trafień w serii jednoelementowej przy zadanym p
        #' y = 2 * stats::rbinom(n, 1, p) - 1. Wynik jest skalowny do wartości -1 lub 1
        #' Uwaga: kolumny od 7 do d nie są zasadnioczo używane po wygenerowaniu.

# 

# Z A D A N I E----

#' Zbiór sonar - jeszcze jeden zbiór danych z tego pakietu
#' Zbiór danych "sonar" zawierająca 208 wyników pomiaru energi odbitej od danego obiektu dla 60 częstotliwości. 
#' Dodatkowo zbiór zawiera informację czy dany obiekt to skała (wartość 1) czy minerał (wartość -1)
library(dplyr)

# Analiza zbioru
dat2=sonar
ind_skala=which(dat2$y==1)
ind_miner=which(dat2$y==0)
dat2_skala=dat2[ind_skala,]        #podział na dwie bazy danych - może być przytarne do analizy tylko na wybranym typie kopalin
dat2_miner=dat2[ind_miner,]

dat2nazwa=dat2                     #ta sama baza danych tylko, zamiast 1,-1 są nazwy - do wizualizacji
dat2nazwa$y[ind_miner]="minerał"
dat2nazwa$y[ind_skala]="skała"
colnames(dat2nazwa)[ncol(dat2nazwa)]="rodzaj" 

#przykład nalicznia statystyk
Me_sonar=as.data.frame(aggregate(dat2[,1:60],by = list(dat2$y),FUN = mean))

dane=t(Me_sonar[,-1])
date=as.data.frame(cbind(1:60,dane))

dane=as.data.frame(
  matrix(
    c(
      1:(ncol(Me_sonar)-1),
      Me_sonar[1,2:(ncol(Me_sonar))],
      Me_sonar[2,2:(ncol(Me_sonar))]
      ),
    nrow=60,
    ncol=3,
    byrow=FALSE
    )
  )
colnames(dane)=c("x","M_miner","M_skala") # finalnie mamy tabelę z danymi o średniej w podpróbkach wg fal


ggplot(date,aes(x=V1))+
  geom_line(aes(y=V2,color="Minerał"))+geom_line(aes(y=V3,color="Skała"))+
  guides(color = guide_legend(title = "Typ"))+
  scale_x_continuous(name = "Numer częstotliwości")+
  scale_y_continuous(name = "Średni pomiar odbicia przez kopalinę")+
  scale_color_manual(labels = c("Minerał", "Skała"), values = c("red", "blue"))



#zamiana na formę długą bez wyróżnienia - zachowuje tylko na przyszłe cele
#dat_wek=data.frame(iksy=rep(colnames(dat2)[-ncol(dat2)],each=nrow(dat2)),
#                    wart=matrix(as.matrix(dat2[,-ncol(dat2)]),nrow=nrow(dat2)*(ncol(dat2)-1),ncol=1))
#ggplot(dat_wek, aes(x=iksyx,y=wart)) + geom_boxplot()


#zamiana danych na formę dlugą z wyróżniemiem - minerał, skała
library(reshape2)
data_melt=melt(dat2nazwa, id = c("rodzaj"))

ggplot(data_melt, aes(x=variable,y=value,color=rodzaj)) + geom_boxplot()

#wyraźnie po 10
data_melt_1=data_melt[1:(208*10),]
data_melt_2=data_melt[((208*10)+1):(208*20),]
data_melt_3=data_melt[((208*20)+1):(208*30),]
data_melt_4=data_melt[((208*30)+1):(208*40),]
data_melt_5=data_melt[((208*40)+1):(208*50),]
data_melt_6=data_melt[((208*50)+1):(208*60),]
ggplot(data_melt_1, aes(x=variable,y=value,color=rodzaj)) + geom_boxplot()
ggplot(data_melt_2, aes(x=variable,y=value,color=rodzaj)) + geom_boxplot()
ggplot(data_melt_3, aes(x=variable,y=value,color=rodzaj)) + geom_boxplot()
ggplot(data_melt_4, aes(x=variable,y=value,color=rodzaj)) + geom_boxplot()
ggplot(data_melt_5, aes(x=variable,y=value,color=rodzaj)) + geom_boxplot()
ggplot(data_melt_6, aes(x=variable,y=value,color=rodzaj)) + geom_boxplot()

# Funkcja MULTIplot 
MULTIplot <- function(..., plotlist=NULL, cols) 
  {
  require(grid)
  
  # Zrób listę z ... (podanych) i plotlisty 
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # struktura panelu
  plotCols = cols                           # Liczba kolumn w grafice
  plotRows = ceiling(numPlots/plotCols)     # Oblicza ilość wierszy
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Poukładanie w odpowiednim miejscu
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
}


p1=ggplot(data_melt_1, aes(x=variable,y=value,color=rodzaj)) + geom_boxplot()
p2=ggplot(data_melt_2, aes(x=variable,y=value,color=rodzaj)) + geom_boxplot()
p3=ggplot(data_melt_3, aes(x=variable,y=value,color=rodzaj)) + geom_boxplot()
p4=ggplot(data_melt_4, aes(x=variable,y=value,color=rodzaj)) + geom_boxplot()
p5=ggplot(data_melt_5, aes(x=variable,y=value,color=rodzaj)) + geom_boxplot()
p6=ggplot(data_melt_6, aes(x=variable,y=value,color=rodzaj)) + geom_boxplot()


MULTIplot(p1,p2,p3,p4,p5, cols=2)

#ZADANIE
# dla zbioru sonar  przeprowadz uczenie w oparciu o adaboost względem zmiennej y.
# oceń wyniki na podstawie Confusion matrix



#Rozwiązanie:
sonar$y=recode(sonar$y,'1'=1,'0'=-1)

ada_sonar = adaboost(                             
  as.matrix(sonar[-ncol(sonar)]),          #Macierz x
  sonar$y,                       #Wektor odpowiedzi -1 lub 1 - wymagane zakwalifikowanie w adaboost
  tree_depth = 2,                           #Ustalona "głębokość drzewa"
  n_rounds = 200,                            #Ilość rund
  verbose = TRUE                            #Czy pokazać iteracje
  #control = NULL                           #lista kontrolująca właściwości dopasowania drzew decyzyjnych
)




# wyniki
ada_sonar$alphas # wagi drzewek. Wagi obliczone w dopasowaniu adaboost
ada_sonar$confusion_matrix # macierz pomyłek
ada_sonar$trees # podgląd pojedynczych drzewek. Drzewa zbudowane w każdej rundzie wzmocnienia.

# opis
print(ada_sonar)                                      #zwraca głębokość drzewa, ilość wykonanych rund oraz macierz pomyłek


yhat = predict(ada_sonar, sonar)   #zwraca wartości wg modelu ada... dla zbioru który nie był ujety w zbiorze uczącym


# misclassification rate
mean(sonar$y != yhat)           #zwraca odsetek błędnej klasyfikacji


# Miary na podstawie macierzy pomyłek
caret::confusionMatrix(as.factor(yhat),as.factor(sonar$y))
gmodels::CrossTable(as.factor(yhat),as.factor(sonar$y))


Miary_Macierzy_pomyłek(ada_sonar$confusion_matrix)





