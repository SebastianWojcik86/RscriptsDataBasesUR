
#----drzewa decyzyjne----
library(rpart) #tu jest algorytm CART
library(rpart.plot)
library(mlbench)# pakiet z danymi w R
library(rattle)
library(dplyr)


#wine <- read.csv2("//vmfrze01/dsm/=Sebastian=/UR/Zadania projektowe/Wine quality/winequality.csv")
wine <- rattle::wine

# przekoduj wartości zmiennej Type na 'katania', 'bolonia' i 'toskania'





levels(wine$Type) <- c('katania','bolonia','toskania')

# zrób wykresy pudełkowe wszystkich zmiennych numerycznych względem zmiennej Type
# które zmienne byłyby dobrymi predyktorami zmiennej Type?


boxplot(wine$Alcohol~wine$Type)
boxplot(wine$Ash~wine$Type)
boxplot(wine$Malic~wine$Type)
boxplot(wine$Magnesium~wine$Type)
boxplot(wine$Flavanoids~wine$Type)
boxplot(wine$Nonflavanoids~wine$Type)
boxplot(wine$Proanthocyanins~wine$Type)
boxplot(wine$Color~wine$Type)
boxplot(wine$Hue~wine$Type)
boxplot(wine$Dilution~wine$Type)
boxplot(wine$Proline~wine$Type)
boxplot(wine$Phenols~wine$Type)
boxplot(wine$Alcalinity~wine$Type)





# narysuj wykres punktowy zmiennych Dilution i Proline, zrób legendę






plot(wine$Dilution[wine$Type=="toskania"],wine$Proline[wine$Type=="toskania"],
     xlim=c(min(wine$Dilution),max(wine$Dilution)),
     ylim=c(min(wine$Proline),max(wine$Proline)),
     xlab = "Dilution",
     ylab = "Proline",
     col="red",
     pch=15)
points(wine$Dilution[wine$Type=="bolonia"],wine$Proline[wine$Type=="bolonia"],col="blue",pch=15)
points(wine$Dilution[wine$Type=="katania"],wine$Proline[wine$Type=="katania"],col="green",pch=15)
legend(x="topleft",legend = c("toskania","bolonia","katania"),pch = 15,col=c("red","green","blue"))



# drzewo decyzyjne
wineType <- rpart(Type~.,	# formuła postaci y~x1+x2 
                  data=wine, # nazwa ramki danych skąd brane są zmienne
                  method="class", # dla problemu regresji wybieramy 'anova', dla klasyfikacji wybieramy  'class'
                  model=TRUE, # czy zachować kopię modelu?
                  #control=rpart.control(minsplit=2,minbucket =1,cp=0.001), # parametry algorytmu definiowane przez obiekt rpart.control
                  na.action=na.rpart # co robić z NA?
) 


rpart.plot(wineType)


# ocena modelu 
rsq.rpart(wineType) #1 - R2 modelu oraz R2 w walidacji krzyżowej, 2 - odsetek błędnych predykcji
plotcp(wineType) # cp - minimalna wymagana zmiana R2/Accuracy, aby dokonać podziału drzewa
printcp(wineType) # Root node error - porównaj z 1-max(table(wine$Type))/nrow(wine)
summary(wineType)



# sprawdź dokładność predykcji modelu

predict(wineType,wine[,-1],type = "prob")
predict(wineType,wine[,-1],type = "class")

table(wine$Type,predict(wineType,wine[,-1],type = "class"))




# ZADANIE

Załaduj plik 'HCV data.csv'. Zmienna celu to 'Category'

1) utwórz zmienną 'Category2', która równa jest Category dla etykiet "Blood Donor" a w pozostałych przypadkach "Liver disease"
2) oglądnij dane i oceń wstępnie, które zmienne dobrze dyskryminują zmienną 'Category' oraz 'Category2'
3) zbuduj drzewa decyzyjne dla zmiennej 'Category' oraz 'Category2'
4) oceń jakość modelu


library(readr)
hcv <- read_delim("//vmfrze01/dsm/=Sebastian=/UR/Analiza danych w systemie R/Machine learning/Knn i drzewa decyzyjne/hcv dane.csv", 
                       delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                       trim_ws = TRUE)










#  wykresy pudełkowe zmiennych względem zmiennej Category2
hcv$Category2=dplyr::recode(hcv$Category,.default = "Liver disease","0=Blood Donor"="Blood Donor") %>% as.factor()
boxplot(data=hcv,ALB~Category2,col=c("red","green"))
boxplot(data=hcv,ALP~Category2,col=c("red","green"))
boxplot(data=hcv,AST~Category2,col=c("red","green"))
boxplot(data=hcv,BIL~Category2,col=c("red","green"))
boxplot(data=hcv,CHE~Category2,col=c("red","green"))
boxplot(data=hcv,CREA~Category2,col=c("red","green"))
boxplot(data=hcv,CHOL~Category2,col=c("red","green"))
boxplot(data=hcv,GGT~Category2,col=c("red","green"))



plot(data=hcv,GGT~AST,xlim=c(0,150),ylim=c(0,200),col=Category2)

hcv %>% filter(GGT<200&AST<150) %>% 
ggplot(aes(GGT,AST,group = Category2)) + 
  stat_density_2d(geom = "density_2d_filled", alpha = .5,aes( fill = Category2))+ #,h=c(21,9.9),
  theme_bw()

hcv %>% filter(!is.na(GGT)&!is.na(AST)) %>% 
  ggplot(aes(GGT,AST)) + 
  stat_density_2d(geom = "polygon",h=c(21,9.9),alpha = .5,aes(fill = Category2))+
  theme_bw()




ind_BD=which(hcv$Category2=="Blood Donor")
ind_LD=which(hcv$Category2=="Liver disease")

BD <-
  data.frame(
    X = round(as.numeric(hcv$GGT[ind_BD],0)),
    Y = round(as.numeric(hcv$AST[ind_BD],0)),
    Label=c(rep('Blood Donor',length(ind_BD)))
  )
LD <-
  data.frame(
    X = round(as.numeric(hcv$GGT[ind_LD],0)),
    Y = round(as.numeric(hcv$AST[ind_LD],0)),
    Label=c(rep('Liver disease',length(ind_LD)))
  )

plot_data=rbind(BD,LD)

plot_data %>% filter(X<75,Y<75) %>% 
ggplot(aes(X, Y)) + 
  stat_density_2d(geom = "polygon", aes(alpha = ..level.., fill = Label))+
  theme_bw()

