#----ZBIORY DANYCH----


# zbiór wygenerowany
set.seed(1)
n <-400
normal <-cbind(
  x = c(0.11,0.2, 0.6, 0.7)+rnorm(n, sd = 0.1),
  y = c(.9, 0.7,0.7, 1) + rnorm(n , sd = 0.1),
  true_clusters = rep(1:4, times = 100)
)

head(normal)

plot(y~x,data=normal,col=true_clusters)

#zbiór IRIS
head(iris)

plot(data=iris,Sepal.Length~Sepal.Width,col=Species)
plot(data=iris,Petal.Length~Petal.Width,col=Species)
plot(data=iris,Sepal.Length~Petal.Length,col=Species)
plot(data=iris,Sepal.Width~Petal.Width,col=Species)

#zbiór wine
library(rattle) # pakiet z danymi w R
head(wine)

plot(data=wine,Nonflavanoids~Flavanoids,col=Type)
plot(data=wine,Dilution~Proline,col=Type)
plot(data=wine,Alcohol~Malic,col=Type)
plot(data=wine,Alcalinity~Magnesium,col=Type)



#----KLASTROWANIE----

#----metoda k-średnich----
iris_kmeans <- kmeans(x = iris[,-5], # macierz danych numerycznych albo data.frame
                      centers = 3,   # liczba klastrów albo środki klastrów
                      iter.max = 10, # maksymalna liczba iteracji
                      nstart = 10    # liczba różnych układów punktów startowych (dla uzyskania zbieżności algorytmu)
                      )

iris_kmeans$tot.withinss # Within Sum of Squares
iris_kmeans$betweenss    # Between Sum of Squares
iris_kmeans$totss        # Total Sum of Squares
iris_kmeans$size         # liczebność klastrów
iris_kmeans$cluster      # przynależność do klastrów
iris_kmeans$centers      # centroidy klastrów











# sprawdź iloraz WSS/(WSS+BSS) dla k=2,...,10
# narysuj Scree Plot






ratio=rep(0,9)
for(k in 1:9)
{
  iris_kmeans <- kmeans(x = iris[,-5],centers = (k+1),iter.max = 10,nstart = 10)
  ratio[k] <- iris_kmeans$tot.withinss/iris_kmeans$totss
  print(ratio[k])
}  
ratio
plot(y = ratio,x=2:10,type="b",xlab="k",ylim = c(0,0.5))
abline(a=0.2,b=0)

library(dbscan)
# Funkcja hullplot tworzy otoczki wypukłe klastrów
hullplot(iris[,-5], # macierz danych. Jeżeli jest więcej niż dwie kolumny, obliczane są dwie główne składowe i wykres względem nich
         iris_kmeans # numeryczny wektor z oznaczeniem klastra albo obiekt klasy clust (zawiera element list z nazwą cluster)
) 
hullplot(iris[,c(3,1)],iris_kmeans)
hullplot(iris[,c(3,4)],iris_kmeans)
hullplot(iris[,c(4,2)],iris_kmeans)
hullplot(iris[,c(1,2)],iris_kmeans)



#----Density-Based Spatial Clustering of Applications with Noise (DBSCAN)----

library(dbscan)

plot(y~x,data=normal,col = true_clusters)


# poszukiwanie parametrów
results <- dbscan(normal[,-3],  # macierz danych numerycznych albo data.frame albo obiekt dist (idealna opcja dla własnych danych i metryk)
                  eps = 0.01,   # promień kuli,
                  minPts = 3)   # minimalna liczba punktów, aby utworzyć klaster
results # 0 oznacza element odstający, niesklastrowany
results$cluster # przypisanie do klastrów, 0 oznacza element niesklastrowany


plot(y~x,data=normal, col = results$cluster + 1L,pch = results$cluster + 1L) #czarne kółko to obiekt niesklastrowany
# bardzo małe klastry -> zwiększ eps np. 0.1

results <- dbscan(normal[,-3], eps = 0.1, minPts = 3) 
results 
plot(y~x,data=normal, col = results$cluster+1L,pch = results$cluster + 1L) 

# Funkcja hullplot tworzy otoczki wypukłe klastrów
hullplot(normal[,-3], # macierz danych. Jeżeli jest więcej niż dwie kolumny, obliczane są dwie główne składowe i wykres względem nich
         results # numeryczny wektor z oznaczeniem klastra albo obiekt klasy clust (zawiera element list z nazwą cluster)
         ) 

# tylko jeden klaster -> 
  # ja wybrać odległość? tak aby wyszła konkretna liczba klastrów?
  # czy można wybrać odległość bardziej obiektywnie?



# Scree plot

# Funkcja kNNdist przedstawia odległości każdego punktu od k najbliższych jego sąsiadów
kNNdist(normal[,-3], # macierz numeryczna albo obiekt dist (odległość Euklidesowa jest domyślna)
        k = 3,       # liczba najbliższych sąsiadów
        all = TRUE   # T - macierz dla wszystkich k sąsiadów, F - dla najbliższego
        ) 

kNNdist(normal[,-3], k = 3) # odległość do najdalszego sąsiada z k najbliższych

# Funkcja kNNdistplot pokazuje jaka część (odsetek) punktów (oś OY) ma najdalszego z k-sąsiadóww w danym promieniu (oś OX)
kNNdistplot(normal[,-3], # macierz numeryczna albo obiekt dist (odległość Euklidesowa jest domyślna)
            k = 3)       # liczba najbliższych sąsiadów

abline(h = .04,col = "red",lty = 2) #szukamy zgięcia łokcia

results <- dbscan(normal[,-3], eps = 0.04, minPts = 3) 
results 

plot(y~x,data=normal, col = results$cluster+1L) 
hullplot(normal[,-3],results)

# normal=rbind(normal,cbind(x=0.9,y=1.1,true_clusters=0))
# predict(results,normal[nrow(normal),-3],data=normal[,-3])
predict(results,newdata = cbind(0.6,0.7),data = normal[,-3])


# ZADANIE

# wykorzystując DBSCAN poszukaj optymalnych parametrów i zrób klastrowanie zbioru iris
kNNdist(iris[,-5], k = 3,all = T) #sprawdźmy jakie są odległości każdego punktu od k najbliższych sąsiadów
kNNdist(iris[,-5], k = 3) #największa odległość każdego punktu od k najbliższych sąsiadów
kNNdistplot(iris[,-5], k = 3) #pokazuje jaka część punktów ma najdalszego z k-sąsiadów w danym promieniu (oś OX)
abline(h = .5,col = "red",lty = 2) #szukamy 'łokcia'


results <- dbscan(
  as.matrix(iris[,-5]),
  eps = 0.5, 
  minPts = 3
)
results

plot(Petal.Length~Sepal.Length,data=iris, col = results$cluster+1L,pch = results$cluster + 1L) 
plot(Petal.Length~Petal.Width,data=iris, col = results$cluster+1L,pch = results$cluster + 1L) 
plot(Petal.Width~Sepal.Width,data=iris, col = results$cluster+1L,pch = results$cluster + 1L) 
plot(Sepal.Length~Sepal.Width,data=iris, col = results$cluster+1L,pch = results$cluster + 1L) 

hullplot(iris[,-5],results) # wg dwóch czynników głównych
hullplot(iris[,c(3,1)],results)
hullplot(iris[,c(3,4)],results)
hullplot(iris[,c(4,2)],results)
hullplot(iris[,c(1,2)],results)

# jak uzyskane klastry są powiązane z gatunkami
table(results$cluster,iris$Species)
table(results$cluster,iris$Species) %>% chisq.test()


#----Mean Shift----
library(meanShiftR)

results <- meanShift(
  queryData = normal[,-3],   # zbiór punktów, które chcemy pogrupować
  trainData = normal[,-3],   # zbiór punktów do szacowania gęstości; może być mniejszy niż queryData
  algorithm="LINEAR",        # albo 'KDTREE'
  nNeighbors = nrow(normal), # z ilu najbliższych sąsiadów szacować ocenę estymatora jądrowego
  iterations = 15,           # liczba iteracji
  bandwidth = rep(0.24,2)    # wektor długości równej liczbie kolumn queryData; parametry do estymatora jądrowego
)

results$assignment        # przypisanie do klastrów
table(results$assignment) # liczności klas
results$value             # wektor wartości nabliższego lokalnego maksimum
unique(results$value)     # lokalne maksima gęstości

hullplot(normal[,-3],results$assignment)

# spróbujmy wykorzystać statystyki zbioru
normal[,-3] %>% apply(MARGIN = 2,FUN = sd)

results <- meanShift(
  queryData = normal[,-3],   # zbiór punktów, które chcemy pogrupować
  trainData = normal[,-3],   # zbiór punktów do szacowania gęstości; może być mniejszy niż queryData
  algorithm = "LINEAR",        # albo 'KDTREE'
  nNeighbors = nrow(normal), # z ilu najbliższych sąsiadów szacować ocenę estymatora jądrowego
  iterations = 15,           # liczba iteracji
  bandwidth = c(0.27,17)     # wektor długości równej liczbie kolumn queryData; parametry do estymatora jądrowego
)

results$assignment        # przypisanie do klastrów
table(results$assignment) # liczności klas
results$value             # wektor wartości nabliższego lokalnego maksimum
unique(results$value)     # lokalne maksima gęstości

hullplot(normal[,-3],results$assignment)
aggregate(normal[,-3],list(results$assignment),mean)

# ZADANIE

# wykorzystując MeanShift poszukaj optymalnych parametrów i zrób klastrowanie zbioru iris

results <- meanShift(
  as.matrix(iris[,-5]),
  algorithm="LINEAR",
  iterations = 1,
  bandwidth = rep(10,4)
)


results$assignment
table(results$assignment) #liczności klas
results$value
unique(results$value) # lokalne maksima

hullplot(iris[,-5],results$assignment) # wg dwóch czynników głównych
hullplot(iris[,c(3,1)],results$assignment)
hullplot(iris[,c(3,4)],results$assignment)
hullplot(iris[,c(4,2)],results$assignment)
hullplot(iris[,c(1,2)],results$assignment)









#---- EM-GMM ----
library(mclust)                  

results = Mclust(data = normal[,-3], # wektor, macierz albo ramka danych
                 G = NULL, # zakres sprawdzanych klastrów, domyślnie od 1 do 9
                 modelNames = NULL # wektor nazw modelu do testowania, domyślnie sprawdza wszystkie pasujące do danych
)


results$classification #przydział do klastra
results$uncertainty # prawdpodobieństwo błędu klasyfikacji przy danych założeniach modelu

plot(results) #wpisz 2 w konsoli - klastry
plot(results) #wpisz 3 w konsoli - uncertainty
plot(results) #wpisz 4 w konsoli - gęstość

hullplot(normal[,-3],results$classification)




# ZADANIE

# wykorzystując EM poszukaj optymalnych parametrów i zrób klastrowanie zbioru iris

results = Mclust(iris[,-5])

plot(results) 




# ZADANIE

# wykorzystując poznane metody zrób klastrowanie zbioru wine z pakietu rattle
# użyj zmiennych 
  # Nonflavanoids, Flavanoids, Dilution, Proline
# porównaj wyniki klastrowania ze zmienną Type

