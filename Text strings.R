library(tm)
library(tidytext)
library(tidyverse)
library(stringr)
library(textstem)
library(stringdist)
library(dplyr)

# Wielkość liter
tolower("I work for Statistics Poland") # bazowy R
toupper("I work for Statistics Poland") # bazowy R

# Usuwanie znaków interpunkcyjnych
tekst <- c("I'm a, good-looking new comer to r.","Please, help me:out, here!")
gsub(pattern = "[[:punct:]]",replacement = " ", x = tekst) # bazowy R i wyrażenia regularne
removePunctuation(tekst) # z pakietu tm
removePunctuation(tekst,preserve_intra_word_dashes = TRUE) # zostawia -
removePunctuation(tekst,preserve_intra_word_contractions = TRUE) # zostawia `

# Usuwanie znaków specjalnych
x <- "a1~!@#$%^&*(){}_+:\"<>?,./;'[]-="
str_replace_all(string = x, pattern = "[[:punct:]]", replacement = "") # nieźle
removePunctuation(x) # lepiej

# Usuwanie akcentów
tekst2 <- "Este sábado enfrentarán a un equipo."
iconv(tekst2,to="ASCII//TRANSLIT") # iconv jest w bazowym R
iconv(x = "Műller",to = "ASCII//TRANSLIT")

# Usuwanie  stopwords
tm::stopwords(kind = 'en')
tm::stopwords(kind = 'catalan') # http://latel.upf.edu/morgana/altres/pub/ca_stop.htm
stop_words$word
x="I am a, good-looking new comer to r. Please, help me:out, here!"
x=gsub(pattern = "[[:punct:]]",replacement = " ", x = x)
x=gsub(pattern = "  ",replacement = " ", x = x) # podwójne spacje zamienione na pojedyncz
x=strsplit(x,split = ' ') # x staje się listą, posty sposbó tokenizacji 
x
setdiff(unlist(x),stopwords())







# Tokenizing

x <- c(
  'the dirtier dog has eaten the pies',
  'that shameful pooch is tricky and sneaky',
  "He opened and then reopened the food bag",
  'There are skies of blue and red roses too!',
  NA,
  "The doggies, well they aren't joyfully running.",
  "The daddies are coming over...",
  "This is 34.546 above"
)
z <- x %>% paste0(collapse = ' ') # konkatenacja pojedynczych linijek w jeden tekst
z <- data.frame(text=z) # zamiana vector na data.frame required - konieczne dla kolejnej funkcji 
z %>% unnest_tokens(output = 'words', input = 'text',token = 'words')






#----Document-term oraz document-feature matrix----

booking <- "Hotel Rzeszow is a modern property in the center of the city and it offers elegant accommodations with 
free Wi-Fi. Rzeszow Train and Bus Stations are 651 m away. The rooms at Rzeszow are stylish and carpeted. 
Air-conditioned and allergy-free, they come with a TV with satellite channels and a bathroom with a hairdryer and
free toiletries. They offer views of the city. Further facilities in each room include a mini-bar and tea and 
coffee making equipment. A breakfast buffet is available every morning at the restaurant which serves meals of 
Polish and international cuisine and it is located in the conservatory. A wide selection of drinks can be enjoyed 
at the lobby bar. Guests can use a fitness club with a squash court and day spa facilities. Laundry and ironing 
services are also offered. Couples in particular like the location – they rated it 9.7 for a two-person trip."

hotelcom <- "Located near a train station, Hotel Rzeszow is a great choice for a stay in Rzeszow. 
Guests indulge in spa services, then grab a bite to eat at the coffee shop/cafe. Other highlights at this
upmarket hotel include a fitness centre and a bar/lounge. The restaurant and shopping get good marks from 
fellow travellers."

tripadvisor <- "If you’re looking for a charming hotel in Rzeszow, look no further than Hotel Rzeszow. 
Close to some of Rzeszow's most popular landmarks, such as Kosciol Sw. Wojciecha i Stanislawa (0.3 mi) and 
Lubomirskich Summer Palace (Letni Palac Lubomirskich) (0.6 mi), Hotel Rzeszow is a great destination for tourists.
The rooms offer a flat screen TV, air conditioning, and a minibar, and getting online is possible, as free wifi 
is available, allowing you to rest and refresh with ease. Hotel Rzeszow features a 24 hour front desk, 
room service, and baggage storage. In addition, as a valued Hotel Rzeszow guest, you can enjoy a fitness center
and breakfast that are available on-site. Guests arriving by vehicle have access to paid private parking 
available on-site. While visiting Rzeszow, you may want to try some ribs at one of the nearby restaurants, 
such as Stary Browar Rzeszowski. During your visit, be sure to check out popular attractions like 
Market Square (0.4 mi), Rzeszow Multimedia Fountain (0.6 mi), and Monument to The Revolutionary Action in 
Rzeszow (0.1 mi), which are all within walking distance of the hotel. The staff at Hotel Rzeszow looks forward 
to serving you during your upcoming visit."

# budujemy porządną data frame (one-token-per-row)

booking_tokens <- data.frame(webpage='booking',text=booking) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

hotelcom_tokens <- data.frame(webpage='hotelcom',text=hotelcom) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

tripadvisor_tokens <- data.frame(webpage='tripadvisor',text=tripadvisor) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

description <- bind_rows(booking_tokens,
                         hotelcom_tokens,
                         tripadvisor_tokens) %>% 
  mutate(word = str_extract(word, "[:alpha:]+")) %>% #zostawiamy tylko teksty
  filter(!is.na(word)) %>% # usuwamy braki z poprzedniego kroku
  count(webpage, word, sort = TRUE) # zliczamy występowanie słów

# powyższa ramka danych może być użyta do tworzenia wordcloud
library(wordcloud)
library(RColorBrewer)

df <- description %>% aggregate(n~word,data=.,FUN = sum) # zliczamy słowa pomijając stronę internetową
wordcloud(words = df$word,
          freq = df$n,
          min.freq = 1,
          random.order = F,random.color = F,
          colors = brewer.pal(n=8, name="Dark2"))

df2 <- pivot_wider(data = description,names_from = webpage,values_from = n,values_fill = 0)
term.matrix <- as.matrix(df2[,-1]) 
row.names(term.matrix) <- unlist(df2[,1])

# png("comparisoncloud.png",width = 500,height = 400)
comparison.cloud(term.matrix,
                 max.words=100,
                 random.order=FALSE,
                 scale = c(1.5,0.5)
                 #use.r.layout = F
)
# dev.off()


# document-term matrix oraz  document-feature matrix
description_dtm <- cast_dtm(data = description,document = webpage,term = word,value = n) #list form
description_dfm <- cast_dfm(data = description,document = webpage,term = word,value = n)


description_sparse <- description %>% 
  tidyr::pivot_wider(names_from = word,values_from = n,values_fill = 0) # wersja prosta
description_sparse # taka forma przetworzenia danych tekstowych jest wkładem do metod machine learning np. analiza sentymentów itp.









#----Stemming----
x <- c(
  'the dirtier dog has eaten the pies',
  'that shameful pooch is tricky and sneaky',
  "He opened and then reopened the food bag",
  'There are skies of blue and red roses too!',
  NA,
  "The doggies, well they aren't joyfully running.",
  "The daddies are coming over...",
  "This is 34.546 above"
)

z <- data.frame(text=x) # to change character vector into data.frame required by next function
z %>% unnest_tokens(output = 'words', input = 'text',token = 'words') # from tidytext

library(textstem)
stem_strings(x, language = "porter")

# lista dostępnych jęzków
SnowballC::getStemLanguages()

# hiszpański
y=c("Es decir, la cardinalidad de la intersección de ambos conjuntos dividida por la cardinalidad de su unión. 
    Siempre toma valores entre 0 y 1, correspondiente este último a la igualdad total entre ambos conjuntos.
    En este sentido 0 significa que las estaciones no presentan especies en común, 
    y tiende a 1 a medida que aumenta el número de especies compartidas.
    En informática se utiliza para medir la distancia entre vectores definidos sobre un espacio vectorial booleano 
    (las componentes del vector sólo pueden ser 0 o 1).)")

stem_strings(y, language = "spanish")









#----Lemmatization----
x <- c(
  'the dirtier dog has eaten the pies',
  'that shameful pooch is tricky and sneaky',
  "He opened and then reopened the food bag",
  'There are skies of blue and red roses too!',
  NA,
  "The doggies, well they aren't joyfully running.",
  "The daddies are coming over...",
  "This is 34.546 above"
)

lemmatize_strings(x)

# porównajmy
c(x[1],lemmatize_strings(x)[1])

# Powyższa funkcja używa słownika - data frame z dwoma zmiennymi: słowo i jego lemat 
# Domyśleni stosowany jest zbiór Mechura (2016) dla angielskiego z ponad 41 tys. słów
 
lexicon::hash_lemmas %>% head(10)

# można użyć własnego języka bądź nowowygenerowanego w oparciu o własnej dokumenty i wybrany silnik (algorytm)
make_lemma_dictionary(x,engine = 'hunspell',lang = "en_US")

# dla innych jęzków należy ściągnąć dodatkowy plik typu es_ES.aff
# place it in your working directory
y=c("Es decir, la cardinalidad de la intersección de ambos conjuntos dividida por la cardinalidad de su unión. 
    Siempre toma valores entre 0 y 1, correspondiente este último a la igualdad total entre ambos conjuntos.
    En este sentido 0 significa que las estaciones no presentan especies en común, 
    y tiende a 1 a medida que aumenta el número de especies compartidas.
    En informática se utiliza para medir la distancia entre vectores definidos sobre un espacio vectorial booleano 
    (las componentes del vector sólo pueden ser 0 o 1).)")

setwd("C:/Madryt 6-7 czerwca 2023 INE/Text string - script and data")
make_lemma_dictionary(y,engine = 'hunspell',lang = "es_ES")


lemmatize_strings(y,dictionary = make_lemma_dictionary(y,engine = 'hunspell',lang = "es_ES"))










#----Porównywanie nazw i adresów obiektów zbiorowego zakwaterowania ----

library(readxl)
setwd('//vmfrze01/DSM/Szkolenia prowadzone przez DSM/Madryt 6-7 czerwca 2023 INE/Text string - script and data')
Survey_rzeszow <- read_excel("Survey_rzeszow.xlsx", 
                             col_types = c("text", "text", "text", 
                                           "text", "text", "text", "text", "text", 
                                           "text", "text", "text", "numeric","numeric"))
Webscraping_rzeszow <- read_excel("Webscraping_rzeszow.xlsx", 
                                  col_types = c("numeric", "numeric", "text", 
                                                "text", "text", "text", "text", "text", 
                                                "text", "numeric", "numeric"))
# wielkość zbiorów
nrow(Survey_rzeszow) # 53
nrow(Webscraping_rzeszow) # 27
# Zatem 26=53-27 obiektów nie będzie sparowane

# oglądnijmy 
head(Survey_rzeszow$name) # nazwy obiektów oraz firm -> problematyczne
head(Webscraping_rzeszow$name) # tylko nazwy obiektów 

# w bazach występują obiekty, które konieczne musza być sparowane 
Survey_rzeszow$name[5] 
Webscraping_rzeszow$name[1]

# wyszukajmy inne pary
Webscraping_rzeszow$name[4]
grep('AMBASADOR',Survey_rzeszow$name,value = F) #zwraca indeks: wiersz 47
grep('AMBASADOR',Survey_rzeszow$name,value = T) #zwraca wartość: hotel `AMBASADOR`

# porównajmy nazwy
distances <- stringdistmatrix(Survey_rzeszow$name,Webscraping_rzeszow$name,method = 'lv') # Levensthein
rownames(distances) <- Survey_rzeszow$name
colnames(distances) <- Webscraping_rzeszow$name
head(distances)

distances[5,] 
distances[5,1] # co jest nie tak? dlaczego dystans pomiędzy oczywistymi parami jest tak duża?

Survey_rzeszow$name <- Survey_rzeszow$name %>% tolower()

distances <- stringdistmatrix(Survey_rzeszow$name,Webscraping_rzeszow$name,method = 'lv')
rownames(distances) <- Survey_rzeszow$name
colnames(distances) <- Webscraping_rzeszow$name
head(distances)

distances[5,] # zdecydowanie lepiej
distances[5,1] # dla pary
distances[5,-1] %>% min() # najlepszy kolejny ale nieprawidłowy
distances[48,] # bardzo dobrze 
distances[48,4] # dla pary
distances[48,-4] %>% min() # najlepszy kolejny ale nieprawidłowy




# znajdźmy parę

# z tego powodu, że jest mniej obiektów z web scrapingu niż z operatu będziemy dołączać dane z operatu do web scrapingu
bestmatch <- apply(distances, 2, which.min) # szukamy minimum w każdej kolumnie, zwracając indeks
bestdist <- apply(distances, 2, min) # szukamy minimum w każdej kolumnie, zwracając wartość
matched <- data.frame(survey=Survey_rzeszow$name[bestmatch],webscraping=Webscraping_rzeszow$name,dist=bestdist,row.names = NULL)

matched$survey %>% unique() %>% length()
matched$survey %>% length()
# zatem 15=27-12 obiektów zostało sparowano co najmniej dwa razy  -> to niedobrze

# znajdźmy pary uwzględniając wartość krytyczną (threshold)
# parujemy o ile odległość nie jest większa niż  10
dist_threshold = 9
matched2 <- matched
matched2$survey[matched2$dist>dist_threshold]=NA
matched2 <- matched2 %>% filter(!is.na(survey))

matched2$survey %>% length()
matched2$survey %>% unique() %>% length()
# 6=14-8 czyli nadal 6 jest zduplikowanych

# znajdźmy unikalną parę z użyciem wartości krytycznej odległości
surv_unique <- unique(matched2$survey)

for(establ in surv_unique)
{
  ind <- which(matched2$survey == establ) # sprawdź establ=surv_unique[7]
  if(length(ind)==1){next} # jeśli jest dokładnie jeden sparowany obiekt, to OK
  ind <- ind[-which.min(matched2$dist[ind])]
  matched2$survey[ind] <- NA
}
matched2 <- matched2 %>% filter(!is.na(survey))

# wyniki
matched2 # 5 z 8 jest prawidłowo sparowane

# co wywaliliśmy
matched %>% filter(!(webscraping %in% matched2$webscraping)) # 19 out of 19 are correctly unmatched 


# statystyki
TP=5 # true positives (matches)
FP=3 # false positives (matches)
TN=19 # true negatives (unmatches)
FN=0 # false negatives (unmatches)

# Confusion matrix i wskaźniki
(TP+TN)/nrow(matched) # accuracy
TP/(TP+FP) # precision
TP/(TP+FN) # recall
2*(TP/(TP+FP))*(TP/(TP+FN))/((TP/(TP+FP))+(TP/(TP+FN))) # F1 score



#----Porównywanie opisów hoteli ----
booking <- "Hotel Rzeszów is a modern property in the center of the city and it offers elegant accommodations with 
free Wi-Fi. Rzeszów Train and Bus Stations are 651 m away. The rooms at Rzeszów are stylish and carpeted. 
Air-conditioned and allergy-free, they come with a TV with satellite channels and a bathroom with a hairdryer and
free toiletries. They offer views of the city. Further facilities in each room include a mini-bar and tea and 
coffee making equipment. A breakfast buffet is available every morning at the restaurant which serves meals of 
Polish and international cuisine and it is located in the conservatory. A wide selection of drinks can be enjoyed 
at the lobby bar. Guests can use a fitness club with a squash court and day spa facilities. Laundry and ironing 
services are also offered. Couples in particular like the location – they rated it 9.7 for a two-person trip."

hotelcom <- "Located near a train station, Hotel Rzeszów is a great choice for a stay in Rzeszow. 
Guests indulge in spa services, then grab a bite to eat at the coffee shop/cafe. Other highlights at this
upmarket hotel include a fitness centre and a bar/lounge. The restaurant and shopping get good marks from 
fellow travellers."

tripadvison <- "If you’re looking for a charming hotel in Rzeszow, look no further than Hotel Rzeszow. 
Close to some of Rzeszow's most popular landmarks, such as Kosciol Sw. Wojciecha i Stanislawa (0.3 mi) and 
Lubomirskich Summer Palace (Letni Palac Lubomirskich) (0.6 mi), Hotel Rzeszow is a great destination for tourists.
The rooms offer a flat screen TV, air conditioning, and a minibar, and getting online is possible, as free wifi 
is available, allowing you to rest and refresh with ease. Hotel Rzeszow features a 24 hour front desk, 
room service, and baggage storage. In addition, as a valued Hotel Rzeszow guest, you can enjoy a fitness center
and breakfast that are available on-site. Guests arriving by vehicle have access to paid private parking 
available on-site. While visiting Rzeszow, you may want to try some ribs at one of the nearby restaurants, 
such as Stary Browar Rzeszowski. During your visit, be sure to check out popular attractions like 
Market Square (0.4 mi), Rzeszow Multimedia Fountain (0.6 mi), and Monument to The Revolutionary Action in 
Rzeszow (0.1 mi), which are all within walking distance of the hotel. The staff at Hotel Rzeszow looks forward 
to serving you during your upcoming visit."


stringdist(booking,tripadvisor,'cosine')
stringdist(booking,hotelcom,'cosine')
stringdist(tripadvisor,hotelcom,'cosine')

# later
str_extract_all('Is it a hotel, motel or hostel?', '(m|h)otel')
str_extract_all('Is it a hotel, motel or hostel?', 'ho(s|)tel')



#---- zadania ----
# wczytaj zbiór CBOP
# utwórz nowe zmienne: województw, powiat i adres na podstawie "Miejsce pracy" wykorzystując regex
# Na podstawie danych adresów nadal współrzędne geograficzne i zwizualizuj
# utwórz wordcloud z "Zakres obowiązków"
# jakie są rodzaje umowy? utwórz tablice i strukturę wg "Rodzja umowy"
# zgrupuj umowy o pracę razem i sprawdz jakie części etatu występują "Wymiar etatu"
# Zwizualizuj rozkład wynagrodzeń z "Wynagrodzenie brutto" przyjmując 
#   1) dolne widełki, 
#   2) średnią z widełek płacowych
# Jakie są systemy wynagradzania? "System wynagradzania" Utwórz tablicę
# utwórz wordcloud z "Zawód"
# Jakie jest wymagane wykształcenie? "Wykształcenie" 
# Utwórz tablicę z głównymi a osobno przebadaj te kierunkowe 
# utwórz wordcloud z "Krótki opis działalności gospodarczej"
# jaki jest odsetek ofert dla osób niepełnosprawnych? "Przeznaczone dla osób niepełnosprawnych"
# Wyodrębnij zmienne "Data dodania" i "Data ważności", skonwertuje je na datę.
# Jak można zwizualizować te daty?
# Oblicz przez jaki czas publikowana jest oferta. Jaki jest rozkład tego czasu?
