library(tm)
library(tidytext)
library(stringr)
library(textstem)
library(stringdist)
library(dplyr)

# Zamiana liter na małe/duże
tolower("I work for Statistics Poland") # na małe
toupper("I work for Statistics Poland") # na duże

# Usuwanie znaków interpunkcyjnych
mytext <- c("I'm a, good-looking new comer to r.","Please, help me:out, here!")
gsub(pattern = "[[:punct:]]",replacement = " ", x = mytext) # użycie funkcji podstawowego R i wyrażenia regularnego
removePunctuation(mytext) # z pakietu tm 
removePunctuation(mytext,preserve_intra_word_dashes = TRUE) # zachowaj "-"
removePunctuation(mytext,preserve_intra_word_contractions = TRUE) # zachowaj "`"

#  Usuwanie znaków specjalnych
x <- "a1~!@#$%^&*(){}_+:\"<>?,./;'[]-="
str_replace_all(string = x, pattern = "[[:punct:]]", replacement = " ") # nie złe
removePunctuation(x) # lepsze :)

# Usuwanie akcentów
tekst <- "Este sábado enfrentarán a un equipo."
iconv(tekst,to="ASCII//TRANSLIT")                # iconv jest w podstawowym R
iconv(x = "Műller",to = "ASCII//TRANSLIT")

# Usuwanie stopwords
tm::stopwords(kind = 'en')
tm::stopwords(kind = 'catalan')               # http://latel.upf.edu/morgana/altres/pub/ca_stop.htm
stop_words$word
x="I am a, good-looking new comer to r. Please, help me:out, here!"
x=gsub(pattern = "[[:punct:]]",replacement = " ", x = x)
x=gsub(pattern = "  ",replacement = " ", x = x) # podwójna spacja zamieniana na pojedynczą
x=strsplit(x,split = ' ') # X to lista, prosty sposób na tokenizację tekstu
x
setdiff(unlist(x),stopwords())







# Tokenizacja

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
z <- x %>% paste0(collapse = ' ') # Łączenie wyników w jeden ciąg
z <- data.frame(text=z) # zmiana wektora znaków na data.frame wymagany przez następną funkcję
z %>% unnest_tokens(output = 'words', input = 'text',token = 'words')






#----Document-term i document-feature matrix----

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

# Budowanie uporządkowanej ramki danych (jeden token na wiersz)

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
  mutate(word = str_extract(word, "[:alpha:]+")) %>% 
  filter(!is.na(word)) %>% 
  count(webpage, word, sort = TRUE)

# Uporządkowana ramka danych (tidy data frame) może być użyta do tworzenia wordcloudów.
library(wordcloud)
library(RColorBrewer)

df <- description %>% aggregate(n~word,data=.,FUN = sum) # data frame without webpage 
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
                 scale = c(1.5,0.5),
                 use.r.layout = F
)
# dev.off()


# document-term matrix and document-feature matrix
description_dtm <- cast_dtm(data = description,document = webpage,term = word,value = n) #forma listy
description_dfm <- cast_dfm(data = description,document = webpage,term = word,value = n)


description_sparse <- description %>% 
  tidyr::pivot_wider(names_from = word,values_from = n,values_fill = 0) # explicit form
description_sparse # Pożądany format wejściowy dla metod uczenia maszynowego opartych na ciągach tekstowych, np. analiza sentymentu itp.









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
z <- x %>% paste0(collapse = ' ') # Aby połączyć wynik w pojedynczy ciąg znaków.
z <- data.frame(text=z) # Aby zamienić wektor znaków na ramkę danych (data.frame) wymaganą przez następną funkcję.
z %>% unnest_tokens(output = 'words', input = 'text',token = 'words') # z pakietu tidytext

library(textstem)
stem_strings(x, language = "porter")

# Lista dostępnych języków znajduje się tutaj
SnowballC::getStemLanguages()

# Po hiszpańsku
y=c("Es decir, la cardinalidad de la intersección de ambos conjuntos dividida por la cardinalidad de su unión. 
    Siempre toma valores entre 0 y 1, correspondiente este último a la igualdad total entre ambos conjuntos.
    En este sentido 0 significa que las estaciones no presentan especies en común, 
    y tiende a 1 a medida que aumenta el número de especies compartidas.
    En informática se utiliza para medir la distancia entre vectores definidos sobre un espacio vectorial booleano 
    (las componentes del vector sólo pueden ser 0 o 1).)")

stem_strings(y, language = "spanish")









#----Lematyzacja----
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


# Ta funkcja używa słownika - czyli ramki danych, która określa, jaki token powinien zostać zamieniony na jaki lemat
# Domyślnie zestaw danych oparty na liście lematyzacji Mechury (2016) w języku angielskim z ponad 41 tys. wierszy i 2 zmiennymi

 
lexicon::hash_lemmas %>% head(10)

#Możesz użyć własnego słownika lub wygenerować nowy, bazując na własnych dokumentach przy użyciu wybranego silnika (algorytmu)
make_lemma_dictionary(x,engine = 'hunspell',lang = "en_US")

# Dla innych języków musisz pobrać dodatkowy plik, np. es_ES.aff
# Umieść go w swoim katalogu roboczym
y=c("Es decir, la cardinalidad de la intersección de ambos conjuntos dividida por la cardinalidad de su unión. 
    Siempre toma valores entre 0 y 1, correspondiente este último a la igualdad total entre ambos conjuntos.
    En este sentido 0 significa que las estaciones no presentan especies en común, 
    y tiende a 1 a medida que aumenta el número de especies compartidas.
    En informática se utiliza para medir la distancia entre vectores definidos sobre un espacio vectorial booleano 
    (las componentes del vector sólo pueden ser 0 o 1).)")

setwd("//vmfrze01/DSM/Szkolenia prowadzone przez DSM/Madryt 6-7 czerwca 2023 INE/Text string - script and data")
make_lemma_dictionary(y,engine = 'hunspell',lang = "es_ES")


lemmatize_strings(y,dictionary = make_lemma_dictionary(y,engine = 'hunspell',lang = "es_ES"))










#----Porównywanie nazw i adresów obiektów noclegowych----

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
# rozmiar zestawów danych
nrow(Survey_rzeszow) # 53
nrow(Webscraping_rzeszow) # 27
# więc 26=53-27 obiektów nie może mieć dopasowania

# sprawdźmy nazwy
head(Survey_rzeszow$name) # nazwy obiektów i firm -> trudny orzech do zgryzienia
head(Webscraping_rzeszow$name) # tylko nazwy obiektów

# zauważmy, że mamy obiekt, który MUSI być dopasowany
Survey_rzeszow$name[5] 
Webscraping_rzeszow$name[1]

# szukamy dalszych dopasowań
Webscraping_rzeszow$name[4]
grep('AMBASADOR',Survey_rzeszow$name,value = F) #returns indices: row 47
grep('AMBASADOR',Survey_rzeszow$name,value = T) #returns values: hotel `AMBASADOR`

# porównajmy nazwy
distances <- stringdistmatrix(Survey_rzeszow$name,Webscraping_rzeszow$name,method = 'lv') # Levensthein
rownames(distances) <- Survey_rzeszow$name
colnames(distances) <- Webscraping_rzeszow$name
head(distances)

distances[5,] 
distances[5,1] # co jest nie tak? dlaczego odległość między oczywistym dopasowaniem jest tak duża?

Survey_rzeszow$name <- Survey_rzeszow$name %>% tolower()

distances <- stringdistmatrix(Survey_rzeszow$name,Webscraping_rzeszow$name,method = 'lv')
rownames(distances) <- Survey_rzeszow$name
colnames(distances) <- Webscraping_rzeszow$name
head(distances)

distances[5,] # zdecydowanie lepiej
distances[5,1] # dla poprawnego dopasowania
distances[5,-1] %>% min() # najlepsze niepoprawne
distances[48,] # bardzo dobrze 
distances[48,4] # dla poprawnego dopasowania
distances[48,-4] %>% min() # najlepsze niepoprawne




# szukamy dopasowania

# ponieważ jest mniej obiektów w danych zebranych, łączymy nazwy z ankiety z nazwami ze scrapingu
bestmatch <- apply(distances, 2, which.min) # szukamy minimum w każdej kolumnie, zwraca indeks
bestdist <- apply(distances, 2, min) # szukamy minimum w każdej kolumnie, zwraca wartość
matched <- data.frame(survey = Survey_rzeszow$name[bestmatch], webscraping = Webscraping_rzeszow$name, dist = bestdist, row.names = NULL)


matched$survey %>% unique() %>% length()
matched$survey %>% length()
# więc 15=27-12 obiektów zostało dopasowanych co najmniej dwa razy -> to źle

# szukamy dopasowania z progiem
# dopasuj, gdy odległość jest mniejsza niż 10
dist_threshold = 9
matched2 <- matched
matched2$survey[matched2$dist>dist_threshold]=NA
matched2 <- matched2 %>% filter(!is.na(survey))

matched2$survey %>% length()
matched2$survey %>% unique() %>% length()
# 6=14-8 nadal 6 jest zdublowanych

# szukamy unikalnego dopasowania z progiem  
surv_unique <- unique(matched2$survey)

for(establ in surv_unique) 
  {
  ind <- which(matched2$survey == establ) # sprawdź establ=surv_unique[7]
  if(length(ind) == 1) {next} # jeśli jest dokładnie jedno dopasowanie, jest OK
  ind <- ind[-which.min(matched2$dist[ind])]
  matched2$survey[ind] <- NA
  }
matched2 <- matched2 %>% filter(!is.na(survey))

# wyniki
matched2 # 5 z 8 zostały poprawnie dopasowane

# sprawdź niedopasowane
matched %>% filter(!(webscraping %in% matched2$webscraping)) # 19 z 19 zostały poprawnie niedopasowane

# statystyki
TP = 5 # prawdziwe pozytywy (dopasowania)
FP = 3 # fałszywe pozytywy (dopasowania)
TN = 19 # prawdziwe negatywy (niedopasowania)
FN = 0 # fałszywe negatywy (niedopasowania)

# Macierz pomyłek i miary
(TP + TN) / nrow(matched) # dokładność
TP / (TP + FP) # precyzja
TP / (TP + FN) # czułość
2 * (TP / (TP + FP)) * (TP / (TP + FN)) / ((TP / (TP + FP)) + (TP / (TP + FN))) # wynik F1

#----Porównywanie opisów obiektów noclegowych----
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

# oblicz różnicę
stringdist(booking,tripadvisor,'cosine')
stringdist(booking,hotelcom,'cosine')
stringdist(tripadvisor,hotelcom,'cosine')

# later
str_extract_all('Is it a hotel, motel or hostel?', '(m|h)otel')
str_extract_all('Is it a hotel, motel or hostel?', 'ho(s|)tel')

# wartości dopasowania
stringdistmatrix(booking, hotelcom, method = "jaccard")
#Ta linia oblicza podobieństwo między dwoma tekstami przy użyciu metody Jaccarda, 
#która mierzy stopień nakładania się dwóch zestawów (w tym przypadku zestawów słów) i wylicza stosunek 
#liczby wspólnych elementów do liczby wszystkich unikalnych elementów w tych zestawach.

#Metoda Jaccarda jest szczególnie przydatna, gdy chcemy zmierzyć podobieństwo w kontekście zbiorów danych 
#(np. słów w tekstach), ignorując ich kolejność.
