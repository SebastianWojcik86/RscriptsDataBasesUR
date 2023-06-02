library(tm)
library(tidytext)
library(stringr)
library(textstem)
library(stringdist)
library(dplyr)

# Casefolding
tolower("I work for Statistics Poland") # base R
toupper("I work for Statistics Poland") # base R

# Removing punctuation
mytext <- c("I'm a, good-looking new comer to r.","Please, help me:out, here!")
gsub(pattern = "[[:punct:]]",replacement = " ", x = mytext) # using base R function and regular expression
removePunctuation(mytext) # from tm package
removePunctuation(mytext,preserve_intra_word_dashes = TRUE) # keeps -
removePunctuation(mytext,preserve_intra_word_contractions = TRUE) # keeps `

# Removing special characters
x <- "a1~!@#$%^&*(){}_+:\"<>?,./;'[]-="
str_replace_all(string = x, pattern = "[[:punct:]]", replacement = " ") # not bad
removePunctuation(x) # better

# Removing accents
nota <- "Este sábado enfrentarán a un equipo."
iconv(nota,to="ASCII//TRANSLIT") # iconv is in base R
iconv(x = "Műller",to = "ASCII//TRANSLIT")

# Removing stopwords
tm::stopwords(kind = 'en')
tm::stopwords(kind = 'catalan') # http://latel.upf.edu/morgana/altres/pub/ca_stop.htm
stop_words$word
x="I am a, good-looking new comer to r. Please, help me:out, here!"
x=gsub(pattern = "[[:punct:]]",replacement = " ", x = x)
x=gsub(pattern = "  ",replacement = " ", x = x) # double space replaced with single space
x=strsplit(x,split = ' ') # x is a list, simple way to tokenize text
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
z <- x %>% paste0(collapse = ' ') # To collapse the output into a single string
z <- data.frame(text=z) # to change character vector into data.frame required by next function
z %>% unnest_tokens(output = 'words', input = 'text',token = 'words')






#----Document-term and document-feature matrix----

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

# building a tidy data frame (one-token-per-row)

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

# tidy data frame can be used to create a wordcloud
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
description_dtm <- cast_dtm(data = description,document = webpage,term = word,value = n) #list form
description_dfm <- cast_dfm(data = description,document = webpage,term = word,value = n)


description_sparse <- description %>% 
  tidyr::pivot_wider(names_from = word,values_from = n,values_fill = 0) # explicit form
description_sparse # desired input for machine learning methods based on text strings e.g. sentiment analysis etc.









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
z <- x %>% paste0(collapse = ' ') # To collapse the output into a single string
z <- data.frame(text=z) # to change character vector into data.frame required by next function
z %>% unnest_tokens(output = 'words', input = 'text',token = 'words') # from tidytext

library(textstem)
stem_strings(x, language = "porter")

# list of available languages can be found here
SnowballC::getStemLanguages()

# let us use Spanish language
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

# let's compare
c(x[1],lemmatize_strings(x)[1])

# This function uses a dictionary - that is a data frame saying what token should be replaced with what lemma
# By default, data set based on Mechura's (2016) English lemmatization list with over 41 th. rows and 2 variables
 
lexicon::hash_lemmas %>% head(10)

# you can use your own dictionary or generate new one basing own documents with a chosen engine (algorithm)
make_lemma_dictionary(x,engine = 'hunspell',lang = "en_US")

# for other languages you need to download additional file like es_ES.aff
# place it in your working directory
y=c("Es decir, la cardinalidad de la intersección de ambos conjuntos dividida por la cardinalidad de su unión. 
    Siempre toma valores entre 0 y 1, correspondiente este último a la igualdad total entre ambos conjuntos.
    En este sentido 0 significa que las estaciones no presentan especies en común, 
    y tiende a 1 a medida que aumenta el número de especies compartidas.
    En informática se utiliza para medir la distancia entre vectores definidos sobre un espacio vectorial booleano 
    (las componentes del vector sólo pueden ser 0 o 1).)")

setwd("//vmfrze01/DSM/Szkolenia prowadzone przez DSM/Madryt 6-7 czerwca 2023 INE/Text string - script and data")
make_lemma_dictionary(y,engine = 'hunspell',lang = "es_ES")


lemmatize_strings(y,dictionary = make_lemma_dictionary(y,engine = 'hunspell',lang = "es_ES"))










#----Comparing names and addresses of accommodation establishment----

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
# size of datasets
nrow(Survey_rzeszow) # 53
nrow(Webscraping_rzeszow) # 27
# hence 26=53-27 of establishments cannot have a match

# take a look at names
head(Survey_rzeszow$name) # names of establishments and companies -> hard nut to crack
head(Webscraping_rzeszow$name) # only names of establishments

# notice we have an establishment which MUST be matched
Survey_rzeszow$name[5] 
Webscraping_rzeszow$name[1]

# search for more matches
Webscraping_rzeszow$name[4]
grep('AMBASADOR',Survey_rzeszow$name,value = F) #returns indices: row 47
grep('AMBASADOR',Survey_rzeszow$name,value = T) #returns values: hotel `AMBASADOR`

# let's compare names
distances <- stringdistmatrix(Survey_rzeszow$name,Webscraping_rzeszow$name,method = 'lv') # Levensthein
rownames(distances) <- Survey_rzeszow$name
colnames(distances) <- Webscraping_rzeszow$name
head(distances)

distances[5,] 
distances[5,1] # what is wrong? why the distance between obvious match is so large?

Survey_rzeszow$name <- Survey_rzeszow$name %>% tolower()

distances <- stringdistmatrix(Survey_rzeszow$name,Webscraping_rzeszow$name,method = 'lv')
rownames(distances) <- Survey_rzeszow$name
colnames(distances) <- Webscraping_rzeszow$name
head(distances)

distances[5,] # definitely better
distances[5,1] # for the correct match
distances[5,-1] %>% min() # best incorrect
distances[48,] # very good 
distances[48,4] # for the correct match
distances[48,-4] %>% min() # best incorrect




# let's find a match

# since there are less establishments in scraped data, we link survey names to scraped names
bestmatch <- apply(distances, 2, which.min) # searching for minimum in every column, returns index
bestdist <- apply(distances, 2, min) # searching for minimum in every column, returns value
matched <- data.frame(survey=Survey_rzeszow$name[bestmatch],webscraping=Webscraping_rzeszow$name,dist=bestdist,row.names = NULL)

matched$survey %>% unique() %>% length()
matched$survey %>% length()
# hence 15=27-12 establishments are matched at least twice -> that is bad

# let's find a match with a threshold
# match whenever the distance is lower then 10
dist_threshold = 9
matched2 <- matched
matched2$survey[matched2$dist>dist_threshold]=NA
matched2 <- matched2 %>% filter(!is.na(survey))

matched2$survey %>% length()
matched2$survey %>% unique() %>% length()
# 6=14-8 still 6 is duplicated

# let's find a unique match with a threshold 
surv_unique <- unique(matched2$survey)

for(establ in surv_unique)
{
  ind <- which(matched2$survey == establ) # check establ=surv_unique[7]
  if(length(ind)==1){next} # if there is exactly one match, it is OK
  ind <- ind[-which.min(matched2$dist[ind])]
  matched2$survey[ind] <- NA
}
matched2 <- matched2 %>% filter(!is.na(survey))

# results
matched2 # 5 out of 8 are correctly matched

# check drooped matches
matched %>% filter(!(webscraping %in% matched2$webscraping)) # 19 out of 19 are correctly unmatched 


# stats
TP=5 # true positives (matches)
FP=3 # false positives (matches)
TN=19 # true negatives (unmatches)
FN=0 # false negatives (unmatches)

# Confusion matrix and measures
(TP+TN)/nrow(matched) # accuracy
TP/(TP+FP) # precision
TP/(TP+FN) # recall
2*(TP/(TP+FP))*(TP/(TP+FN))/((TP/(TP+FP))+(TP/(TP+FN))) # F1 score



#----Comparing descriptions of hotel accommodation establishment----
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
