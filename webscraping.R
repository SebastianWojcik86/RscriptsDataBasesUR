library(polite)

dat <- bow("https://elections.countyofdane.com/Election-Result/135",
           user_agent = "polite R package", # introduce yourself
           delay = 5, # seconds of delay between scraping attempts - not lower than requested by robots.txt
           times = 3, # number of times to attempt scraping
           force = FALSE,) |> 
  scrape() # reading web page

library(rvest) # package developed by Hadley Wickham’s
httr::with_config(httr::user_agent("I am R-BOT")) # introduce yourself with httr
dat <- read_html("https://elections.countyofdane.com/Election-Result/135")  # you don't care about robots.txt

# Task: get data from tables on this webpage

#in this page there is tag table occurring 3 times
results <- dat |> 
  html_table() # get whole table with single command
# we obtained a list of 3 tables

# retrieving names of tables contained in header 4
names(results) <-
  dat |> 
  html_elements("h4") |> 
  html_text2()

results

# CONGRATULATION! You scraped your first data!

#----Relation between elements----

#https://sscc.wisc.edu/sscc/pubs/webscraping-r/scraping-the-web.html



library(polite)
library(rvest)


countries <- 
  bow("http://www.scrapethissite.com/pages/simple/") |> 
  scrape()
#element type
countries |> html_elements("h3") |> html_text2()

#class
countries |> html_elements(".country-capital") |> html_text2()

#descendants
countries |> html_elements("div h3") |> html_text2()


#logical AND
countries |> html_elements("h3.country-name") |> html_text2() # no space

#logical OR
countries |> html_elements("h3,.country-area") |> html_text2() 

#logical NOT
countries |> html_elements("span:not(.country-area)") |> html_text2() 






#exercises
#1. Select all div elements
#2. Select all elements where class="country-population".
#3.Select all elements with a class attribute.
#4.Select all elements whose class attribute starts with ?c?.
#5.Select all elements whose class attribute ends with ?area?.
#6.Select all elements whose class attribute contains ?p?.
#7.Select all elements that are descendants of a section element.
#8.Select all elements that are children of a body element.
#9.Select all elements that are subsequent siblings of an h3 element.
#10.Select all elements that are next siblings of an h3 element.

#1
countries |> html_elements("div") |> html_text2()
#2
countries |> html_elements(".country-population") |> html_text2()
#3
countries |> html_elements("[class]") |> html_text2()
#4
countries |> html_elements("[class^='c']") |> html_text2()
#5
countries |> html_elements("[class$='area']") |> html_text2()
#6
countries |> html_elements("[class*='p']") |> html_text2()
#7
countries |> html_elements("section *") |> html_text2()
#8
countries |> html_elements("body>*") |> html_text2()
#9
countries |> html_elements("h3~*") |> html_text2()
#10
countries |> html_elements("h3+*") |> html_text2()


#And what does it mean?
countries |> html_elements(".country-capital+*") |> html_text2()
countries |> html_elements(".country-capital~*:not(br)") |> html_text2()
countries |> html_elements("br+*") |> html_text2()
countries |> html_elements("[target]") |> html_text2()
countries |> html_elements("h3>*") |> html_text2()
countries |> html_elements(".row") |> html_text2()
countries |> html_elements("[class*='y']") |> html_text2()
countries |> html_elements("span[class*='y']") |> html_text2()


#----Types of pages----
library(polite)
library(rvest)


#atributes
dat <- 
  bow("http://www.scrapethissite.com/pages/simple/") |> 
  scrape()

dat |> 
  html_elements("[href]") |> 
  html_attr("href")


urls <- 
  dat |> 
  html_elements("a[href]") |> 
  html_attr("href") 

urls_full <-
  ifelse(substr(urls, 1, 1) == "/",
         paste0("http://www.scrapethissite.com", urls),
         urls)

urls_full




#tables
page_with_table <- 
  bow("https://scrapethissite.com/pages/forms/") |> 
  scrape()

library(rvest)

myTable <-
  page_with_table |> 
  html_table()

myTable



#wrangle

dat <-
  bow("https://scrapethissite.com/pages/simple/") |> 
  scrape()

Country <- dat |> html_elements(".country-name") |> html_text2()
Capital <- dat |> html_elements(".country-capital") |> html_text2()
Population <- dat |> html_elements(".country-population") |> html_text2()
Area <- dat |> html_elements(".country-area") |> html_text2()

dat_clean <-
  data.frame(Country = Country,
             Capital = Capital,
             Population = as.numeric(Population),
             Area = as.numeric(Area))

head(dat_clean)


#----Agritourism----

url <- 'https://www.fajnewczasy.pl/noclegi/agroturystyka'
url <- paste0("https://www.fajnewczasy.pl/noclegi/agroturystyka?page=",2)

url2 <- 'https://www.hotels.com/Hotel-Search?adults=2&d1=2023-06-13&d2=2023-06-14&destination=Madrid%2C%20Community%20of%20Madrid%2C%20Spain&endDate=2023-06-14&latLong=40.416784%2C-3.703795&regionId=2198&rooms=1&selected=&semdtl=&sort=RECOMMENDED&startDate=2023-06-13&theme=&useRewards=false&userIntent='
url2 <- 'https://www.hotels.com/Hotel-Search?adults=2&d1=2023-06-13&d2=2023-06-14&destination=Madrid%2C%20Community%20of%20Madrid%2C%20Spain&endDate=2023-06-14&latLong=40.416784%2C-3.703795&regionId=2198&rooms=1&selected=&semdtl=&sort=RECOMMENDED&startDate=2023-06-13'

agro <- read_html(url)  

hotels <- read_html(url2)

# we want to extract prices of establishments


# the first approach -  step-by-step
agro <- agro %>% html_elements('body') %>% html_elements('div.accomodation') %>% 
  html_elements('div.shadow-bottom') %>% html_elements('div') 

agro = agro[[3]] %>% html_elements('div.col-lg-12') %>% html_elements('div.row') %>% html_elements('div.col-12.col-lg-8.col-xl-9')
agro = agro %>% html_elements('.ajax_main') %>% html_elements('.ajax_content')
agro = agro %>% html_elements('.listing.listing-sm-grid') %>% html_elements('.item')%>% html_elements('.row')
agro = agro %>% html_elements('div.col-12.col-md-8.col-lg-7') %>% html_elements('.item-info')%>% html_elements('.item-review-price')
agro = agro %>% html_elements('.item-price') %>% html_element('b')

agro %>% html_text()


# the second approach - quick access but always we get unique elements
agro <- read_html(url)  
agro %>% html_elements('.item-price') %>% html_element('b') %>% html_text() # price 
agro %>% html_elements('.item-location') %>% html_element('a') %>% html_text() # location
agro %>% html_elements('b') %>% html_text() # too much
agro %>% html_elements('.item-price') %>% html_text() # too much
agro %>% html_elements('.item-location') %>% html_text() # TOO MUCH

# links to particular offers
agro %>% html_elements('a[href]') #too much
partial_links <- agro %>% html_elements('h3') %>% html_elements('a[href]') %>% html_attr("href")

links <- paste0('https://www.fajnewczasy.pl',partial_links)
offer <- links[1] %>% read_html()
offer_description <- offer %>% html_elements('div.box') %>% html_elements('p') %>% html_text2()
offer_description[1]


# what about images?
agro <- read_html(url) 
image_url<-agro %>% 
  html_element("div.img-cover") %>% 
  html_element('img') %>% 
  html_attr("src")
image_url

#set working directory
setwd("//vmfrze01/DSM/Szkolenia prowadzone przez DSM/Madryt 6-7 czerwca 2023 INE/Web scriping - script and data")
#save image
download.file(image_url,destfile = 'agro.pic.jpg',mode="wb")



agro <- read_html(url) 
image_url<-agro %>% 
  html_elements("div.img-cover") %>% 
  html_elements('img') %>% 
  html_attr("src") # too much, there gifs and other images
image_url


image_url<-agro %>% 
  html_elements("div.img-cover") %>% 
  html_elements("img[src^='http']") %>% #starting with https
  html_attr("src") # only few? where is the rest?
image_url

agro %>% 
  html_elements("div.img-cover") %>% 
  html_elements('img') #hidden also in data-src class

image_url2<-agro %>% 
  html_elements("div.img-cover") %>% 
  html_elements("img[data-src^='http']") %>% #starting with https
  html_attr("data-src") # only few? where is the rest?
image_url2

image_url_all <- c(image_url,image_url)
stringr::str_extract(image_url_all,'/\\d\\d\\d\\d\\d/') %>% unique() %>% length() #unique offers


#----otomoto.pl----

# General-purpose data wrangling
library(tidyverse)  
# Parsing of HTML/XML files  
library(rvest)    
library(httr)
# String manipulation
library(stringr)   
# Verbose regular expressions
library(rebus)     
# Eases DateTime manipulation
library(lubridate)
library(dplyr)

# ======================================================================================================================================================
n.page=6640 #number of pages



# create lists for every feature with length n.page of otomoto.pl (check it on website) 
name <- vector(mode = 'list',length = n.page)
price <- vector(mode = 'list',length = n.page)    
city <- vector(mode = 'list',length = n.page)
voivodeship <- vector(mode = 'list',length = n.page)
fuel <- vector(mode = 'list',length = n.page)
year <- vector(mode = 'list',length = n.page)
mileage <- vector(mode = 'list',length = n.page)
volume <- vector(mode = 'list',length = n.page)


otomoto.wbsc.df <- c()  # create empty data.frame to fill up with scraped data

for(i in 1:n.page)
{
  url <- paste0("https://www.otomoto.pl/osobowe?page=",i)    # i means the number of page
  # function GET:
    
  otomoto_samochody <- read_html(GET(url,timeout(100)))    # with GET() we handle error of url retrieving
  
  # price                                                           
  price.temp <- otomoto_samochody %>% html_nodes("span.ooa-1bmnxg7.evg565y11") %>% html_text()
  price[[i]] <- price.temp[1:32] # 32 means that we have 32 cars offers on one page
  
  #last element when you copy it by click copy css path this that code of the field
  #span.ooa-1bmnxg7.evg565y11
  
  # name  
  name.temp <- otomoto_samochody %>% html_nodes('h2.er34gjf0')  %>% html_text()
  name[[i]] <- name.temp[1:32]
  
  #city
  city.temp <- otomoto_samochody %>% html_nodes("span.ooa-fzu03x") %>%
    html_text() %>%
    str_split('\\(') %>%
    map_chr(1) %>%   # from divided vectors we choose first element - the city 
    str_trim() 
  city[[i]] <- city.temp[1:32]
  
  # voivodeship
  voivodeship.temp <- otomoto_samochody %>% html_nodes("span.ooa-fzu03x") %>%
    html_text() %>%
    str_split('\\(') %>%    
    map_chr(2)    # from divided vectors we choose 2nd  element - voivodeship
  voivodeship.temp <- substring(voivodeship.temp,1, nchar(voivodeship.temp)-1)   # remove the closing brackets (which were left after dividing the elements of the vector) 
  voivodeship[[i]] <- voivodeship.temp[1:32]
  
  
  fuel.year.mileage.volume <- otomoto_samochody %>% html_nodes("li.ooa-1k7nwcr") %>% html_text() 
  fuel.year.mileage.volume <-  fuel.year.mileage.volume[! fuel.year.mileage.volume == 'Niski przebieg']  # delete no-needed text
  # fuel
  fuel.temp <- grep("^[A-Z]", scan(textConnection(fuel.year.mileage.volume), ""), value=TRUE)
  fuel.temp <- sub("Petrol", "Benzyna", fuel.temp)
  row_odd <- seq_len(length(fuel.temp)) %% 2
  fuel.temp <- fuel.temp[row_odd == 1]
  fuel[[i]] <- fuel.temp[1:32]
  
  # year
  year.temp <- as.numeric(fuel.year.mileage.volume)
  year.temp <- year.temp[!is.na(year.temp)]
  row_odd <- seq_len(length(year.temp)) %% 2
  year.temp <- year.temp[row_odd == 1]
  year[[i]] <- year.temp[1:32]
  
  # mileage
  mileage.temp <- grep("km", fuel.year.mileage.volume, value=T)  
  row_odd <- seq_len(length(mileage.temp)) %% 2
  mileage.temp <- mileage.temp[row_odd == 1]
  mileage[[i]] <- mileage.temp[1:32]
  
  # volume
  volume.temp <- grep("cm3", fuel.year.mileage.volume, value = TRUE)
  row_odd <- seq_len(length(volume.temp)) %% 2
  volume.temp <- volume.temp[row_odd == 1]
  volume[[i]] <- volume.temp[1:32]
  
  
  Sys.sleep(0.1) # checking query break
  
}

# do.call merge all data.frame in one
otomoto.wbsc.df <- do.call(rbind, Map(data.frame, Price=price, Name=name, City=city, 
                                      Voivodeship=voivodeship, Fuel=fuel, Year=year, Mileage=mileage, Engine.volume=volume))

#=========================================================195 000 records===========================================================================================







# further for own reading...
https://phantomjs.org/download.html
# this link gives access to PhantomJS
# when the website or webpage makes use of JavaScript to display the data you're interested in 
# the rvest package misses the required functionality. One solution is to make use of PhantomJS.
# it renders java scripts and produces html which can be read by rvest

# check also RSelenium

# how to enter a userid and password to log into a web site
https://riptutorial.com/r/example/23955/using-rvest-when-login-is-required