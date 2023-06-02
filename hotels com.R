######################################
#
# R-srcipt for combining, analysing and estimating results from Hotels.com.
# It is suitable for all webscraping outputs of the same format.
# 
#
# Author: Sebastian Wójcik, Statistical Office in Rzeszów, s.wojcik@stat.gov.pl
# Efficiency improvements: Boris Frankovič, The Statistical Office of the Slovak Republic, Boris.Frankovic@statistics.sk
#
######################################


# install the packages: tidyr, dplyr, plot 3D and their dependencies. Then load it with next three commands
library(tidyr)
library(readr)
library(dplyr)
library(readxl)

#----loading and preparing survey data and scraped data----

#setting a directory where survey data and scraped data is stored after assigning longitude and lattitude from HERE maps
setwd("//Vmfrze01/dsm/Szkolenia prowadzone przez DSM/Madryt 6-7 czerwca 2023 INE/Geographical coords - script and data")

# preparing scraped data (XY collected for the first time from hotels.com)
ScrapedDataXY <- read_delim("outputFileWS.csv",delim = ";", escape_double = FALSE, 
                            col_types = cols(lat = col_double(),lng = col_double()), trim_ws = TRUE) %>% 
  select(name,lat,lng,searchTerm) %>% 
  `colnames<-`(c('hotelName','lat_new','lon_new','searchTerm'))

nrow(ScrapedDataXY[!is.na(ScrapedDataXY$lat_new),])/nrow(ScrapedDataXY) # % of establishments with XY

# establishments database - originally without XY
data_accom_dest <- read_delim("//vmfrze01/dsm/Szkolenia prowadzone przez DSM/Madryt 6-7 czerwca 2023 INE/Geographical coords - script and data/Establishments by type of accommodation.csv", 
                                 delim = ";", escape_double = FALSE, col_types = cols(Period.NA.20 = col_skip()), 
                                 locale = locale(decimal_mark = ",",encoding = "CP1250"), trim_ws = TRUE)

# force common lower case of names (compare names before)
data_accom_dest$hotelName <- data_accom_dest$hotelName %>% tolower()

# merging datasets to get two sets of XY
data_accom_dest_XY=merge(data_accom_dest,ScrapedDataXY,by="hotelName",all.x = T,all.y = F,sort=FALSE);rm(ScrapedDataXY)
nrow(data_accom_dest_XY[!is.na(data_accom_dest_XY$lat_new),])/nrow(data_accom_dest_XY) # % of establishments with XY
nrow(data_accom_dest_XY[!is.na(data_accom_dest_XY$lat),])/nrow(data_accom_dest_XY) # % of establishments with XY

# whenever coords are not available from ScrapedDataXY we use coords from portal. But in fact they are not equivalent
ind <- which(is.na(data_accom_dest_XY$lat_new)==T)
data_accom_dest_XY$lat_new[ind] <- data_accom_dest_XY$lat[ind]
data_accom_dest_XY$lon_new[ind] <- data_accom_dest_XY$lon[ind] 
nrow(data_accom_dest_XY[is.na(data_accom_dest_XY$lat_new)==F,])/nrow(data_accom_dest_XY) # % of establishments with XY
# 96.8% of establishments with XY - compared to 77% from portal only

# discarding all other establishments - we can't match them without XY at this moment, removing redundant variables
data_accom_dest_XY=data_accom_dest_XY[!is.na(data_accom_dest_XY$lat_new),]
data_accom_dest_XY$lat <- data_accom_dest_XY$lat_new; data_accom_dest_XY$lat_new <- NULL 
data_accom_dest_XY$lon <- data_accom_dest_XY$lon_new; data_accom_dest_XY$lon_new <- NULL


library(readxl)

# preparing frame for big establishments
EstablishmentFrameBig=read_xlsx("outputFileAS.xlsx",sheet = 1,range = "A2:AF12681",col_names = T)
colnames(EstablishmentFrameBig)[4]="hotelName"

# strange sequences of empty  fields (check rows ~67)
# records are somehow cut and need to be repaired
EstablishmentFrameBig=EstablishmentFrameBig[!is.na(EstablishmentFrameBig$index),]
ind=which(is.na(EstablishmentFrameBig$address))
EstablishmentFrameBig$name[ind]=paste(EstablishmentFrameBig$name[ind],EstablishmentFrameBig$index[ind+1])
EstablishmentFrameBig[ind,-(1:4)]=EstablishmentFrameBig[ind+1,2:(ncol(EstablishmentFrameBig)-3)]
EstablishmentFrameBig=EstablishmentFrameBig[-(ind+1),]


# converting character to numeric
EstablishmentFrameBig$lat = EstablishmentFrameBig$lat %>% as.numeric()
EstablishmentFrameBig$lng = EstablishmentFrameBig$lng %>% as.numeric()
# almost all establishments with XY, removing the rest
nrow(EstablishmentFrameBig[!is.na(EstablishmentFrameBig$lat),])/nrow(EstablishmentFrameBig)
EstablishmentFrameBig=EstablishmentFrameBig[is.na(EstablishmentFrameBig$lat)==F & is.na(EstablishmentFrameBig$lng)==F,]



#recoding
EstablishmentFrameBig <- mutate(EstablishmentFrameBig, accommodation_type = case_when(
  accommodation_type %in% c("01","02","03","04") ~ 'Hotels and similar accommodation',
  accommodation_type %in% c("14","15") ~ 'Camping grounds',
  TRUE ~ 'Holiday and other short-stay accommodation'
))

EstablishmentFrameBig <- mutate(EstablishmentFrameBig, seasonal = case_when(
  seasonal %in% c("1") ~ '1',#whole year 
  TRUE ~ '0' #seasonal
))

ind=which(grepl(pattern = 'Month',x = colnames(EstablishmentFrameBig))==T)
for(i in ind)
{
  EstablishmentFrameBig[,i] <- EstablishmentFrameBig[,i] %>% unlist() %>% 
    recode('T'=1,.default = 0)
}


# preparing frame for small establishments
EstablishmentFrameSmall=read_xlsx("outputFileASSmall.xlsx",range = "A1:W10922",col_names = T,col_types = 'text') 

# converting character to numeric
EstablishmentFrameSmall$lat = EstablishmentFrameSmall$lat %>% as.numeric()
EstablishmentFrameSmall$lng = EstablishmentFrameSmall$lng %>% as.numeric()
# almost all establishments with XY, removing the rest
nrow(EstablishmentFrameSmall[!is.na(EstablishmentFrameSmall$lat),])/nrow(EstablishmentFrameSmall)
EstablishmentFrameSmall=EstablishmentFrameSmall[is.na(EstablishmentFrameSmall$lat)==F & is.na(EstablishmentFrameSmall$lng)==F,]



#recoding
EstablishmentFrameSmall$accommodation_type <- 'Hotels and similar accommodation' #there are no hotels and similar object

EstablishmentFrameSmall <- mutate(EstablishmentFrameSmall, seasonal = case_when(
  seasonal %in% c("1") ~ '1',#whole year 
  TRUE ~ '0' #seasonal
))


# binding frames
EstablishmentFrame=bind_rows(EstablishmentFrameBig,EstablishmentFrameSmall)
rm(EstablishmentFrameBig);rm(EstablishmentFrameSmall) # removing previous databases -> save memory


# any establishment without correct XY?
sum(is.na(EstablishmentFrame$lat))
sum(is.na(data_accom_dest_XY$lat))


#----Matching survey data and scraped data by coords----

# first we need to create the spatial objects
# in general, it may be SpatialPoints, SpatialPolygons, SpatialLines, SpatialGrid 
# and version with data frame: SpatialPointsDataFrame, SpatialPolygonsDataFrame, SpatialLinesDataFrame, SpatialGridDataFrame

# SpatialPointsDataFrame
library(sp)
EstablishmentFrame <- SpatialPointsDataFrame(coords = cbind(EstablishmentFrame$lat,EstablishmentFrame$lng),
                                             data = EstablishmentFrame)
data_accom_dest_XY <- SpatialPointsDataFrame(coords = cbind(data_accom_dest_XY$lat,data_accom_dest_XY$lon),
                                             data = data_accom_dest_XY)
# View how this data is structured

# calcuating distances - results in meters
library(geosphere) # function distm

distMatrix <- distm(EstablishmentFrame@coords, data_accom_dest_XY@coords,fun = distHaversine)
EstablishmentFrame$dist_to_nearest_in_WS_Haversine <- apply(distMatrix, 1, min)
EstablishmentFrame$nearest_in_WS_Haversine <- apply(distMatrix, 1, which.min)
data_accom_dest_XY$nearest_in_Frame_Haversine <- apply(distMatrix, 2, which.min)

# the nearest establishment is not defined uniquely. 
data_accom_dest_XY$nearest_in_Frame_Haversine %>% head(11) #look at the first and the last
data_accom_dest_XY$nearest_in_Frame_Haversine %>% duplicated() %>% sum()
# 2776=5514-2738 establishments will be used for matching

# If there are at least two establishments in the frame having the same establishments from the portal assigned
# only one may be valid, the rest of establishments should not have any match
EstablishmentFrame$possible_match=FALSE
EstablishmentFrame$possible_match[data_accom_dest_XY$nearest_in_Frame_Haversine[EstablishmentFrame$nearest_in_WS_Haversine]==(1:nrow(EstablishmentFrame@data))]=TRUE

# let's analyse the distribution of distances
EstablishmentFrame$dist_to_nearest_in_WS_Haversine %>% hist()
# something odd occurs -> outlier
max(EstablishmentFrame$dist_to_nearest_in_WS_Haversine) #OMG 9.6 th kilometers

# let's cut 
EstablishmentFrame@data %>% filter(dist_to_nearest_in_WS_Haversine<2000) %>% 
  select(dist_to_nearest_in_WS_Haversine) %>% unlist() %>% hist()

# we need a threshold distance - elsewhere we shall produce lots of false matches


# merging by given two threshold distance for two group: big and small establishments
# inputs are SpatialPointsDataFrame with the following variables
#   dist_to_nearest_in_WS_Haversine 
#   nearest_in_WS_Haversine
#   nearest_in_Frame_Haversine 

mergeByTreshold <- function(treshold=c(80,30),data_accom_dest_XY,EstablishmentFrame) #spatial dataframes with distances included
{
  EstablishmentFrame$dist_below_treshold = FALSE
  EstablishmentFrame$dist_below_treshold[EstablishmentFrame$accommodation_size=="Big" & EstablishmentFrame$dist_to_nearest_in_WS_Haversine <  treshold[1] & EstablishmentFrame$possible_match==TRUE] = TRUE
  EstablishmentFrame$dist_below_treshold[EstablishmentFrame$accommodation_size=="Small" & EstablishmentFrame$dist_to_nearest_in_WS_Haversine <  treshold[2] & EstablishmentFrame$possible_match==TRUE] = TRUE
  
  both=cbind(EstablishmentFrame[EstablishmentFrame$dist_below_treshold == TRUE,],data_accom_dest_XY[EstablishmentFrame$nearest_in_WS_Haversine[EstablishmentFrame$dist_below_treshold == TRUE],])
  
  data_accom_dest_XY=data_accom_dest_XY[-EstablishmentFrame$nearest_in_WS_Haversine[EstablishmentFrame$dist_below_treshold == TRUE],]
  
  EstablishmentFrame=EstablishmentFrame[EstablishmentFrame$dist_below_treshold == FALSE,]
  
  #  binding all dataframes
  completeDataBase=bind_rows(both@data,data_accom_dest_XY@data,EstablishmentFrame@data)
  completeDataBase$Source=c(rep("Portal_Frame",nrow(both@data)),rep("Portal",nrow(data_accom_dest_XY@data)),rep("Frame",nrow(EstablishmentFrame@data)))
  return(completeDataBase)
}

# matching
completeDataBase <- mergeByTreshold(treshold=c(80,20),data_accom_dest_XY,EstablishmentFrame)  

#there are some redundant columns
completeDataBase$destinationName.1=NULL # all redundant columns can be removed by NULL
completeDataBase$destinationName=gsub("Województwo","",completeDataBase$destinationName)
completeDataBase$destinationName=gsub("Woj.","",completeDataBase$destinationName)
completeDataBase$destinationName=gsub(", Polska","",completeDataBase$destinationName)




#----Data imputation with Decision Tree----



#reading again the completeDataBase
#completeDataBase=read.csv2("//VMFRZE01/ESSnet BDII-WPJ/Zbiory danych/Dezagregacja/Establishments_joint_database_B80_S20.csv")

#picking variables
data <- completeDataBase %>% 
  select('accommodation_size',"accommodation_type","seasonal",starts_with('Month'), # data from official statistics
         "price","starRating","guestReviewsRating","guestReviewsTotal", # scraped data
         "accType","accType_Stat","NACE",starts_with('Period.'),'Source')

# preparing training set 
data_learn=data[data$Source=="Portal_Frame",]
data_imp=data[data$Source=="Portal",16:ncol(data)]

# NA to levels in testing set which are not present in the training set
data_imp$accType[which((data_imp$accType %in% data_learn$accType)==FALSE)]=NA
data_imp$accType_Stat[which((data_imp$accType_Stat %in% data_learn$accType_Stat)==FALSE)]=NA
data_imp$NACE[which((data_imp$NACE %in% data_learn$NACE)==FALSE)]=NA
data_imp$Source[which((data_imp$Source %in% data_learn$Source)==FALSE)]=NA




#These variables are going to be imputed (15 variables)
'accommodation_size',"accommodation_type","seasonal",
"Month1","Month2","Month3","Month4","Month5","Month6",
"Month7","Month8","Month9","Month10","Month11","Month12"
#other variables are explanatory variables

# data imputation packages
library(rpart) #CART
library(rpart.plot) #tree plot - it is not pretty...
library("OptimalCutpoints") # to find the optimal cut point

# to find optimal cutpoint we will use Youden's J statistic
# J = sensitivity + specificity -1
# sensitivity is a measure of how well a test can identify true positives 
# specificity is a measure of how well a test can identify true negatives
# higher sensitivity will mean lower specificity and vice versa

# accommodation_size
tree <- rpart(accommodation_size~., data=data_learn[,c(1,16:ncol(data_learn))],  
              control = rpart.control(minsplit = 5, cp = 0.01,xval = 10)) 

# tree statistics
rsq.rpart(tree) #first plot - R2 on training set and R2 from cross-validation, 
                #second plot - model error / null model error (relative error) from model
plotcp(tree) # cp level that generates a new split, and relative error from cross-validation
printcp(tree) # relative error from model, relative error from cross-validation with its standard deviation
summary(tree) # tons of details, hard to read

# redo lines 256-263 tarting with cp = 0.001 and xval = 50

pred_tree <- predict(tree, data_learn[,c(1,16:ncol(data_learn))], type="prob")[,2]
pref_df <- data.frame(pred = pred_tree, truth = data_learn$accommodation_size)
oc <- optimal.cutpoints(X = "pred", status = "truth", methods="Youden", data=pref_df, tag.healthy = "Big")
summary(oc) # all statistics from the confusion matrix
plot(oc, which=1) #ROC curve, which=1 sets 'Specificity' on OX axis

rpart.plot(tree,cex = 0.6,type = 0,varlen = 0,fallen.leaves =F) # tree plot

ind <- which(is.na(completeDataBase$accommodation_size)==T)
completeDataBase$accommodation_size[ind] <- predict(tree,data_imp,type = 'class')


# accommodation_type (NACE)
tree <- rpart(accommodation_type~., data=data_learn[,c(2,16:ncol(data_learn))],  
              control = rpart.control(minsplit = 5, cp = 0.01))

pred_tree <- predict(tree, data_learn[,c(2,16:ncol(data_learn))], type="prob")[,2]
pref_df <- data.frame(pred = pred_tree, truth = data_learn$accommodation_type)
oc <- optimal.cutpoints(X = "pred", status = "truth", methods="Youden", data=pref_df, tag.healthy = "Hotels and similar accommodation") #in this approach we can check stats for one selected label against all other
summary(oc) # all statistics from the confusion matrix
plot(oc, which=1) #ROC curve, which=1 sets 'Specificity' on OX axis

oc <- optimal.cutpoints(X = "pred", status = "truth", methods="Youden", data=pref_df, tag.healthy = "Holiday and other short-stay accommodation") #in this approach we can check stats for one selected label against all other
summary(oc) # all statistics from the confusion matrix
plot(oc, which=1) #ROC curve, which=1 sets 'Specificity' on OX axis

rpart.plot(tree,cex = 0.6,type = 0,varlen = 0,fallen.leaves =F)
#rpart.plot(tree)

ind <- which(is.na(completeDataBase$accommodation_type)==T)
completeDataBase$accommodation_type[ind] <- predict(tree,data_imp,type = 'class')


# seasonal (1 means operating whole year, 0 - seasonal)
data_learn$seasonal <- as.factor(data_learn$seasonal)
tree <- rpart(seasonal~., data=data_learn[,c(3,16:ncol(data_learn))],  
              control = rpart.control(minsplit = 5, cp = 0.01))

pred_tree <- predict(tree, data_learn[,c(3,16:ncol(data_learn))], type="prob")[,2]
pref_df <- data.frame(pred = pred_tree, truth = data_learn$seasonal)
oc <- optimal.cutpoints(X = "pred", status = "truth", methods="Youden", data=pref_df, tag.healthy = '0') 
summary(oc) # all statistics from the confusion matrix
plot(oc, which=1) #ROC curve, which=1 sets 'Specificity' on OX axis

rpart.plot(tree,cex = 0.6,type = 0)

ind <- which(is.na(completeDataBase$seasonal)==T)
completeDataBase$seasonal[ind] <- predict(tree,data_imp,type = 'class')

