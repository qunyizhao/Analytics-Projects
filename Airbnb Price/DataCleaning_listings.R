###########################################
### Data Science for Business
### Final Group Proj -- Airbnb Price Evaluation
### Section C, Team 31

### Hoa Anh Hoang (hh274)
### Jiayun Ling (jl1170)
### Junaid Mohammed (jm855)
### Yidi Wu (yw547)
### Ariel Zhao (qz93) 

### Do the data cleaning:
### Part1: listing.csv file

listingsSeptember22 <- read.csv("/Users/unicornnnnn/Desktop/ListingsSep22.csv")
listingsSeptember22 <- subset(listingsSeptember22, select = -c(source) )

listingsJune22 <- read.csv("/Users/unicornnnnn/Desktop/ListingsJune22csv.csv")
listingsMarch22 <- read.csv("/Users/unicornnnnn/Desktop/ListingsMarch22.csv")
listingsDecember21 <- read.csv("/Users/unicornnnnn/Desktop/ListingsDecember21.csv")
listingFinal <- rbind(listingsJune22, listingsMarch22, listingsDecember21,listingsSeptember22)
### Check whether there're any duplicates after combining three quarters
#listingFinal[duplicated(listingFinal$id)]
#View(listingFinal)
#na.omit(listingFinal)
listingFinal[listingFinal =='N/A' |listingFinal == "NA" | is.na(listingFinal)]<- 0

### Data Cleaning of listingFinal
### 1. Drop useless information
### 1.1 Drop listing_url, description, neighborhood_overview, picture_url, host_url,
### host_about, host_thumbnail_url, host_picture_url, host_neighbourhood, neighbourhood,
### neighbourhood_group_cleansed, bathrooms, calendar_updated, license
#Delete long text descriptive columns such as url, neighbourhood information
library(dplyr)
listingFinal <- select(listingFinal,-contains("url"))
#listingFinal <- select(listingFinal,-contains("neighbourhood"))
#listingFinal <- select(listingFinal,-contains("neighborhood"))
listingFinal <- select(listingFinal,-contains("description"))
listingFinal <- select(listingFinal,-contains("host_about"))
listingFinal <- select(listingFinal,-contains("calendar_updated"))
listingFinal <- select(listingFinal,-contains("license"))
listingFinal <- select(listingFinal, -"bathrooms")
#listingFinal <- select(listingFinal, -contains("calculated"))
listingFinal <- select(listingFinal, -contains("minimum_minimum_nights"))
listingFinal <- select(listingFinal, -contains("maximum_minimum_nights"))
listingFinal <- select(listingFinal, -contains("maximum_maximum_nights"))
listingFinal <- select(listingFinal, -contains("minimum_maximum_nights"))
listingFinal <- select(listingFinal, -contains("minimum_nights_avg_ntm"))
listingFinal <- select(listingFinal, -contains("maximum_nights_avg_ntm"))

View(listingFinal)
### 1.2 Removes rows missing price data since we are interested in price variability
#sum(is.na(listingFinal$Price))
#sum(listingFinal$Price == "N/A")

### 2. Check data types of each column (e.g., convert any strings to numbers;
### convert characters into date...)
str(listingFinal)
### 2.1 Change last_scraped, host_since, first_review, last_review, calendar_last_scraped, all the date variables 
### from CHAR into DATE type
listingFinal$last_scraped <- as.Date(c(listingFinal$last_scraped))
listingFinal$host_since <- as.Date(c(listingFinal$host_since))
listingFinal$calendar_last_scraped <- as.Date(c(listingFinal$calendar_last_scraped))
listingFinal$first_review <- as.Date(c(listingFinal$first_review))
listingFinal$last_review <- as.Date(c(listingFinal$last_review))

### 2.2 Convert dollar dtype to float: price
#Remove the dollar sign of price
listingFinal$price = gsub("\\$", "", listingFinal$price)
#Convert the price of listing from strings to floats
listingFinal$price <- as.numeric(factor(listingFinal$price))

### 2.3 Convert N/A string into 0
#listingFinal[listingFinal =='N/A' | listingFinal =='N/A' | is.na(listingFinal)]<- 0

### 2.4 Convert character into number

listingFinal$host_response_rate <-as.numeric(sub("%", "", listingFinal$host_response_rate))/100
listingFinal$host_acceptance_rate <-as.numeric(sub("%", "", listingFinal$host_acceptance_rate))/100


### 3. Dealing with character data type
### 3.1 Change host_location change into 3 columns
library(stringr)
listingFinal[c('host_city', 'host_states', 'host_country')] <- str_split_fixed(listingFinal$host_location, ',', 3)
#listingFinal <- cbind(listingFinal, listingFinal[c('host_city', 'host_states', 'host_country')])

### Dummy !!!
### 3.2 Convert host_response_time into different numerical types
#listingFinal$host_response_time_num <- rep(0,length(listingFinal$host_response_time))
#listingFinal$host_response_time_num[listingFinal$host_response_time=="within a few hours"] <- 0
#listingFinal$host_response_time_num[listingFinal$host_response_time=="within an hour"] <- 1
#listingFinal$host_response_time_num[listingFinal$host_response_time=="within a day"]<-2
#listingFinal$host_response_time_num[listingFinal$host_response_time=="a few days or more"] <- 3

### 3.3 Convert all the t/f volumns into 0/1. They are: host_is_superhost,
### host_has_profile_pic, host_identity_verified, has_availability, 
### instant_bookable and room type
#Change textual data t or f into dummies 1 or 0
colnames(listingFinal)
#host_is_superhost; host_has_profile_pic; host_identity_verified; has_availability; instant_bookable; room_type
listingFinal$host_is_superhost <- ifelse(listingFinal$host_is_superhost=="t",1,0)
listingFinal$host_has_profile_pic <- ifelse(listingFinal$host_has_profile_pic=="t",1,0)
listingFinal$host_identity_verified <- ifelse(listingFinal$host_identity_verified=="t",1,0)
listingFinal$has_availability <- ifelse(listingFinal$has_availability=="t",1,0)
listingFinal$instant_bookable <- ifelse(listingFinal$instant_bookable=="t",1,0)
#listingFinal$room_type <- ifelse(listingFinal$room_type=="Entire home/apt",1,0)
listingFinal$host_location_dummy <-grepl("Florida", listingFinal$host_location, fixed=TRUE)
listingFinal$host_location_dummy <- ifelse(listingFinal$host_location_dummy == TRUE, 1, 0)

#length(regmatches(listingFinal$amenities, gregexpr(",", listingFinal$amenities)))

listingFinal$num_amenities <- str_count(listingFinal$amenities, ",")+1


### 3.4 Convert bathroom_text into numerical number
listingFinal$bathrooms <-as.numeric(substr(listingFinal$bathrooms_text, 1 , 1))



### Delete some irrelevant variables
listingDF <- subset(listingFinal, select = -c(scrape_id, name, host_name, host_listings_count, 
                                                 latitude, longitude, availability_60,  availability_90, 
                                                 number_of_reviews_ltm, number_of_reviews_l30d, 
                                                host_neighbourhood, neighbourhood, 
                                                neighbourhood_group_cleansed, neighborhood_overview) )
# Remove listings with 0 for  bedrooms, bathrooms, accomodates, price, beds, review_scores_rating, reviews_per_month
listingDF = listingDF[,"listingDF$bedrooms" > 0]
listingDF = listingDF[,"listingDF$bathrooms" > 0]
listingDF = listingDF[,"listingDF$accommodates" > 0]
listingDF = listingDF[,"listingDF$price" > 0]
listingDF = listingDF[,"listingDF$beds" > 0]
listingDF = listingDF[,"listingDF$review_scores_rating" > 0]
listingDF = listingDF[,"listingDF$reviews_per_month" > 0]

write.csv(listingDF, "/Users/unicornnnnn/Desktop/cleaned_listings.csv", row.names=FALSE)



