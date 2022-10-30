
##########################################################################
## Import the cleaned_listing.csv
##########################################################################

## Below is descriptive summary (EDA) for listing and Calendar

listingDF <- read.csv("/Users/unicornnnnn/Desktop/cleaned_listings.csv")

listingDF <- select(cleaned_listing, price, id,name,longitude,latitude,instant_bookable,host_id,host_name,host_since,
                    host_response_time,review_scores_rating,property_type,room_type,accommodates,bathrooms,bedrooms,beds,reviews_per_month,
                    amenities,number_of_reviews)

listingDF$host_response_time_num <- rep(0,length(listingFinal$host_response_time))
listingDF$host_response_time_num[listingFinal$host_response_time=="within a few hours"] <- 0
listingDF$host_response_time_num[listingFinal$host_response_time=="within an hour"] <- 1
listingDF$host_response_time_num[listingFinal$host_response_time=="within a day"]<-2
listingDF$host_response_time_num[listingFinal$host_response_time=="a few days or more"] <- 3

### Create a correlation matrix
nums <- unlist(lapply(listingDF, is.numeric), use.names = FALSE) 
nums <- listingDF[nums]

str(listingFinal)
CorMatrix <- cor(nums,  use="pairwise.complete.obs")
library(corrplot)
corrplot(CorMatrix, method = "color", tl.col="black", main ="Correlation Plot")

### Correlation table
installpkg("ggplot2")
installpkg("GGally")
library(ggplot2)
library(GGally)
ggpairs(nums)
ggpairs(data.holdout[,c(2,3,5,18,20)])

ggpairs(nums, columns = 4:8, aes(color = Species, alpha = 0.5))

# Analyzing the listings based on room types. It is stated in AirBnB's website that they have 3 room types.
room<-data.frame(dplyr::count(listingDF, room_type, sort = TRUE))
library(ggplot2)
dev.off()
room$room_type <- factor(room$room_type, levels = room$room_type[order(room$n, decreasing = TRUE)])
ggplot(data=room, aes(x=room_type, y=n, fill=room_type, label = n)) + geom_bar(stat="identity") + ggtitle("Florida Airbnb Room Types") + geom_text(size = 3)



# Anaylzing the listings based on the number of bedrooms.
boxplot(price ~ bedrooms, data=listingDF, main="Boxplot of price and bedrooms",
        xlab="Number of bedrooms", ylab="Price")


listingDF$bedrooms <- as.character(listingDF$bedrooms)
listingDF$bedrooms <- as.numeric(listingDF$bedrooms)

ggplot(listingDF, aes(x=bedrooms, y=price, fill=bedrooms)) + 
    geom_boxplot(alpha=0.3) +
    theme(legend.position="none")


# Analyzing the mean price based on the host response time.
boxplot(price ~ host_response_time, data=listingDF, main="Boxplot of price and host response time",
        xlab="Host Response Time", ylab="Price")
library(ggplot2)
ggplot(listingDF, aes(x=host_response_time, y=price, fill=host_response_time)) + 
    geom_boxplot(alpha=0.3) +
    theme(legend.position="none")

# Analyzing the listings based on the property type.
# Do not use this because there're so many property types...
property<-data.frame(dplyr::count(listingDF, property_type, sort = TRUE))
property$property_type <- factor(property$property_type, levels = property$property_type[order(property$n, decreasing = TRUE)])
ggplot(data=property, aes(x=property_type, y=n, color=property_type)) + geom_bar(stat="identity") + ggtitle("Florida Airbnb Room Types")


property<-data.frame(dplyr::count(listingDF, property_type, sort = TRUE))
property <- property[1:10,]
property
# property_type     n
# 1                   Entire rental unit 13222
# 2                          Entire home  7426
# 3                         Entire condo  6180
# 4              Entire residential home  5885
# 5           Entire condominium (condo)  5063
# 6                         Entire villa  1630
# 7                 Private room in home  1544
# 8            Entire serviced apartment  1541
# 9     Private room in residential home  1432
# 10                  Entire guest suite  1193
ggplot(data=property, aes(x=property_type, y=n, fill=property_type, label = n)) + 
    geom_bar(stat="identity") + 
    ggtitle("Florida Airbnb Property Type") + 
    geom_text(size = 3)


# Default Heatmap

property<-data.frame(dplyr::count(listingDF, property_type, sort = TRUE))
property <- property$property_type[property$n >1000]

listingDF_1 <- listingDF[listingDF$property_type == "Entire rental unit" | 
                             listingDF$property_type == "Entire home" | 
                             listingDF$property_type == "Entire condo"|
                             listingDF$property_type == "Entire residential home" |
                             listingDF$property_type == "Entire condominium (condo)" |
                             listingDF$property_type == "Private room in home" |
                             listingDF$property_type == "Entire villa"|
                             listingDF$property_type == "Private room in residential home"|
                             listingDF$property_type == "Entire serviced apartment"|
                             listingDF$property_type == "Entire guest suite",]


df1 <- data.frame(aggregate(listingDF_1$price, by=list(listingDF_1$room_type, listingDF_1$property_type), FUN=mean))
name <- c("Entire condo", "Entire condominium (condo)","Entire guest suite","Entire home","Entire rental unit",
          "Entire residential home","Entire serviced apartment","Entire villa","Private room in home","Private room in residential home")


## Heapmap of mean price for different room type and property type
ggplot(df1, aes(Group.1,Group.2)) + geom_tile(aes(fill=x)) + geom_text(aes(label = round(x, 1)), colour="white") + scale_fill_gradient(low = "light blue", high="blue") + ggtitle("Heatmap of mean price for different room type and property type")+ labs(y="Property Type",x = "Room Type")

## plot of # of reviews vs. price
model1 <- lm(price ~ number_of_reviews, data = listingDF)
summary(model1)

library(ggplot2)
ggplot(listingDF, aes(x=number_of_reviews, y=price)) + 
    geom_point()+
    geom_smooth(method=lm)

#####################################
### Desriptive Sumary for Calendr ###
#####################################
Cal <- read.csv("/Users/unicornnnnn/Desktop/DS_data/FINAL_cleaned_calendar_new.csv")
## Create day_of_week
Cal$day_of_week <- strftime(Cal$date, "%A")

## Create weekend dummy variable
Cal$weekend <- ifelse(Cal$day_of_week == "Saturday" | Cal$day_of_week == "Sunday", 1, 0)
summary(Cal)

# Compare prices and # of listings between months
# Seasonal Change of Price and # of Available Listings

Cal$Month1 <- factor(Cal$Month)
library(plyr)

df_pri <- data.frame(aggregate(Cal$price, by=list(Cal$Month), FUN=mean))
df_ava <- data.frame(aggregate(Cal$available_dummy, by=list(Cal$Month), FUN=sum))

colnames(df_pri) <- c("Month","avg_price")
colnames(df_ava) <- c("Month","number_of_ava")

df_both <- cbind(df_pri,df_ava$number_of_ava)
colnames(df_both) <- c("Month","avg_price","number_of_ava")



################
# Draw first plot using axis y1
par(mar=c(5, 4, 4, 6) + 0.1)             

## Plot first set of data and draw its axis
plot(df_both$Month, df_both$avg_price,axes=FALSE,type="b", xlab="", ylab="", 
     col="black", main="Seasonal Change of Price and # of Available Listings",lwd=3.0)
axis(2, ylim=c(0,1),col="black",las=1)  ## las=1 makes horizontal labels
mtext("Average Price",side=2,line=2.5)
box()

# set parameter new=True for a new axis
par(new = TRUE)        


## Plot the second plot and put axis scale on right
plot(df_both$Month, df_both$number_of_ava,type="b", xlab="", ylab="",
     axes=FALSE, col="red",lwd=3.0)
## a little farther out (line=4) to make room for labels
mtext("Number of Available Lisings",side=4,col="red",line=4) 
axis(4, ylim=c(0,7000), col="red",col.axis="red",las=1)

## Draw the time axis
axis(1,pretty(range(df_both$Month),12))
mtext("Month",side=1,col="black",line=2.5)  

## Add Legend
legend("topright",legend=c("Average Price","Number of Available Listings"),
       text.col=c("black","red"),pch=c(16,15),col=c("black","red"))


#############################
# Weekly Change of Price and # of Available Listings
library(plyr)

Cal$day_num <- as.integer(factor(Cal$day_of_week,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),ordered=TRUE))

df_pri1 <- data.frame(aggregate(Cal$price, by=list(Cal$day_num), FUN=mean))
df_ava1 <- data.frame(aggregate(Cal$available_dummy, by=list(Cal$day_num), FUN=sum))

colnames(df_pri1) <- c("day_of_week","avg_price")
colnames(df_ava1) <- c("day_of_week","number_of_ava")

df_both1 <- cbind(df_pri1,df_ava1$number_of_ava)
colnames(df_both1) <- c("day_of_week","avg_price","number_of_ava")



################
# Draw first plot using axis y1
par(mar=c(5, 4, 4, 6) + 0.1)             

## Plot first set of data and draw its axis
plot(df_both1$day_of_week, df_both1$avg_price,axes=FALSE,type="b", xlab="", ylab="", 
     col="black", main="Weekly Change of Price and # of Available Listings",lwd=3.0)
axis(2, ylim=c(0,1),col="black",las=1)  ## las=1 makes horizontal labels
mtext("Average Price",side=2,line=2.5)
box()

# set parameter new=True for a new axis
par(new = TRUE)        


## Plot the second plot and put axis scale on right
plot(df_both1$day_of_week, df_both1$number_of_ava,type="b", xlab="", ylab="",
     axes=FALSE, col="red",lwd=3.0)
## a little farther out (line=4) to make room for labels
mtext("Number of Available Lisings",side=4,col="red",line=4) 
axis(4, ylim=c(0,7000), col="red",col.axis="red",las=1)

## Draw the time axis
axis(1,pretty(range(df_both1$day_of_week),6))
mtext("Day",side=1,col="black",line=2.5)  

## Add Legend
legend("left", legend=c("Average Price","Number of Available Listings"),
       text.col=c("black","red"),pch=c(16,15),cex=0.8,col=c("black","red"))


setwd("/Users/jiayunling/Desktop/Desktop_JL/DS_Florida_airbnb_data")
cleaned <- read.csv("cleaned_listings.csv")
install.packages("Hmisc")
library(Hmisc)
library(ggplot2)

str(cleaned)
######### check property type ##########
unique(cleaned$property_type)
response<-data.frame(dplyr::count(cleaned, property_type, sort = TRUE))
response <- response[1:10,]
response
# property_type     n
# 1                   Entire rental unit 13222
# 2                          Entire home  7426
# 3                         Entire condo  6180
# 4              Entire residential home  5885
# 5           Entire condominium (condo)  5063
# 6                         Entire villa  1630
# 7                 Private room in home  1544
# 8            Entire serviced apartment  1541
# 9     Private room in residential home  1432
# 10                  Entire guest suite  1193
ggplot(data=response, aes(x=property_type, y=n, fill=property_type, label = n)) + geom_bar(stat="identity") + ggtitle("Florida Airbnb Property Type") + geom_text(size = 3)
######## check host_response_time ########
unique(cleaned$host_response_time)
# histogram of host response time frequency
library(ggplot2)
cleaned <- cleaned[!(cleaned$host_response_time == ""), ]
cleaned <- cleaned[!(cleaned$host_response_time == 0), ]
response2<-data.frame(dplyr::count(cleaned, host_response_time, sort = TRUE))
response2
dev.off()
# room$room_type <- factor(room$room_type, levels = room$room_type[order(room$n, decreasing = TRUE)])
ggplot(data=response2, aes(x=host_response_time, y=n, fill=host_response_time, label = n)) + geom_bar(stat="identity") + ggtitle("Florida Airbnb Host Reponse Time") + geom_text(size = 3)
# we can see the majority of the hosts respond within an hour

# plot boxplot with y = mean(price)
library(ggplot2)
ggplot(cleaned, aes(x=host_response_time, y=price, fill=host_response_time)) + 
    geom_boxplot(alpha=0.3) +
    theme(legend.position="none")+ ggtitle("Florida Airbnb Response Time")

# plot heatmap with property type and room type
# calculate the mean with each combination of prop type and room type
bed<-data.frame(dplyr::count(cleaned, bedrooms, sort = TRUE))

hm <- cleaned[cleaned$property_type == "Entire rental unit" | 
                  cleaned$property_type == "Entire home" | 
                  cleaned$property_type == "Entire condo"|
                  cleaned$property_type == "Entire residential home" |
                  cleaned$property_type == "Entire condominium (condo)" |
                  cleaned$property_type == "Private room in home" |
                  cleaned$property_type == "Entire villa"|
                  cleaned$property_type == "Private room in residential home"|
                  cleaned$property_type == "Entire serviced apartment"|
                  cleaned$property_type == "Entire guest suite",]
hm <- hm[hm$bedrooms == 1 |hm$bedrooms == 2 |hm$bedrooms == 3|
             hm$bedrooms == 4 |hm$bedrooms == 5 |hm$bedrooms == 6|
             hm$bedrooms == 7,]

# Heatmap for mean price in bedrooms and property type
hm1 <- aggregate(hm$price, by=list(hm$bedrooms, hm$property_type), FUN=mean)
hm1

ggp <- ggplot(hm1, aes(Group.1,Group.2)) + geom_tile(aes(fill=x)) + geom_text(aes(label = round(x, 1)), color = "white") + scale_fill_gradient(low = "pink", high = "black") + labs(y = "Property Type", x = "Bedrooms") + ggtitle("Heatmap of mean price in different property types and bedrooms")
ggp