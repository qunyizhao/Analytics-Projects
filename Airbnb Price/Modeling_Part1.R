###########################################
### Data Science for Business
### Final Group Proj -- Airbnb Price Evaluation
### Section C, Team 31

### Hoa Anh Hoang (hh274)
### Jiayun Ling (jl1170)
### Junaid Mohammed (jm855)
### Yidi Wu (yw547)
### Ariel Zhao (qz93) 

############################################################################
### Modeling Part 1 using listing file with price as target variable #######
############################################################################

## For the whole Modeling_Part1, we only use "cleaned_listings.csv" file. No other files.
listingDF <-read.csv("cleaned_listings.csv")


## Kind tips: Regression2, k-means plot, random forest for regression may take 
## you about 30 minutes to run.

library(glmnet)
library(stringr)
library(plfm)
library(devtools)
install_github("vqv/ggbiplot")
library(ggplot2)
library(ggbiplot)
library(ggrepel)
library(factoextra)
library(dplyr)
library(olsrr)


## divide into train and test
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(listingDF), replace=TRUE, prob=c(0.8,0.2))
train_listingDF  <- listingDF[sample, ]
test_listingDF   <- listingDF[!sample, ]

dim(train_listingDF)
dim(test_listingDF)

## We first use "cleaned_listings.csv" file to select some significant variables 
## using forward/backward stepwise methods (regression 2) and keep all selected 
## variables (regression 3 and listingDF_significant)

## regression 1: with all variables
regression1 <- lm(price ~host_response_time + host_response_rate +host_location_dummy
                  + host_acceptance_rate +host_is_superhost+ host_total_listings_count
                  + host_has_profile_pic
                  + room_type +accommodates +bathrooms +bedrooms 
                  + beds +num_amenities +number_of_reviews+review_scores_rating
                  +calculated_host_listings_count + calculated_host_listings_count_entire_homes
                  +calculated_host_listings_count_private_rooms 
                  +calculated_host_listings_count_shared_rooms 
                  +reviews_per_month+ host_identity_verified+has_availability
                  + review_scores_cleanliness +instant_bookable
                  +review_scores_checkin
                  +review_scores_communication +review_scores_location
                  +review_scores_value, data =train_listingDF)
summary(regression1)

predict.regression1 <-as.data.frame(predict(regression1, data =train_listingDF))


### regression 2 use forward and backward method to select data from regression1
regression2 <- olsrr::ols_step_both_p(regression1, pent = 0.05, prem = 0.05)

summary(regression2)

## regression 3 contains all variables with p-value < 0.05 concluded from regression 2
# excluding: neighborhood_cleansed +host_location_dummy(cause: too much variables)
regression3 <- lm(price ~bathrooms+number_of_reviews+room_type+accommodates
                  +bedrooms+calculated_host_listings_count_private_rooms
                  +host_total_listings_count+review_scores_location+review_scores_checkin
                  +review_scores_location+review_scores_rating
                  +calculated_host_listings_count_entire_homes
                  +reviews_per_month
                  +calculated_host_listings_count_shared_rooms
                  +calculated_host_listings_count
                  +num_amenities, data = train_listingDF)
summary(regression3)


## listingDF_significant: dataset result from regression 3, which 22 variables are significant except for neighborhood_cleansed
listingDF_significant<-subset(listingDF, select = c(bathrooms,number_of_reviews,room_type,accommodates
                                                    ,bedrooms,calculated_host_listings_count_private_rooms
                                                    ,host_total_listings_count,review_scores_location
                                                    ,review_scores_checkin
                                                    ,review_scores_location,review_scores_rating
                                                    ,calculated_host_listings_count_entire_homes
                                                    ,reviews_per_month
                                                    ,calculated_host_listings_count_shared_rooms
                                                    ,calculated_host_listings_count
                                                    ,num_amenities))
listingDF_significant<-na.omit(listingDF_significant)


## PCA
## make all variables in listingDF_significant numeric and ready for PCA
num_cols <-unlist(lapply(listingDF_significant, is.numeric))## whether the column is numeric or not, choose only numeric columns

pca.data <- listingDF_significant[, num_cols]
pca.data <- na.omit(pca.data)

## PCA (from all numeric variables except for id, scrape_id, host_id,price)
pca.listingDF <-prcomp(pca.data,scale = TRUE,center = TRUE)
## pca.listingDF <-prcomp(pca.data, scale = TRUE, center= TRUE, retx = T) is not that useful
summary(pca.listingDF)

### plot pca scree plot
fviz_eig(pca.listingDF, col.var ="salmon")

# Spikey plot for PCA

ggbiplot(pca.listingDF)


### loadings analysis

loadings <- pca.listingDF$rotation[,1:4]
#### Loading 1
v<-loadings[order(abs(loadings[,1]), decreasing=TRUE)[1:15],1]
loadingfit <- lapply(1:15, function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]

#### Loading 2
v<-loadings[order(abs(loadings[,2]), decreasing=TRUE)[1:15],2]
loadingfit <- lapply(1:15, function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]

#### Loading 3
v<-loadings[order(abs(loadings[,3]), decreasing=TRUE)[1:15],3]
loadingfit <- lapply(1:15, function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]

#### Loading 4
v<-loadings[order(abs(loadings[,4]), decreasing=TRUE)[1:15],4]
loadingfit <- lapply(1:15, function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]


### K-means
set.seed(123)
num_cols <-unlist(lapply(listingDF_significant, is.numeric))
kmeans.data <- listingDF_significant[, num_cols]

kmeans.data <- scale(kmeans.data)
kmeans.listingDF <- kmeans(kmeans.data,2, nstart = 25)
kmeans.listingDF


### aggregate result
aggregate(listingDF_significant,by=list(cluster=kmeans.listingDF$cluster), mean)

### Plot K-means

fviz_cluster(kmeans.listingDF, data = kmeans.data)

## Random Forest for regression
library(randomForest)
listingDF.randomforest <- randomForest( price~neighbourhood_cleansed +host_location_dummy+bathrooms_text
                                       +number_of_reviews+accommodates+bedrooms+ calculated_host_listings_count_private_rooms
                                      +review_scores_location +review_scores_checkin
                                       +has_availability+calculated_host_listings_count_entire_homes
                                       +reviews_per_month+instant_bookable+host_response_rate
                                       +calculated_host_listings_count_shared_rooms+calculated_host_listings_count
                                       +num_amenities+host_is_superhost+review_scores_communication
                                       +host_identity_verified,data=train_listingDF, na.action=na.omit)
  

summary(listingDF.randomforest)

## random forest prediction for train
listingDF.randomforest.predict.train <-predict(listingDF.randomforest,newdata =train_listingDF)
listingDF.randomforest.predict.train <-na.omit(as.data.frame(listingDF.randomforest.predict.train))
## random forest prediction for test
listingDF.randomforest.predict.test <-predict(listingDF.randomforest,newdata =test_listingDF)
listingDF.randomforest.predict.test <-na.omit(as.data.frame(listingDF.randomforest.predict.test))

## R-squared for train (random forest regression)
actual.forest.train <- na.omit(train_listingDF$price)
R2.train <- 1 - (sum((actual.forest.train-listingDF.randomforest.predict.train)^2)/sum((actual.forest.train-mean(actual.forest.train))^2))

## R-squared for test (random forest regression)
actual.forest.test <- na.omit(test_listingDF$price)
R2.test <- 1 - (sum((actual.forest.test-listingDF.randomforest.predict.test)^2)/sum(( actual.forest.test-mean(actual.forest.test))^2))


## RMSE for random forest regression, function retrieved from the book 'R in the Nutshell'
calculate_rms_error <- function(mdl, train, test, yval) {
  train.yhat <- predict(object=mdl,newdata=train)
  test.yhat <- predict(object=mdl,newdata=test)
  train.y <- with(train,get(yval))
  test.y <- with(test,get(yval))
  train.err <- sqrt(mean((train.yhat - train.y)^2))
  test.err <- sqrt(mean((test.yhat - test.y)^2))
  c(train.err=train.err,test.err=test.err)
}


calculate_rms_error_listingDF_randomforest <-calculate_rms_error(listingDF.randomforest, na.omit(train_listingDF),
                    na.omit(test_listingDF),"price")

calculate_rms_error_listingDF_randomforest


