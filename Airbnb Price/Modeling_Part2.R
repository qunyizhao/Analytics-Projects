###########################################
### Data Science for Business
### Final Group Proj -- Airbnb Price Evaluation
### Section C, Team 31

### Hoa Anh Hoang (hh274)
### Jiayun Ling (jl1170)
### Junaid Mohammed (jm855)
### Yidi Wu (yw547)
### Ariel Zhao (qz93) 

###########################
### Modeling Part 2 #######
###########################
setwd("~/Desktop/Fuqua/FuquaFall1/DS for Business/Final/DS Final Code and Data/Cleaned Datasets")

##First, we combined listing and calendar file into one big data file for final modeling
##by usiing listing's id = calendar's listing_id
listingDF <- read.csv("cleaned_listings.csv")
Cal <- read.csv("cleaned_calendar.csv")

##The specific steps show below:
## Step1: Merge the two above dataset together
combine <- merge(Cal,listingDF, by.x="listing_id",by.y="id")
## Step2: Delete duplicates
combine1 = combine[!duplicated(combine$date, combine$listing_id),]
## Step3: Write the new csv file for final modeling 
write.csv(combine1, "/Users/unicornnnnn/Desktop/cleaned_combined.csv", row.names=FALSE)

#### import cleaned_combine
new_combined <- read.csv("cleaned_combined.csv")




################################################
### Modeling Part 2 use combined dataset #######
################################################

library(caTools)
library(ROCR)
library(glmnet)
library(stringr)
library(plfm)
library(devtools)
install_github("vqv/ggbiplot")
library(ggplot2)
library(ggbiplot)
install.packages("ggrepel")
library(ggrepel)
install.packages("factoextra")
library(factoextra)
library(dplyr)
install.packages("olsrr")
library(olsrr)
library(randomForest)
library(libcoin)
library(partykit)


# Splitting dataset with 80% train and 20% test
## Kind tips: This sample spilt may take around 5 to 10 minutes.
set.seed(17)
split <- sample.split(new_combined, SplitRatio = 0.8)
split

train <- subset(new_combined, split == "TRUE")
test <- subset(new_combined, split == "FALSE")


################## Model4.linear regression
### We selected variables from which the stepwise selection give to us 
### and used in the combined dataset for further modeling so that for each
### listing we have a price for specific date
model.linear <- glm(price.x ~bathrooms+number_of_reviews+room_type+accommodates
                  +bedrooms+calculated_host_listings_count_private_rooms
                  +review_scores_location+review_scores_checkin
                  +review_scores_location+review_scores_rating
                  +calculated_host_listings_count_entire_homes
                  +reviews_per_month
                  +calculated_host_listings_count_shared_rooms
                  +calculated_host_listings_count
                  +num_amenities, data = train)
summary(model.linear)
Rsq <- 1 - model.linear$deviance/model.linear$null.deviance
## R square here is 0.5579

### create a vector of fold memberships for 10 fold cross validation
n <- nrow(train)
nfold <- 10
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]

## Compare between different models
### model.linear : linear reg
### Model.lasso : Lasso to select min choice of lambda
### model.pl: Post Lasso associated with Lasso and min choice of lambda

### create an empty dataframe of results
OOSPerformance <- data.frame(linear=rep(NA,nfold), lasso=rep(NA,nfold), pl=rep(NA,nfold)) 
R2 <- function(y, pred, family=c("gaussian","binomial")){
    fam <- match.arg(family)
    if(fam=="binomial"){
        if(is.factor(y)){ y <- as.numeric(y)>1 }
    }
    dev <- deviance(y, pred, family=fam)
    dev0 <- deviance(y, mean(y), family=fam)
    return(1-dev/dev0)
}
PerformanceMeasure <- function(prediction, actual, threshold) {
    mean( abs( prediction - actual )/ ifelse(abs(actual)>threshold, abs(actual),threshold) )  
    R2(y=actual, pred=prediction)
}


#### Model5+6 Lasso/Post Lasso

Mx<- model.matrix(price.x ~ Month+weekend+ holiday+room_type +accommodates +bathrooms +bedrooms 
                  + beds +num_amenities +number_of_reviews+review_scores_rating
                  +calculated_host_listings_count + calculated_host_listings_count_entire_homes
                  +calculated_host_listings_count_private_rooms 
                  +calculated_host_listings_count_shared_rooms 
                  +reviews_per_month+ host_identity_verified
                  + review_scores_cleanliness +instant_bookable+review_scores_checkin
                  +review_scores_communication +review_scores_location+review_scores_value
                  +check_in_24h+high_end_electronics
                  +bbq+breakfast+coffee_machine+cooking_basics+child_friendly
                  +parking+host_greeting+internet+pets_allowed+secure
                  +self_check_in+smoking_allowed+event_suitable, data=train)[,-1]

My<- train$price.x
lasso <- glmnet(Mx,My)
lassoCV <- cv.glmnet(Mx,My)
par(mar=c(1.5,1.5,2,1.5))
par(mai=c(1.5,1.5,2,1.5))
plot(lassoCV, main="Fitting Graph for CV Lasso \n \n # of non-zero coefficients  ", xlab = expression(paste("log(",lambda,")")))
#### Post Lasso #####
support<- function(x, tr = 10e-6) {
    m<- rep(0, length(x))
    for (i in 1:length(x)) if( abs(x[i])> tr ) m[i]<- i
    m <- m[m>0]
    m
}
features.min <- support(lasso$beta[,which.min(lassoCV$cvm)])
length(features.min)
data.min <- data.frame(Mx[,features.min],My)


#####################Calculate OOS Performance for three different models
for(k in 1:nfold){ 
    new_train <- which(foldid!=k) # train on all but fold `k'
    
    ### This is the CV for the Post Lasso Estimates
    pl <- glm(My~., data=data.min, subset=new_train)
    predmin <- predict(pl, newdata=data.min[-new_train,], type="response")
    OOSPerformance$pl[k] <- PerformanceMeasure(predmin, My[-new_train], .02) 
    
    ### This is the CV for the Lasso estimates  
    lassomin  <- glmnet(Mx[new_train,],My[new_train],lambda = lassoCV$lambda.min)
    predlassomin <- predict(lassomin, newx=Mx[-new_train,], type="response")
    OOSPerformance$lasso[k] <- PerformanceMeasure(predlassomin, My[-new_train], .02) 
    
    ### model.linear
    model.linear <- glm(price.x~ bathrooms+number_of_reviews+room_type+accommodates+bedrooms+calculated_host_listings_count_private_rooms+review_scores_location+review_scores_checkin
                   +review_scores_location+review_scores_rating
                   +calculated_host_listings_count_entire_homes
                   +reviews_per_month+calculated_host_listings_count_shared_rooms+calculated_host_listings_count
                   +num_amenities, data=train, subset=new_train)
    pred.linear <- predict(model.linear, newdata=train, subset=-new_train, type="response")
    OOSPerformance$linear[k] <- PerformanceMeasure(pred.linear, My[-new_train], .02) 
    
    ###
    print(paste("Iteration",k,"of",nfold,"completed"))
}

barplot(colMeans(OOSPerformance), las=2,xpd=FALSE , xlab="", ylab = bquote( "Average Out of Sample Performance (Relative)"))
barplot(colMeans(OOSPerformance), las=2,xpd=FALSE , xlab="", ylim=c(0,1), ylab = bquote( "Average Out of Sample "~R^2), main="Model Performance Evaluation (OOS R2)")

### Our conclusion: Post Lasso seems a little bit better with average
### Make predictions of Lasso and Post Lasso
new_train1 <- which(foldid!=1)
### the Post Lasso Estimates
pl1 <- glm(My~., data=data.min, subset=new_train1)
predmin1 <- predict(pl, newdata=data.min[-new_train1,], type="response")
par(mar=c(1.5,1.5,1.5,1.5))
par(mai=c(1.5,1.5,1.5,1.5))
hist(predmin1, breaks = 40, main="Predictions for Post Lasso", xlab="Predictions of Price")

## Calculate the Post Lasso R2
predmin1 <- as.data.frame(predmin1)
R2.train <- 1 - (sum((train$price.x-predmin1)^2)/sum((train$price.x-mean(train$price.x))^2))
### Lasso R2 is 0.857


## Calculate the Lasso R2
lassomin1  <- glmnet(Mx[new_train1,],My[new_train1],lambda = lassoCV$lambda.min)
predlassomin1 <- predict(lassomin1, newx=Mx[-new_train1,], type="response")
predlassomin1 <- as.data.frame(predlassomin1)
R2.train1 <- 1 - (sum((train$price.x-predlassomin1)^2)/sum((train$price.x-mean(train$price.x))^2))
### Post Lasso R2 is 0.862



# Try Logistic Regression by using avaliable_dummy as the target variable but it gives us bad R2 result:
# Model7: Logistic Regression
View(Combined)
logistic_model <- glm(available_dummy==1 ~ price.x +holiday
                      + host_response_time + host_response_rate +host_location_dummy
                      + host_acceptance_rate + host_total_listings_count
                      + room_type +accommodates +bathrooms +bedrooms 
                      + beds +num_amenities +number_of_reviews+review_scores_rating
                      +reviews_per_month+ host_identity_verified
                      +review_scores_accuracy + review_scores_cleanliness +instant_bookable
                      +review_scores_checkin
                      +review_scores_communication +review_scores_location
                      +review_scores_value 
                      +breakfast + tv+ white_goods+gym+cooking_basics
                      +minimum_nights.y+Month+day_of_week
                      , data=train,family = "binomial")
summary(logistic_model)
Rsq <- 1 - logistic_model$deviance/logistic_model$null.deviance
predict(logistic_model,newdata=test)
predict(logistic_model,newdata=test,type="response")
## R-square is 0.0282

## Calculate TPR and FPR
threshold <- .5
TP <- sum((logistic_model$fitted >= threshold)*logistic_model$y)
FP <- sum((logistic_model$fitted >= threshold)*(!logistic_model$y))
FN <- sum((logistic_model$fitted <  threshold)*logistic_model$y)
TN <- sum((logistic_model$fitted <  threshold)*(!logistic_model$y))

LR.FPR <- FP / (FP + TN)
LR.TPR <- TP / (TP + FN)


###### Model5: Classification Tree
library(tree)
library(partykit)
ava_tree <- tree(as.factor(available)~price.x+weekend+holiday+ minimum_nights.x 
                 + maximum_nights.x + Year + Month + Date
                 +bathrooms+number_of_reviews+room_type+accommodates
                 +bedrooms+calculated_host_listings_count_private_rooms
                 +host_total_listings_count+review_scores_location+review_scores_checkin
                 +review_scores_location+review_scores_rating
                 +calculated_host_listings_count_entire_homes
                 +reviews_per_month
                 +calculated_host_listings_count_shared_rooms
                 +calculated_host_listings_count
                 +num_amenities, data=train) 
summary(ava_tree)
ava_tree
plot(ava_tree)
text(ava_tree, label="yval")


## Tree TRP and FPR + LR TPR and FPR

TP <- sum((predict(ava_tree,type="class") == "t" )*(test$available=="t"))
FP <- sum((predict(ava_tree,type="class") == "t" )*(test$available=="f"))
FN <- sum((predict(ava_tree,type="class") == "f" )*(test$available=="t"))
TN <- sum((predict(ava_tree,type="class") == "f" )*(test$available=="f"))
Tr.FPR <- FP / (FP + TN)
Tr.TPR <- TP / (TP + FN)

plot( c( 0, 1 ), c(0, 1), type="n", xlim=c(0,1), ylim=c(0,1), bty="n", xlab = "False positive rate", ylab="True positive rate")
lines(c(0,1),c(0,1), lty=2)
points( c( LR.FPR,Tr.FPR), c(LR.TPR,Tr.TPR))
text( c( LR.FPR+0.04,Tr.FPR), c(LR.TPR-.01,Tr.TPR-.05), labels=c("LR","Tree"))





