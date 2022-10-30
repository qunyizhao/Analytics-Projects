###########################################
### Customer Relationship Management
### Final Project of CRM
### Section C, Team 31

### Hoa Anh Hoang (hh274)
### Jiayun Ling (jl1170)
### Junaid Mohammed (jm855)
### Yidi Wu (yw547)
### Ariel Zhao (qz93) 

#########EDA
library(ggplot2)
library(dplyr)
library(psych)
library(corrplot)


## data cleaning
### read file
profile <-read.csv("/Users/abigail/Desktop/MQM_Fall_1/CRM/Group Project/Starbucks-spending-prediction-main/data/profile.csv")
combined_data <- read.csv("/Users/abigail/Desktop/MQM_Fall_1/CRM/Group Project/Starbucks-spending-prediction-main/data/combined_data.csv")
portfolio <-read.csv("/Users/abigail/Desktop/MQM_Fall_1/CRM/Group Project/Starbucks-spending-prediction-main/data/portfolio.csv")
transcript <-read.csv("/Users/abigail/Desktop/MQM_Fall_1/CRM/Group Project/Starbucks-spending-prediction-main/data/transcript.csv")

### combine profile and transcript
combine_profile_transcript <- merge(profile, transcript, by.x ="id", by.y ="customer_id")

### descriptive statistics
describe(combined_data)


### correlation matrix and 
corr_matrix <- subset(combined_data, select = -c(age_.0..10., channel_email,offer_id, customer_id, gender,offer_type))
corr_matrix <- as.matrix(corr_matrix)
cormat <-round(cor(corr_matrix),2)
corrplot(cormat, method = "square", tl.col = 'black')

### see the arrangement of age
hist(combined_data$age, col = "rosybrown3", 
     main = 'Distribution of Starbucks Customer Age',
     xlab = 'Age')

### see distribution of income 
hist(combined_data$income, col = "rosybrown2", 
     main = 'Distribution of Starbucks Customer Income',
     xlab = "Income")

### Mean amount of transaction in Starbucks by Gender
mean_amount <-data.frame(aggregate(combined_data$total_amount, list(combined_data$age), FUN=mean) )
mean_amount <-rename(mean_amount, mean_amount_by_age = x)

barplot(mean_amount$mean_amount_by_age, names.arg=mean_amount$Group.1, ylim = c(0,50),
        ylab="Mean Amount of Transaction in Starbucks", xlab="Age",
        main = "Distribution of Transaction Amount by Age",border="#69b3a2", col="white")


### Total amount group by gender

combined_data$gender <- ifelse(combined_data$gender_F =="1", "Female",
                               ifelse(combined_data$gender_M == "1", "Male", "Other"))
mean_amount <-data.frame(aggregate(combined_data$total_amount, list(combined_data$gender), FUN=mean) )
mean_amount <-rename(mean_amount, mean_amount_by_gender = x)
barplot(mean_amount$mean_amount_by_gender, names.arg=mean_amount$Group.1, ylim = c(0,50),
        ylab="Mean Amount of Transaction ($)", xlab="Gender",
        main = "Distribution of Transaction Amount by Gender",border="#69b3a2", col="lavenderblush2")


### boxplot
ggplot(combined_data, aes(x=offer_type, y=total_amount, fill=offer_type)) + 
    geom_boxplot(alpha=0.3, outlier.shape = NA) +
    theme(legend.position="none") +
    scale_fill_brewer(palette="BuPu")+
    coord_cartesian(ylim = c(0,125))+
    ggtitle("Boxplot of Average Amount by Offer Type")+
    xlab("Offer Type")+ylab("Average Amount($)")


### Pie chart

# Create test data.
data <- data.frame(
    offer_type=c("BOGO", "Discount", "Informational"),
    count=c(26537, 26664, 13300)
)
# Compute percentages
data$fraction <- data$count / sum(data$count)
data$fraction <-round(data$fraction,4)
# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$offer_type, "\n value: ", data$count, "\n",data$fraction*100,"%")

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=offer_type)) +
    geom_rect() +
    geom_text( x=2, aes(y=labelPosition, label=label, color=offer_type), size=6) + # x here controls label position (inner / outer)
    scale_fill_brewer(palette=3) +
    scale_color_brewer(palette=3) +
    coord_polar(theta="y") +
    xlim(c(-1, 4)) +
    theme_void() +
    theme(legend.position = "none")+
    ggtitle("Pie Chart of Offer Types")

######### Modeling Part
data <- read.csv("/Users/unicornnnnn/Desktop/CRM_data/combined_data.csv")

portfolio_cleaned <- read.csv("/Users/unicornnnnn/Desktop/CRM_data/portfolio_cleaned.csv")


### Step 1. Calculate the offer success rate
### Model1: Logisitc Regression
install.packages("stringr")
library(stringr)
data$offer_id <- str_replace(data$offer_id, "ae264e3637204a6fb9bb56bc8210ddfd", "BOGO3")
data$offer_id <- str_replace(data$offer_id, "9b98b8c7a33c4b65b9aebfe6a799e6d9", "BOGO4")
data$offer_id <- str_replace(data$offer_id, "3f207df678b143eea3cee63160fa8bed", "INFO2")
data$offer_id <- str_replace(data$offer_id, "5a8bc65990b245e5a138643cd4eb9837", "INFO1")
data$offer_id <- str_replace(data$offer_id, "f19421c1d4aa40978ebb69ca19b0e20d", "BOGO2")
data$offer_id <- str_replace(data$offer_id, "2906b810c7d4411798c6938adc9daaa5", "DISCOUNT2")
data$offer_id <- str_replace(data$offer_id, "fafdcd668e3743c1bb461111dcafc2a4", "DISCOUNT4")
data$offer_id <- str_replace(data$offer_id, "2298d6c36e964ae4a3e7e9706d1fb8c2", "DISCOUNT1")
data$offer_id <- str_replace(data$offer_id, "0b1e1539f2cc45b7b9fa7c272da2e1d7", "DISCOUNT3")
data$offer_id <- str_replace(data$offer_id, "4d5c57ea9a6940dd891ad53e9dbe8da0", "BOGO1")



### 
portfolio_cleaned$offer_id <- str_replace(portfolio_cleaned$offer_id, "ae264e3637204a6fb9bb56bc8210ddfd", "BOGO3")
portfolio_cleaned$offer_id <- str_replace(portfolio_cleaned$offer_id, "9b98b8c7a33c4b65b9aebfe6a799e6d9", "BOGO4")
portfolio_cleaned$offer_id <- str_replace(portfolio_cleaned$offer_id, "3f207df678b143eea3cee63160fa8bed", "INFO2")
portfolio_cleaned$offer_id <- str_replace(portfolio_cleaned$offer_id, "5a8bc65990b245e5a138643cd4eb9837", "INFO1")
portfolio_cleaned$offer_id <- str_replace(portfolio_cleaned$offer_id, "f19421c1d4aa40978ebb69ca19b0e20d", "BOGO2")
portfolio_cleaned$offer_id <- str_replace(portfolio_cleaned$offer_id, "2906b810c7d4411798c6938adc9daaa5", "DISCOUNT2")
portfolio_cleaned$offer_id <- str_replace(portfolio_cleaned$offer_id, "fafdcd668e3743c1bb461111dcafc2a4", "DISCOUNT4")
portfolio_cleaned$offer_id <- str_replace(portfolio_cleaned$offer_id, "2298d6c36e964ae4a3e7e9706d1fb8c2", "DISCOUNT1")
portfolio_cleaned$offer_id <- str_replace(portfolio_cleaned$offer_id, "0b1e1539f2cc45b7b9fa7c272da2e1d7", "DISCOUNT3")
portfolio_cleaned$offer_id <- str_replace(portfolio_cleaned$offer_id, "4d5c57ea9a6940dd891ad53e9dbe8da0", "BOGO1")
portfolio_cleaned<-portfolio_cleaned[order(portfolio_cleaned$offer_id),]



### Calculate the offer success rate 
library(dplyr)
success_count <- aggregate(data$offer_successful, by=list(data$offer_id), FUN=sum)
offer_count<- data %>% count(data$offer_id)
success_percentage<- (success_count$x)/(offer_count$n)
portfolio_cleaned <- cbind(portfolio_cleaned, success_count$x, offer_count$n, success_percentage)
View(portfolio_cleaned)

### Conclusion: The 2 top successful percentage offers are from discount type with at less 7 days of duration and are promoted by 
### all available channels. The 2 worst successful percentage offers are informational type, without difficulty (0) neither reward (0) 
### and short duration (4 and 3 days).


### Plot the graph
library(ggplot2)
barplot(success_percentage, names=portfolio_cleaned$offer_id, main="Barplot of Offer Success Percentage", xlab="Offer ID", ylab="Success Percentage")

### Reorder the offer success percentage by descending order
portfolio_cleaned$offer_id <- factor(portfolio_cleaned$offer_id, levels = portfolio_cleaned$offer_id[order(portfolio_cleaned$success_percentage, decreasing = TRUE)])


### Make a colorful barplot of offer success percentage
ggplot(portfolio_cleaned, aes(x=offer_id, y=success_percentage, color=offer_id)) + 
    geom_bar(stat = "identity",fill="white") +
    scale_fill_brewer(palette="Blue") + ggtitle("Barplot of Starbucks Offer Success Rate")

### Make a grey barplot of offer success percentage
ggplot(portfolio_cleaned, aes(x=offer_id, y=success_percentage, fill=offer_id)) + 
    geom_bar(stat = "identity") +
    scale_fill_grey() + ggtitle("Barplot of Starbucks Offer Success Rate")



### Model1: Logistic Regression

# Installing the package
install.packages("caTools") # For Logistic regression
install.packages("ROCR")  # For ROC curve to evaluate model

# Loading package
library(caTools)
library(ROCR)

# Splitting dataset: 80% for train and 20% for test
set.seed(1)
split <- sample.split(data, SplitRatio = 0.8)

train_reg <- subset(data, split == "TRUE")
test_reg <- subset(data, split == "FALSE")

# Model1: Logistic Regression
# To make more variations, we only choose the variable age rather than all the age groups
logistic_model <- glm(offer_successful==1 ~ reward + difficulty + duration
                      + channel_mobile + channel_social + channel_web 
                      + bogo + discount + informational + age + income 
                      + year + month + gender_F + gender_M
                      , data = train_reg, family = "binomial")
summary(logistic_model)
## See from the reg result that all the variables here are significant
Rsq <- 1 - logistic_model$deviance/logistic_model$null.deviance
predict(logistic_model,newdata=test_reg)
predict(logistic_model,newdata=test_reg,type="response")
## R-square is 0.214

## Model2: Logistic reg with interaction
model.logistic.interaction <-glm(offer_successful==1~(reward + difficulty + duration
                                                      + channel_mobile + channel_social + channel_web 
                                                      + bogo + discount + informational + age + income 
                                                      + year + month + gender_F + gender_M)^2, data=train_reg, family="binomial")
Rsq <- 1 - model.logistic.interaction$deviance/model.logistic.interaction$null.deviance
summary(model.logistic.interaction)
predict(model.logistic.interaction,newdata=test_reg)
predict(model.logistic.interaction,newdata=test_reg,type="response")
# R-square is 0.233

## Model3: Classification Tree
install.packages("tree")
install.packages("partykit")
library(tree)
library(partykit)

Offertree <- tree(offer_successful~reward+income, data=train_reg) 
summary(Offertree)
###
### We can obtain a description of the whole tree by 
Offertree
###
### We can easily plot the tree
plot(Offertree)

text(Offertree, label="yval")
### to get the probabilities we simple use label="yprob"
plot(Offertree)
text(Offertree, label="yprob")
Offertree[[1]]$yprob[,2]

#####Another version of classfication tree
### Classification Tree
install.packages("party")
install.packages("rpart.plot")
library(party)
library(rpart)
library(rpart.plot)
# tree with reward details as variables, total amount as Y
tree_offer <- ctree(total_amount ~ reward + difficulty + duration,
                    data = train_reg, controls = ctree_control(mincriterion = 0.99, minsplit = 1000))
tree_offer
plot(tree_offer)

# tree with customer details as variables
tree_customer <- ctree(total_amount ~ age + income,
                       data = train_reg, controls = ctree_control(mincriterion = 0.99, minsplit = 1000))
tree_customer
plot(tree_customer)

# tree with successful 1 or 0 as Y

tree_model <- rpart(as.factor(offer_successful) ~ reward + difficulty + duration
                    + channel_mobile + channel_social + channel_web 
                    + bogo + discount + informational + age + income 
                    + year + month + gender_F + gender_M
                    , data = train_reg, method = "class")
rpart.plot(tree_model)


predict(tree_model, data = test_reg)

###
## Tree TRP and FPR
TP <- sum((predict(tree_model,type="class") == 1 )*(train_reg$offer_successful==1))
FP <- sum((predict(tree_model,type="class") == 1 )*(train_reg$offer_successful==0))
FN <- sum((predict(tree_model,type="class") == 0 )*(train_reg$offer_successful==1))
TN <- sum((predict(tree_model,type="class") == 0 )*(train_reg$offer_successful==0))
Tr.FPR <- FP / (FP + TN)
Tr.TPR <- TP / (TP + FN)
Tr.TPR
Tr.FPR

###
### Plotting FPR and TPR
### Logistic Regression
threshold <- .5
TP <- sum((logistic_model$fitted >= threshold)*logistic_model$y)
FP <- sum((logistic_model$fitted >= threshold)*(!logistic_model$y))
FN <- sum((logistic_model$fitted <  threshold)*logistic_model$y)
TN <- sum((logistic_model$fitted <  threshold)*(!logistic_model$y))

LR.FPR <- FP / (FP + TN)
LR.TPR <- TP / (TP + FN)
LR.TPR
LR.FPR

### Logistic Regression with interactions
TP <- sum((model.logistic.interaction$fitted >= threshold)*model.logistic.interaction$y)
FP <- sum((model.logistic.interaction$fitted >= threshold)*(!model.logistic.interaction$y))
FN <- sum((model.logistic.interaction$fitted <  threshold)*model.logistic.interaction$y)
TN <- sum((model.logistic.interaction$fitted <  threshold)*(!model.logistic.interaction$y))
LRI.FPR <- FP / (FP + TN)
LRI.TPR <- TP / (TP + FN)
LRI.TPR
LRI.FPR

plot( c( 0, 1 ), c(0, 1), type="n", xlim=c(0,1), ylim=c(0,1), bty="n", xlab = "False positive rate", ylab="True positive rate",main ="Model Performance Evaluation: OOS R2")
lines(c(0,1),c(0,1), lty=2)
points( c( LR.FPR, Tr.FPR, LRI.FPR), c(LR.TPR, Tr.TPR, LRI.TPR))
text( c( LR.FPR+0.04, Tr.FPR+0.08, LRI.FPR-0.08), c(LR.TPR-.05, Tr.TPR-0.02, LRI.TPR-0.05), labels=c("LR", "Tree", "LR with Inter"))




###########################################
### Model Performance Measurement #########
###########################################
library(tree)

model.logistic.interaction <-glm(offer_successful==1~(reward + difficulty + duration
                                                       + channel_mobile + channel_social + channel_web 
                                                       + bogo + discount + informational + age + income 
                                                       + year + month + gender_F + gender_M)^2, data=train_reg, family="binomial")
model.logistic <-glm(offer_successful==1~(reward + difficulty + duration
                                         + channel_mobile + channel_social + channel_web 
                                         + bogo + discount + informational + age + income 
                                         + year + month + gender_F + gender_M), data=train_reg,family="binomial")
model.tree <- tree(factor(offer_successful)~ (reward + difficulty + duration
                                              + channel_mobile + channel_social + channel_web 
                                              + bogo + discount + informational + age + income 
                                              + year + month + gender_F + gender_M), data=train_reg, method = "class") 
model.null <- glm(offer_successful==1~1, data=train_reg, family="binomial")

### Get R2 for each model
R2 <- function(y, pred, family=c("gaussian","binomial")){
    fam <- match.arg(family)
    if(fam=="binomial"){
        if(is.factor(y)){ y <- as.numeric(y)>1 }
    }
    dev <- deviance(y, pred, family=fam)
    dev0 <- deviance(y, mean(y), family=fam)
    return(1-dev/dev0)
}


### We can see that Logistic regression with interactions has better R2 than others.
###
### Lets plot FPR and TPR
plot( c( 0, 1 ), c(0, 1), type="n", xlim=c(0,1), ylim=c(0,1), bty="n", xlab = "False positive rate", ylab="True positive rate",main="Model Performance Evaluation: ROC Curve")
lines(c(0,1),c(0,1), lty=2)
val<- .5
values <- FPR_TPR( (model.logistic.interaction$fitted >= val) , model.logistic.interaction$y )
points( values$FPR , values$TPR )
ACC.model.logistic.interaction <- round(values$ACC,3)
text( values$FPR-0.12, values$TPR+.01, labels=c("LR with inter"))
values <- FPR_TPR( (model.logistic$fitted >= val) , model.logistic$y )
points( values$FPR , values$TPR)    
ACC.model.logistic <- round(values$ACC,4)
text( values$FPR+.02, values$TPR+0.05, labels=c("LR"))
values <- FPR_TPR( (predict(model.tree,type="class") == 1) , model.logistic.interaction$y )
points( values$FPR , values$TPR )    
ACC.model.tree <- round(values$ACC,3)
text( values$FPR, values$TPR+.04, labels=c("tree"))



for( val in seq(from=0,to=1,by=0.05)){
    values <- FPR_TPR( (model.logistic.interaction$fitted >= val) , model.logistic.interaction$y )
    points( values$FPR , values$TPR, pch = 21, bg="red" )
    values <- FPR_TPR( (model.logistic$fitted >= val) , model.logistic$y )
    points( values$FPR , values$TPR, pch = 22, bg="blue" )    
    values <- FPR_TPR( (predict(model.tree,type="vector")[,2] >= val) , model.logistic.interaction$y )
    points( values$FPR , values$TPR, pch = 23, bg="green" )    
}

library(RColorBrewer)
coul <- brewer.pal(3, "Set2") 
xx<-barplot(c(ACC.model.logistic.interaction, ACC.model.logistic, ACC.model.tree),ylim=c(0,1), xlab="Method", names = c("\n logistic regression \n with interaction terms", "\n logistic\n regression ", "\n classification \n tree"), ylab = "Accuracy",col=coul, main="Model Performance of OOS Accuracy")
text(x = xx, y = c(ACC.model.logistic.interaction, ACC.model.logistic, ACC.model.tree), label = c(ACC.model.logistic.interaction, ACC.model.logistic, ACC.model.tree), pos = 3, cex = 0.8, col = "black")


### Then we calculate OOS R2
OOS <- data.frame(logistic.interaction=NA, logistic=NA, tree=NA, randomforest=NA, null=NA)
## get predictions: type=response so we have probabilities
pred.logistic.interaction <- predict(model.logistic.interaction, newdata=test_reg, type="response")
pred.logistic             <- predict(model.logistic, newdata=test_reg, type="response")
pred.tree                 <- predict(model.tree, newdata=test_reg, type="vector")[,2]
pred.rf                   <- predict(rf_model, newdata = test_reg, type="response")
pred.null                 <- predict(model.null, newdata=test_reg, type="response")

## calculate OOS R2
OOS$logistic.interaction <- R2(test_reg$offer_successful==1, pred=pred.logistic.interaction, family="binomial")
# Logistic
OOS$logistic <- R2(y=test_reg$offer_successful==1, pred=pred.logistic, family="binomial")
# Tree
OOS$tree <- R2(y=test_reg$offer_successful==1, pred=pred.tree, family="binomial")
# Random Forest
OOS$randomforest <- R2(y=test_reg$offer_successful==1, pred=pred.rf, family="binomial")
#Null model (just intercept)
OOS$null <- R2(y=test_reg$offer_successful==1, pred=pred.null, family="binomial")
#Null Model guess
sum(train_reg$offer_successful==1)/length(train_reg)


### Lets list the results stored in the dataframe OOS
OOS

par(mar=c(7,5,.5,1)+0.3)
barplot(colMeans(OOS), las=2,xpd=FALSE, xlab="", ylim=c(0,0.35), ylab = "")


### Calculate RMSE for all four models
## RMSE for LR with interaction
install.packages("caret")
library(caret)
test_reg$offer_successful <- as.numeric(test_reg$offer_successful)
pred.logistic.interaction <- as.data.frame(pred.logistic.interaction)
pred.logistic.interaction$pred.logistic.interaction <- as.numeric(pred.logistic.interaction$pred.logistic.interaction)
rmse.lri <- RMSE(test_reg$offer_successful, pred.logistic.interaction$pred.logistic.interaction)
rmse.lri
### rmse for LR with interaction is 0.4253867



## RMSE for LR
pred.logistic <- as.data.frame(pred.logistic)
pred.logistic$pred.logistic <- as.numeric(pred.logistic$pred.logistic)
rmse.lr <- RMSE(test_reg$offer_successful, pred.logistic$pred.logistic)
rmse.lr
### rmse for LR  is 0.4298436


## RMSE for tree
pred.tree <- as.data.frame(pred.tree)
pred.tree$pred.tree <- as.numeric(pred.tree$pred.tree)
rmse.tree <- RMSE(train_reg$offer_successful, pred.tree$pred.tree)
rmse.tree
### rmse for tree is 0.5558561



###################################
#######  Model5: Random Forest ####
###################################


library(randomForest)

train_reg$offer_successful <- as.factor(train_reg$offer_successful)
train_reg$reward <- as.factor(train_reg$reward)
train_reg$difficulty <- as.factor(train_reg$difficulty)
train_reg$duration <- as.factor(train_reg$duration)
train_reg$channel_mobile <- as.factor(train_reg$channel_mobile)
train_reg$channel_social <- as.factor(train_reg$channel_social)
train_reg$channel_web <- as.factor(train_reg$channel_web)
train_reg$bogo <- as.factor(train_reg$bogo)
train_reg$discount <- as.factor(train_reg$discount)
train_reg$informational <- as.factor(train_reg$informational)
train_reg$age <- as.factor(train_reg$age)
train_reg$income <- as.factor(train_reg$income)
train_reg$year <- as.factor(train_reg$year)
train_reg$month <- as.factor(train_reg$month)
train_reg$gender_F <- as.factor(train_reg$gender_F)
train_reg$gender_M <- as.factor(train_reg$gender_M)

test_reg$offer_successful <- as.factor(test_reg$offer_successful)
test_reg$reward <- as.factor(test_reg$reward)
test_reg$difficulty <- as.factor(test_reg$difficulty)
test_reg$duration <- as.factor(test_reg$duration)
test_reg$channel_mobile <- as.factor(test_reg$channel_mobile)
test_reg$channel_social <- as.factor(test_reg$channel_social)
test_reg$channel_web <- as.factor(test_reg$channel_web)
test_reg$bogo <- as.factor(test_reg$bogo)
test_reg$discount <- as.factor(test_reg$discount)
test_reg$informational <- as.factor(test_reg$informational)
test_reg$age <- as.factor(test_reg$age)
test_reg$income <- as.factor(test_reg$income)
test_reg$year <- as.factor(test_reg$year)
test_reg$month <- as.factor(test_reg$month)
test_reg$gender_F <- as.factor(test_reg$gender_F)
test_reg$gender_M <- as.factor(test_reg$gender_M)

# no age and income because they exceed 53 categories
rf_model <- randomForest(formula = offer_successful ~ reward + difficulty + duration
                         + channel_mobile + channel_social + channel_web 
                         + bogo + discount + informational
                         + year + month + gender_F + gender_M
                         , data = train_reg
                         , nodesize=5, ntree = 500, mtry = 4)
rf_model
predict_rf <- predict(rf_model, newdata = test_reg[,c("reward", "difficulty", "duration",
                                                      "channel_mobile", "channel_social", "channel_web", 
                                                      "bogo", "discount", "informational", "year", "month", "gender_F", "gender_M")])
predict_rf <- as.data.frame(predict_rf)
class(predict_rf$predict_rf)
confusionMatrix(predict_rf$predict_rf, test_reg$offer_successful)

str(test_reg)
levels(predict_rf$predict_rf) <- list("0" = "1", "1" = "2")
str(predict_rf)

library(ROCR)
perf = prediction(predict_rf[,2], test_reg$offer_successful)

test_reg$offer_successful <- as.factor(test_reg$offer_successful)
predict_rf <- as.data.frame(predict_rf)
predict_rf$predict_rf <- as.factor(predict_rf$predict_rf)
recall(test_reg$offer_successful, predict_rf$predict_rf, cutoff = 0)
sensitivity(test_reg$offer_successful, predict_rf$predict_rf, cutoff = 0)
tpr(test_reg$offer_successful, predict_rf$predict_rf, cutoff = 0)


# 1. Area under curve
auc = performance(perf, "auc")
auc
# 2. True Positive and Negative Rate
pred3 = performance(perf, "tpr","fpr")
# 3. Plot the ROC curve
plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

# randomForest(formula = offer_successful ~ reward + difficulty + duration + channel_mobile + channel_social + channel_web +      bogo + discount + informational + year + month + gender_F +      gender_M, data = train_reg, nodesize = 5, ntree = 500, mtry = 4) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 4
# 
# OOB estimate of  error rate: 27.57%
# Confusion matrix:
#   0     1 class.error
# 0 20859  7257   0.2581093
# 1  7399 17649   0.2953928
importance(rf_model)
# MeanDecreaseGini
# reward               1301.33459
# difficulty           1204.97710
# duration             1090.83262
# channel_mobile         34.80627
# channel_social        448.19071
# channel_web            45.27077
# bogo                  128.94061
# discount              217.77633
# informational         878.04135
# year                 1402.38745
# month                 369.48454
# gender_F              134.90482
# gender_M              195.77426
varImpPlot(rf_model, main="RF Importance features")
summary(rf_model)
plot(rf_model)

## random forest prediction for train
randomforest.predict.train <-as.numeric(predict(rf_model,newdata = train_reg))
randomforest.predict.train <-na.omit(as.data.frame(randomforest.predict.train))
## random forest prediction for test
randomforest.predict.test <-as.numeric(predict(rf_model,newdata = test_reg))
randomforest.predict.test <-na.omit(as.data.frame(randomforest.predict.test))

## R-squared for train
actual.forest.train <- na.omit(train_reg$offer_successful)
actual.forest.train <- as.numeric(actual.forest.train)
randomforest.predict.train <- as.numeric(randomforest.predict.train$randomforest.predict.train)
R2.train <- 1 - (sum((actual.forest.train-randomforest.predict.train)^2)/sum((actual.forest.train-mean(actual.forest.train))^2))
R2.train
# R2 for train is -0.08517283

## R-squared for test
actual.forest.test <- na.omit(test_reg$offer_successful)
actual.forest.test <- as.numeric(actual.forest.test)
randomforest.predict.test <- as.numeric(randomforest.predict.test$randomforest.predict.test)
R2.test <- 1 - (sum((actual.forest.test-randomforest.predict.test)^2)/sum((actual.forest.test-mean(actual.forest.test))^2))
R2.test
# R2 for test is -0.1022602

## RMSE for train
train_reg$offer_successful <- as.numeric(train_reg$offer_successful)
rmse.rf <- rmse(train_reg$offer_successful, randomforest.predict.train)
rmse.rf
### rmse for train random forest is 0.5199901

## RMSE for test
test_reg$offer_successful <- as.numeric(test_reg$offer_successful)
rmse.rf <- rmse(test_reg$offer_successful, randomforest.predict.test)
rmse.rf
### rmse for test random forest is 0.5242132

## RF TPR and FPR
TP <- sum((predict(rf_model,type="class") == 1 )*(train_reg$offer_successful==1))
FP <- sum((predict(rf_model,type="class") == 1 )*(train_reg$offer_successful==0))
FN <- sum((predict(rf_model,type="class") == 0 )*(train_reg$offer_successful==1))
TN <- sum((predict(rf_model,type="class") == 0 )*(train_reg$offer_successful==0))
Tr.FPR <- FP / (FP + TN)
Tr.TPR <- TP / (TP + FN)
Tr.FPR
Tr.TPR

plot( c( 0, 1 ), c(0, 1), type="n", xlim=c(0,1), ylim=c(0,1), bty="n", xlab = "False positive rate", ylab="True positive rate")
lines(c(0,1),c(0,1), lty=2)
points( c( Tr.FPR), c(Tr.TPR))
text(c(Tr.FPR), c(Tr.TPR-.05), labels=c("LR","Random Forest"))
install.packages("plm")
library(plm)



### PCA modelling
library(glmnet)
library(stringr)
library(plfm)
library(devtools)
library(ggplot2)
library(ggbiplot)
library(ggrepel)
library(factoextra)
library(dplyr)
library(ggfortify)
install.packages("devtools")
library(devtools)

# using numeric data for pca
### Check the correlation between each independent variables
nums <- unlist(lapply(data, is.numeric), use.names = FALSE) 
nums <- data[nums]
str(nums)

data_p <- subset(nums, select = c(time_in_days, total_amount, reward, difficulty, duration, age, income))
pca <- prcomp(data_p, center = TRUE, scale. = TRUE)
summary(pca)
str(pca)

### plot pca scree plot
fviz_eig(pca)
get_eig(pca)
plot.pca.elbow <- plot(pca, type="l")
ggbiplot(pca)

# eigenvalue variance.percent cumulative.variance.percent
# Dim.1  2.1007054        30.010077                    30.01008
# Dim.2  1.4416107        20.594439                    50.60452
# Dim.3  1.0018630        14.312329                    64.91685
# Dim.4  0.9138517        13.055024                    77.97187
# Dim.5  0.8084995        11.549992                    89.52186
# Dim.6  0.6064342         8.663346                    98.18521
# Dim.7  0.1270355         1.814793                   100.00000

plot(pca,main="PCA: Variance Explained by Factors")
mtext(side=1, "Factors",  line=1, font=2)
### It seems like the first 3 components are responsible for almost all variation
### They can explain 64.9% of variance.

autoplot(pca, data = data_p, color = 'viridis')
# fviz_pca_ind(pca,
#              col.ind = "cos2", # Color by the quality of representation
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE   
#              # Avoid text overlapping)

### loadings analysis
loadings(pca)
loadings <- pca$rotation[,1:5]
#### Loading 1
v<-loadings[order(abs(loadings[,1]), decreasing=TRUE)[1:ncol(data_p)],1]
loadingfit <- lapply(1:ncol(xdata), function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]

#### Loading 2
v<-loadings[order(abs(loadings[,2]), decreasing=TRUE)[1:ncol(data_p)],2]
loadingfit <- lapply(1:ncol(xdata), function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]

#### Loading 3
v<-loadings[order(abs(loadings[,3]), decreasing=TRUE)[1:ncol(data_p)],3]
loadingfit <- lapply(1:15, function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]














