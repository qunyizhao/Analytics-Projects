
# import survey data
data <- read.csv("Athena_survey_data.csv")
head(data)
View(data)

####################################################################################
### Question 3 Factor, Cluster, Demographic (Crosstab) Analysis
####################################################################################

# 3a) Factor Analysis
# Step 1: Evaluate the data
# run Bartlett's Test of Sphericity (want p < 0.05 to go ahead)
#install.packages("REdaS")
library("REdaS")
bart_spher(data[,7:46])

# run KMO-test (want overall MSA > 0.6 to go ahead)
#install.packages("psych")
library("psych")
KMO(data[,7:46])

# Step 2: Determine the number of factors
# install package for Factor Analysis
#install.packages("FactoMineR")
library("FactoMineR")
pca <- PCA(data[,7:46], scale = TRUE)
summary(pca)

# eigenvalues
pca$eig

# visualize eigenvalues / screeplot
#install.packages("factoextra")
library("factoextra")
fviz_eig(pca, addlabels = TRUE)

# Step 3: Extract the factor solution (varimax rotation)
fit <- factanal(data[,7:46], factors=6, rotation="varimax")

# loadings
fit$loadings
as.table(fit$loadings)

# loadingplot
plot(fit$loadings, main = "Factor loadings", xlab = "Factor 1", 
     ylab = "Factor 2", col="blue", bg = "blue", pch=21)
text(fit$loadings, labels = rownames(fit$loadings))
abline(h=0, v=0, col="purple")

scores <- factor.scores(data[,7:46], fit$loadings)$scores
data$strategic_challenge <- scores[,1]
data$customized_explorer <- scores[,2]
data$completion_achievement <- scores[,3]
data$action_lover <- scores[,4]
data$competitive_collaborator <- scores[,5]
data$character_story <- scores[,6]

View(data)


# 3b) **CLUSTER ANALYSIS


# Step 1: determine the number of clusters
# create a dissimilarity matrix
d <- dist(data[,49:54], method = "euclidean")

# perform hierarchical clustering using Ward's method on this matrix
hc <- hclust(d, method="ward.D") 

# plot the resulting endrogram
plot(hc, cex = 0.6, hang = -1)

#Step 2: Calculate the final cluster solution
# note that K-Means uses a random seed; if you want to see the same 
# result you got before, use the same seed
set.seed(42)

# run K-Means to find 3 clusters
kmeans.solution <- kmeans(data[,49:54], centers = 5)

# create new variable showing who is in which cluster
data$cluster <- kmeans.solution$cluster
View(data)

# Step 3: Interpret the K-means output
# look at the cluster centers to distinguish each cluster
# (we want to look for high & low values)
kmeans.solution$centers

# visualize K-means clusters
fviz_cluster(kmeans.solution, data=data[,49:54])



## 3c)
library(plyr)
data$segment.label <- revalue(as.character(data$cluster),
                              c("1"="Achievement-Mastery", "2"="Customized-Social", "3"="Strategic-Explorer", "4"="Mastery-Social", "5"="Action-Immersion"))
View(data)

data$female<-ifelse(data$gender=="female",1,0)
data


install.packages("purrr")              # Install & load purrr
library("purrr")
data %>%                               # Summary by group using purrr
    split(.$segment.label) %>%
    map(summary)

library(dplyr)

## Calculate the average age for each cluster
data %>%
    group_by(segment.label) %>%
    summarise(avg=mean(age))
## Calculate the average income for each cluster
data %>%
    group_by(segment.label) %>%
    summarise(avg=mean(income))
## Calculate the average income for each cluster
data %>%
    group_by(segment.label) %>%
    summarise(avg=mean(female))

reg1 <- lm(age ~ segment.label, data=data)
summary(reg1)

reg2 <- lm(income ~ segment.label, data=data)
summary(reg2)


## Do CrossTab analysis for gender and location

## do crosstab 1
# crosstab analysis (gender & cluster)
crosstab <- xtabs(~segment.label+gender, data)
crosstab
summary(crosstab)
# Check the CrossTable
library(gmodels)
CrossTable(data$segment.label, data$gender, expected = TRUE)



## do crosstab 2
# crosstab analysis (location & cluster)
crosstab <- xtabs(~segment.label+state, data)
crosstab
summary(crosstab)
# Check the CrossTable
library(gmodels)
CrossTable(data$segment.label, data$state, expected = TRUE)

north <- c("ME", "NH", "VT", "MA", "RI", "CT", "NY", "NJ", "PA", "OH", "IN", "IL", "MI", "WI", "MN", "IA", "NE", "ND", "SD")
west <- c("WA", "OR", "CA", "NV", "ID", "MT", "WY", "UT", "CO", "AZ", "NM", "AK", "HI")
east <- c("DE", "MD", "DC", "VA", "WV", "KY", "TN", "NC", "SC", "GA", "FL", "AL", "MS", "AR", "LA")
south <- c("TX", "OK", "KS", "MO")

data$region <- ifelse(data$state %in% north, "North", 
                    ifelse(data$state %in% west, "West",
                           ifelse(data$state %in% east, "East", 
                                  ifelse(data$state %in% south, "South", NA))))
crosstab <- xtabs(~segment.label+region, data)
summary(crosstab)
CrossTable(data$segment.label, data$region, expected=TRUE)


## do crosstab 3

data$age_group = 2
data$age_group[data$age <= 20] <- 1
data$age_group[data$age >= 35] <- 3
data$age_group <- revalue(as.character(data$age_group),
                       c("1"="Age under 20", "2"="Age between 20 and 35", "3"="Age above 35"))
crosstab <- xtabs(~segment.label+age_group, data)
crosstab
summary(crosstab)
CrossTable(data$segment.label, data$age_group, expected=TRUE) 


## do crosstab 4

data$income_group = 2
data$income_group[data$income <= 35000] <- 1
data$income_group[data$income >= 80000] <- 3
data$income_group <- revalue(as.character(data$income_group),
                          c("1"="Low Income", "2"="Middle Income", "3"="High Income"))
crosstab <- xtabs(~segment.label+income_group, data)
crosstab
summary(crosstab)
CrossTable(data$segment.label, data$income_group, expected=TRUE) 




############################
### Question 4 Pricing
############################

data_w <- data[data$game.presented == "Warrior Guild",]
data_s <- data[data$game.presented == "Seraph Guardians",]
data_e <- data[data$game.presented == "Evercrest",]

## Game 1: Warrior Guild
# calculate cumuative customers WTP at given price and expected revenue (# cust WTP * price)
cumulative.wtp_1 <- data.frame()
for (price in seq(min(data_w$gg.max.price), max(data_w$gg.max.price), 5)) {
    num.respondents.wtp <- sum(data_w$gg.max.price >= price)
    cumulative.wtp_1 <- rbind(cumulative.wtp_1, 
                            data.frame(price=price, 
                                       per.customers.wtp=num.respondents.wtp/nrow(data_w),
                                       pred.revenue=num.respondents.wtp*price))
}

# plot customers willing to pay as a function of price
library(ggplot2)
ggplot(cumulative.wtp_1, aes(x=price, y=per.customers.wtp)) +
    geom_line() + geom_point() +
    theme_bw() + ylab("Percent customers willing to pay") +ggtitle("Warrior Guild WTP(%)")

# plot revenue as a function of price
ggplot(cumulative.wtp_1, aes(x=price, y=pred.revenue)) +
    geom_line() + geom_point() + geom_vline(xintercept=50,linetype=2) +
    theme_bw() + ylab("Predicted revenue") +ggtitle("Warrior Guild Pred. Revenue")

cumulative.wtp_1


## Game 2: Seraph Guardians
# calculate cumuative customers WTP at given price and expected revenue (# cust WTP * price)
cumulative.wtp_2 <- data.frame()
for (price in seq(min(data_s$gg.max.price), max(data_s$gg.max.price), 5)) {
    num.respondents.wtp <- sum(data_s$gg.max.price >= price)
    cumulative.wtp_2 <- rbind(cumulative.wtp_2, 
                            data.frame(price=price, 
                                       per.customers.wtp=num.respondents.wtp/nrow(data_s),
                                       pred.revenue=num.respondents.wtp*price))
}

# plot customers willing to pay as a function of price
library(ggplot2)
ggplot(cumulative.wtp_2, aes(x=price, y=per.customers.wtp)) +
    geom_line() + geom_point() + 
    theme_bw() + ylab("Percent customers willing to pay") + ggtitle("Seraph Guardians WTP(%)")

# plot revenue as a function of price
ggplot(cumulative.wtp_2, aes(x=price, y=pred.revenue)) +
    geom_line() + geom_point() + geom_vline(xintercept=60,linetype=2) +
    theme_bw() + ylab("Predicted revenue") + ggtitle("Seraph Guardians Pred. Revenue")

cumulative.wtp_2

## Game 3: Evercrest
# calculate cumuative customers WTP at given price and expected revenue (# cust WTP * price)
cumulative.wtp_3 <- data.frame()
for (price in seq(min(data_e$gg.max.price), max(data_e$gg.max.price), 5)) {
    num.respondents.wtp <- sum(data_e$gg.max.price >= price)
    cumulative.wtp_3 <- rbind(cumulative.wtp_3, 
                            data.frame(price=price, 
                                       per.customers.wtp=num.respondents.wtp/nrow(data_e),
                                       pred.revenue=num.respondents.wtp*price))
}

# plot customers willing to pay as a function of price
library(ggplot2)
ggplot(cumulative.wtp_3, aes(x=price, y=per.customers.wtp)) +
    geom_line() + geom_point() + 
    theme_bw() + ylab("Percent customers willing to pay") + ggtitle("Evercrest WTP(%)")

# plot revenue as a function of price
ggplot(cumulative.wtp_3, aes(x=price, y=pred.revenue)) +
    geom_line() + geom_point() + geom_vline(xintercept=60,linetype=2) +
    theme_bw() + ylab("Predicted revenue") + ggtitle("Evercrest Pred. Revenue")

cumulative.wtp_3


### regression analysis for Warrior Guild
# First, we need to find the baseline
reg3 <- lm(gg.max.price ~ factor(cluster), data=data_w)
summary(reg3)
# We can see that the cluster3 has the most negative score -2.527, we'll set cluster 3 as the base.
reg4 <- lm(gg.max.price ~ relevel(factor(cluster), ref = 3), data = data_w)
summary(reg4)

### regression analysis for Seraph Guardians
reg5 <- lm(gg.max.price ~ factor(cluster), data=data_s)
summary(reg5)
# We can see that the cluster2 has the most negative score -40.002, we'll set cluster 2 as the base.
reg6 <- lm(gg.max.price ~ relevel(factor(cluster), ref = 2), data=data_s)
summary(reg6)


### regression analysis for Evercrest
reg7 <- lm(gg.max.price ~ factor(cluster), data=data_e)
summary(reg7)
# We can see that the cluster4 has the most negative score -23.404, we'll set cluster 5 as the base.
reg8 <- lm(gg.max.price ~ relevel(factor(cluster), ref = 5), data=data_e)
summary(reg8)


## Common Information
potential_customer <- 3000000
fixed_cost <- 7000000
## Game 1: WG
add_cost_wg <- 5000000
price_wg <- 50
wg_per_wtp <- 0.8730
gross_wg <- price_wg*potential_customer*wg_per_wtp
revenue_wg <- gross_wg*0.95 - fixed_cost - add_cost_wg
netrevenue_wg <- 10000000*(1-0.3)+40000000*(1-0.25)+(revenue_wg-50000000)*(1-0.2)

## Game 2: SG
add_cost_sg <- 5500000
price_sg <- 60
sg_per_wtp <- 0.8841
gross_sg <- price_sg*potential_customer*sg_per_wtp
revenue_sg <- gross_sg*0.95 - fixed_cost - add_cost_sg
netrevenue_sg <- 10000000*(1-0.3)+40000000*(1-0.25)+(revenue_sg-50000000)*(1-0.2)

## Game 2: E
add_cost_e <- 6000000
price_e <- 60
e_per_wtp <- 0.8084
gross_e <- price_e*potential_customer*e_per_wtp
revenue_e <- gross_e*0.95 - fixed_cost - add_cost_e
netrevenue_e <- 10000000*(1-0.3)+40000000*(1-0.25)+(revenue_e-50000000)*(1-0.2)



