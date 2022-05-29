## ----setup, include=FALSE----------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(message = FALSE,warning=FALSE)


## ----------------------------------------------------------------------------------------------------------------------------------
#Load data
df2 <- read.csv("covid_19_data.csv")
head(df2, 2)


## ----------------------------------------------------------------------------------------------------------------------------------
library(dplyr)
#check all missing values
sapply(df2, function(k) sum(is.na(k)))
#exclude all rows with negative values
df2 <- df2[df2$Recovered >= 0, ]
df2 <- df2[df2$Recovered < 100000, ]

#clean data
df2 <- subset(df2, select = c(Recovered,Deaths,Confirmed,Country.Region))
 #create a new variable that has classes
df2 <- df2 %>% mutate(Recovery_Rate =
                     case_when(Recovered < 10044~ "0",
                              Recovered >= 10044 ~ "1")
)

df2$Recovery_Rate <- as.factor(df2$Recovery_Rate)#convert column spread_rate into factor

head(df2,3)


## ----------------------------------------------------------------------------------------------------------------------------------
#load libraries
library(MASS)
library(tidyverse)
library(caret)

library(ggplot2)
library(MASS)
library(mvtnorm)
theme_set(theme_classic())


## ----------------------------------------------------------------------------------------------------------------------------------
# Split the data into training (80%) and test set (20%)
df <- df2[1:200000,]
df <- na.omit(df)
set.seed(123)
training.individuals <- df$Recovery_Rate %>% 
            createDataPartition(p = 0.8, list = FALSE)
train.data <- df[training.individuals, ]
test.data <- df[-training.individuals, ]
dim(train.data)
dim(test.data)


## ----------------------------------------------------------------------------------------------------------------------------------
#create LDA model
ldamodel = lda (factor(Recovery_Rate)~Recovered + Deaths +Confirmed, data=train.data)
ldamodel

plot(ldamodel)


## ----------------------------------------------------------------------------------------------------------------------------------
##Predicting training results.
ldaPrediction = predict(ldamodel, data=train.data)
table(Predicted=ldaPrediction$class, Recovery_Rate=train.data$Recovery_Rate)



## ----------------------------------------------------------------------------------------------------------------------------------
library(klaR)

partimat(Recovery_Rate~Recovered+ Deaths+Confirmed, method="lda",data=test.data, main= "Partition Plots")



## ----------------------------------------------------------------------------------------------------------------------------------
# Model accuracy
mean(ldaPrediction$class==test.data$Recovery_Rate)*100

predmodeltestlda = predict(ldamodel, newdata=test.data)#prediction on test data
table(Predicted=predmodeltestlda$class, Recovery_Rate=test.data$Recovery_Rate)


## ---- fig.width=7, fig.height=5----------------------------------------------------------------------------------------------------
#plot classes
#install.packages("pROC")

#library(pROC)

par(pty = "s")

roc(test.data$Recovery_Rate,predmodeltestlda$posterior[,2], plot=TRUE,legacy.axes = TRUE, 
percent =TRUE, xlab="False Positive Percentage", ylab="True Positive Percentage", main="ROC Curve")




## ----------------------------------------------------------------------------------------------------------------------------------
# For decision tree model
library(rpart)
# For data visualization
library(rpart.plot)


## ----------------------------------------------------------------------------------------------------------------------------------
#filter data
df3 <- df[10000:20000,]
df3 <- na.omit(df3)
set.seed(234)
#test and training data
train = sample(1:nrow(df3))
Recoverytrain=df3[train,]


## ----------------------------------------------------------------------------------------------------------------------------------
# Build the regression tree on the training set
fit.tree = rpart(Recovered~ ., data=Recoverytrain, control=rpart.control(cp=0.08))
summary(fit.tree)
#fit.tree


## ----------------------------------------------------------------------------------------------------------------------------------
#visualize regression tree
rpart.plot(fit.tree)



## ----------------------------------------------------------------------------------------------------------------------------------
#check variable importance
fit.tree$variable.importance


## ----------------------------------------------------------------------------------------------------------------------------------
##finding the best cp involves overfitting the data by choosing  smaller cp value
myoverfittedtree=rpart(Recovered~ ., data=Recoverytrain,control=rpart.control(cp=0.008))

# to see out of sample performance of a tree

printcp(myoverfittedtree)
plotcp(myoverfittedtree)

# find cp value that minimizes the error

opt_cp=myoverfittedtree$cptable[which.min(myoverfittedtree$cptable[,"xerror"]),"CP"]

# the best tree
opt_cp

# best out of sample performances

mybesttree=rpart(Recovered~ ., data=Recoverytrain,control=rpart.control(cp=opt_cp))
printcp(mybesttree)
rpart.plot(mybesttree)



## ----------------------------------------------------------------------------------------------------------------------------------
#data preparation
library(factoextra)
df5 <- df2[3000:3039,]
df4 <- subset(df5, select = c(Recovered,Deaths,Confirmed,Recovery_Rate))

# Omitting any NA values
df4 <- na.omit(df4)
 
# Scaling dataset
#df4 <- scale(df4)



## ----------------------------------------------------------------------------------------------------------------------------------
#distance matrix
dist_mat <- dist(df4, method = 'euclidean')


## ---- fig.width=10, fig.height=7---------------------------------------------------------------------------------------------------
#first clusters dendrogram
hclust_avg <- hclust(dist_mat, method = 'average')
hclust_avg
plot(hclust_avg)


## ---- fig.width=10, fig.height=7---------------------------------------------------------------------------------------------------
#cut the dendrogram tree into 2 clusters
cut_avg <- cutree(hclust_avg, k = 2)
plot(hclust_avg)
rect.hclust(hclust_avg , k = 2, border = 2:6)
abline(h = 2, col = 'red')


## ---- fig.width=10, fig.height=7---------------------------------------------------------------------------------------------------
#install.packages('package_name', dependencies = TRUE)
suppressPackageStartupMessages(library(dendextend))
avg_dend_obj <- as.dendrogram(hclust_avg)
avg_col_dend <- color_branches(avg_dend_obj)
plot(avg_col_dend, main="Covid 19 Recovery Clusters ")



## ----------------------------------------------------------------------------------------------------------------------------------
#cluster values
suppressPackageStartupMessages(library(dplyr))
df_cluster <- mutate(df4, cluster = cut_avg)
count(df_cluster,cluster)


## ----------------------------------------------------------------------------------------------------------------------------------
#create a target
target = ifelse(df4$Recovery_Rate=="0","0","1")
table(target)
clusters =cutree(hclust_avg, k=2)
table(clusters,target)


## ----------------------------------------------------------------------------------------------------------------------------------
#accuracy of the hierarchical clustering
cm = as.matrix(table(Actual = target, Predicted = clusters))
cm
accuracy = sum(diag(cm)) / sum(cm) 
print(accuracy*100)




