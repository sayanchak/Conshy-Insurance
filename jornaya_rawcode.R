# SAYAN CHAKRABORTI
# RAW CODE:
# I'd prefer you read my RMarkdown file because I
# explain my work better on that file than here

library(rmarkdown)
library(Amelia)
library(ggplot2)
library(corrplot)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(pROC)
df<- read.csv('client_leads_with_outcomes.csv')
head(df)

# Check to see if dataset needs cleaning
any(is.na(df))
missmap(df,y.at=c(1),col=c('yellow','black'))

# drop the token column
df<- df[,-1]

# general structure and summary of data
str(df)
summary(df)

# get total lead_cost in order to convince Conshy on buying jornaya
sum(df$lead_cost)

# get percentage where purchase=1 for purchase column
summary(factor(df$purchase))
29/560

# plot distrubution of 'lead_cost' with 'purchase' as factor
pl_purchase_dist<-  ggplot(df, aes(x=lead_cost))+
  geom_bar(color='black',aes(fill=factor(purchase)))+ 
  scale_x_continuous(breaks = c(25,50,75,100))+
  ggtitle('Lead costs and proportion of purchases')
print(pl_purchase_dist)


# build heatmap for 'purchase' column and showcase importance
# of using jornaya data.
pl_1_purchase <- ggplot(df,aes(x = lead_age,y=lead_duration))+
  geom_density_2d_filled(bins=6,show.legend = F, aes(color=purchase))+
  ggtitle('Heatmap of Purchase for Lead Age vs Lead Duration')
print(pl_1_purchase)
# I want to get the best value leads to persuade Conshy Insurance
# to buy Jornaya Services.
# subset df based where lead_cost <= 50
df_bestvalueleads <-df[df$lead_cost<=50,]
str(df_bestvalueleads)
pl_1_purchase_bestvalue <- ggplot(df_bestvalueleads,aes(x = lead_age,y=lead_duration))+
  geom_density_2d_filled(bins=6,show.legend = F, aes(color=purchase))+
  ggtitle('Best Value Leads (lead_cost<=50): 
          Heatmap of Purchase for Lead Age vs Lead Duration')
print(pl_1_purchase_bestvalue)
# Plots for the other variables
pl_2_purchase <- ggplot(df,aes(x = lead_age,y=field_count))+
  geom_density_2d_filled(bins=6,show.legend = F,aes(color=purchase))+
  ggtitle("pl_2_purchase: Heatmap of Purchase for Lead Age vs Field Count")
print(pl_2_purchase)
pl_3_purchase <- ggplot(df,aes(x = lead_age,y=competitors))+
  geom_density_2d_filled(bins=6,show.legend = F,aes(color=purchase))+
  ggtitle("pl_3_purchase: Heatmap of Purchase for Lead Age vs Competitors")
print(pl_3_purchase)
pl_4_purchase <- ggplot(df,aes(x = lead_duration,y=field_count))+
  geom_density_2d_filled(bins=6,show.legend = F,aes(color=purchase))+
  ggtitle("pl_4_purchase: Heatmap of Purchase for Lead Duration vs Field Count")
print(pl_4_purchase)
pl_5_purchase <- ggplot(df,aes(x = lead_duration,y=competitors))+
  geom_density_2d_filled(bins=6,show.legend = F,aes(color=purchase))+
  ggtitle("pl_5_purchase: Heatmap of Purchase for Lead Duration vs Competitors")
print(pl_5_purchase)
pl_6_purchase <- ggplot(df,aes(x = field_count,y=competitors))+
  geom_density_2d_filled(bins=6,show.legend = F,aes(color=purchase))+
  ggtitle("pl_6_purchase: Heatmap of Purchase for Field Count vs Competitors")
print(pl_6_purchase)

# MODEL BUILDING FOR PURCHASE VARIABLE

# extract original data
df<- read.csv('client_leads_with_outcomes.csv')
df<- df[,-1]
head(df)

# get only numerical column to build correlation matrix
num.cols <- sapply(df, is.numeric)
cor.data <- cor(df[,num.cols])
corrplot(cor.data,method='color')
# linear reg wont work for this dataset so we move onto
# binary classification algorithms

# Create 80/20 split for train/test data set
set.seed(101) 
sample <- sample.split(df$purchase, SplitRatio = 0.80)
train = subset(df, sample == TRUE)
test = subset(df, sample == FALSE)

# LOGISTIC MODEL
logistic.model <- glm(purchase ~ ., family = binomial(logit), 
                     data = train)
summary(logistic.model)
logistic.model.preds <-  predict(logistic.model, test)
# evaluate accuracy from confusion matrix
table1<-table(test$purchase, logistic.model.preds > 0.5)
table1
# parse confusion matrix and get TP, TN, FP, FN
table1<-as.data.frame(table1)
# accuracy = (TP+TN)/(TP+TN+FP+FN)
logistic.model.accuracy<- (table1[,'Freq'][1]+table1[,'Freq'][4])/
  (table1[,'Freq'][1]+table1[,'Freq'][4]+table1[,'Freq'][2]+table1[,'Freq'][3])

# DECISION TREE
tree.model <- rpart(purchase ~ .,   data=train)
# tree model with labels
prp(tree.model)
tree.model.preds<-predict(tree.model, test)
table2<-table(test$purchase, tree.model.preds > 0.5)
table2
# parse confusion matrix and get TP, TN, FP, FN
table2<-as.data.frame(table2)
# accuracy = (TP+TN)/(TP+TN+FP+FN)
tree.model.accuracy<- (table2[,'Freq'][1]+table2[,'Freq'][4])/
  (table2[,'Freq'][1]+table2[,'Freq'][4]+table2[,'Freq'][2]+table2[,'Freq'][3])


# RANDOM FOREST
forest.model <- randomForest(purchase ~ .,   data=train)
forest.model.preds<-predict(forest.model, test)
table3<-table(test$purchase, forest.model.preds > 0.5)
table3
# parse confusion matrix and get TP, TN, FP, FN
table3<-as.data.frame(table3)
# accuracy = (TP+TN)/(TP+TN+FP+FN)
forest.model.accuracy<- (table3[,'Freq'][1]+table3[,'Freq'][4])/
  (table3[,'Freq'][1]+table3[,'Freq'][4]+table3[,'Freq'][2]+table3[,'Freq'][3])


# COMPARE DIFFERENT MODEL ACCURACIES
logistic.model.accuracy
tree.model.accuracy
forest.model.accuracy

# COMPARE MODELS USING AUC and ROC for all 3 models
roc(test$purchase,logistic.model.preds,plot=T, 
    main='Logistic Model: AUC=0.967', col='red')
roc(test$purchase, tree.model.preds,plot=T, 
    main='Decision Tree Model: AUC =0.9772', col='red')
roc(test$purchase, forest.model.preds,plot=T, 
    main='Random Forest Model: AUC=0.9575',col='red')
