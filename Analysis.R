setwd("D:/STUDIES/BA with R/PROJECT")

hmeq<- read.csv("hmeq_cleaned.csv")

#Imputation
summary(hmeq)
hmeq$MORTDUE[is.na(hmeq$MORTDUE)] <-  median(hmeq$MORTDUE, na.rm = TRUE)
hmeq$VALUE[is.na(hmeq$VALUE)] <-  median(hmeq$VALUE, na.rm = TRUE)
hmeq$REASON[hmeq$REASON == ""] <-   'HomeImp'
hmeq$JOB[hmeq$JOB == ""] <- 'Other'
hmeq$YOJ[is.na(hmeq$YOJ)] <-  median(hmeq$YOJ, na.rm = TRUE)
hmeq$DEROG[is.na(hmeq$DEROG)] <-  median(hmeq$DEROG, na.rm = TRUE)
hmeq$DELINQ[is.na(hmeq$DELINQ)] <-  median(hmeq$DELINQ, na.rm = TRUE)
hmeq$CLAGE[is.na(hmeq$CLAGE)] <-  mean(hmeq$CLAGE, na.rm = TRUE)
hmeq$NINQ[is.na(hmeq$NINQ)] <-  median(hmeq$NINQ, na.rm = TRUE)
hmeq$CLNO[is.na(hmeq$CLNO)] <-  median(hmeq$CLNO, na.rm = TRUE)
hmeq$DEBTINC[is.na(hmeq$DEBTINC)] <-  median(hmeq$DEBTINC, na.rm = TRUE)

#Converting the categorical columns to Numerical as new columns
hmeq$ReasonNum <- factor(hmeq$REASON,
                         levels = c("DebtCon", "HomeImp"),
                         labels = c("1", "2"))

hmeq$JobNum <- factor(hmeq$JOB,
                      levels = c("Mgr", "Office", "ProfExe", "Sales", "Self", "Other"),
                      labels = c("1", "2", "3", "4", "5", "6"))

#Removing the categorical columns from dataframe
hmeq <- subset(hmeq, select = -c(REASON, JOB))


#Creating training & validation datasets (30% Training & 70% Validation)
hmeq_data <- sort(sample(nrow(hmeq), nrow(hmeq)*.7))
train.df <- hmeq[hmeq_data,]
valid.df <- hmeq[-hmeq_data,]


install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

#Plotting the decision tree
default.ct <- rpart(BAD ~ ., data = train.df,maxdepth = 3, method = "class")
prp(default.ct,type = 1, extra=1, varlen=10, under = TRUE)

#Confusion matrix for Training data
library(caret)
default.ct.pred.train <- predict(default.ct,train.df,type = "class")
confusionMatrix(default.ct.pred.train, as.factor(train.df$BAD))

#Confusion matrix for Validation data

default.ct.pred.valid <- predict(default.ct,valid.df,type = "class")
confusionMatrix(default.ct.pred.valid, as.factor(valid.df$BAD))

#ROC for Decision Tree
library(pROC)
default.ct <- rpart(BAD ~ ., data = train.df,maxdepth = 3, method = "class")
roc.default.ct.pred.valid <- predict(default.ct, valid.df,type = "prob")
r <- roc(valid.df$BAD,roc.default.ct.pred.valid[,1])
plot.roc(r)
auc(r)

#LOGIT METHOD

hmeq_logit_data<- hmeq_data


logittrain.df <- train.df
logitvalid.df <- valid.df

logit.reg <- glm(BAD ~ ., data = logittrain.df, family = "binomial") 
options(scipen=999)
summary(logit.reg)

#Confusion matrix for training data
logit.reg.pred.train <- predict(logit.reg, logittrain.df, type = "response")
confusionMatrix(as.factor(ifelse(logit.reg.pred.train > 0.5, 1, 0)), as.factor(train.df$BAD))

#Confusion matrix for validation data
logit.reg.pred.valid <- predict(logit.reg, logitvalid.df, type = "response")
confusionMatrix(as.factor(ifelse(logit.reg.pred.valid > 0.5, 1, 0)), as.factor(valid.df$BAD))

#ROC for Logistic Regression
r1 <- roc(logitvalid.df$BAD, logit.reg.pred.valid)
plot.roc(r1)
auc(r1)