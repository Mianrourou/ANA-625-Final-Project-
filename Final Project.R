#EDA PART
library(psych)
par(mfrow=c(2,3))
#scater plot matrix for general data set
pairs.panels (CableTVSubscribersData[c("age", "gender", "income", "kids","subscribe","Segment")])
#box plot for Income and Subscribe
boxplot(CableTVSubscribersData$income~CableTVSubscribersData$subscribe,main="Boxplot for Income and Subscribe",col="red",ylab="Income($)",xlab="Subscribtion")
#box plot for age and subscribe
boxplot(CableTVSubscribersData$age~CableTVSubscribersData$subscribe,main="Boxplot for Age and Subscribe",col="red",ylab="Age(Years)",xlab="Subscribtion")
#QQline for Income
qqnorm(CableTVSubscribersData$income,main = "quantile-quantile plots for Income")
#QQline for Age
qqnorm(CableTVSubscribersData$age,main = "quantile-quantile plots for Age")
#Density for income
plot(density(CableTVSubscribersData$income),main = "Density Plot for Income")
#checking categorical data if as factor
str(CableTVSubscribersData)
#having a overview of the data
summary(CableTVSubscribersData)
#attach(can use variables without data$variable)
attach(CableTVSubscribersData)
hist(income)
#levels
levels(age)
levels(Segment)
levels(subscribe)
levels(income)
levels(kids)
levels(ownHome)
#checking missing values
is.na(CableTVSubscribersData)
#making variable as factor if R does not recongnize it as factor
as.factor(CableTVSubscribersData$gender)
dim(CableTVSubscribersData)
dimnames(CableTVSubscribersData)
#delet entire row of missing values(however, we dont have any missing values for this data)
# ----na.omit()----
#logistic regression
model=glm(subscribe~.,data=CableTVSubscribersData,family=binomial)
#summary of model
summary(model)
head(predict(model, type = "response"))

# "manually" convert the probabilities to classifications
# a midpoint value such as 0.5 is used to "categorize" the probabilities
cable_pred <- ifelse(predict(model, type = "response") > 0.08, "subYes", "subNo")
head(cable_pred)

# Logistic Regression Model Evaluation using a cross-table: confusion matrix (CM)
# Making predictions on the train set.
trn_tab <- table(predicted = cable_pred, actual = CableTVSubscribersData$subscribe)
trn_tab 

library("caret")
confusionMatrix(data=trn_tab , reference = CableTVSubscribersData,positive = "subYes")

skew(CableTVSubscribersData$age)
skew(CableTVSubscribersData$income)
#new model by removing kid variable
newdata=CableTVSubscribersData
#newdata=bin(CableTVSubscribersData$age,nbins = 6,labels =c("19-29","30-39","40-49","50-59","60-69","70-80"))
#cut(CableTVSubscribersData$age,6,include.lowest = T,labels = c("19-29","30-39","40-49","50-59","60-69","70-80"))

newdata$age[age > 70.9] <- "70-80"
newdata$age[age > 60 & age <= 69.9] <- "60-69"
newdata$age[age > 50 & age <= 59.9] <- "50-59"
newdata$age[age > 40 & age <= 49.9] <- "40- 49"
newdata$age[age > 30 & age <= 39.9] <- "30- 39"
newdata$age[age <= 29.999] <- "19-29"
View(newdata)

#logistic regression
newmodel=glm(subscribe~.,data=newdata,family=binomial)
#summary of model
summary(newmodel)
head(predict(newmodel, type = "response"))

# "manually" convert the probabilities to classifications
# a midpoint value such as 0.5 is used to "categorize" the probabilities
newcable_pred <- ifelse(predict(newmodel, type = "response") > 0.08, "subYes", "subNo")
head(newcable_pred)

# Logistic Regression Model Evaluation using a cross-table: confusion matrix (CM)
# Making predictions on the train set.
newtrn_tab <- table(newpredicted = newcable_pred, newactual = newdata$subscribe)
newtrn_tab 

library("caret")
confusionMatrix(data=newtrn_tab , reference = newdata,positive = "subYes")

#creating .csv from newdata
write.csv(newdata, file = "newdata.cvs")