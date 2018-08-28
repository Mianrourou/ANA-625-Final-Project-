mydata=CableTVSubscribersData
#MDA PART
library(psych)
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
#checking class of variables
class(CableTVSubscribersData$age)
class(CableTVSubscribersData$Segment)
class(CableTVSubscribersData$subscribe)
class(CableTVSubscribersData$income)
class(CableTVSubscribersData$kids)
class(CableTVSubscribersData$ownHome)
#attach(can use variables without data$variable)
attach(CableTVSubscribersData)
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

#delet entire row of missing values(however, we dont have any missing values for this data)
# ----na.omit()----
#logistic regression
model=glm(subscribe~age+gender+income+kids+ownHome+Segment,data=CableTVSubscribersData,family=binomial)
#check model
model
#summary of model
summary(model)

#plot for each categori level
#adjusted line for each categori level
abline()
#review chapter 2 for EDA parts

