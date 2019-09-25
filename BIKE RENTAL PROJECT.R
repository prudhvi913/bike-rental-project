# clearing the environment
rm(list=ls())

# setting the working directory
setwd("D:/R and PYTHON files/data set/project 1")

# checking the working directory
getwd()

#Load Libraries
install.packages (c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
                    "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees'))


# loading the data in environment (in object bike_df)
bike_df=read.csv("day.csv",header=TRUE)

# missing value anlysis
sum(is.na(bike_df))      # there is no missing value in this dataset.

# exploring the data 
str(bike_df)

# converting the variables in their respective datatype.
bike_df$season=as.factor(bike_df$season)
bike_df$yr=as.factor(bike_df$yr)
bike_df$mnth=as.factor(bike_df$mnth)
bike_df$holiday=as.factor(bike_df$holiday)
bike_df$weekday=as.factor(bike_df$weekday)
bike_df$workingday=as.factor(bike_df$workingday)
bike_df$weathersit=as.factor(bike_df$weathersit)

# seperating the numerical variables and categorical variables.
numeric_index=sapply(bike_df,is.numeric)
numeric_data=bike_df[,numeric_index]
cnames=colnames(numeric_data) #numerical variables.

factor_index=sapply(bike_df,is.factor)
factor_data=bike_df[,factor_index]
pnames=colnames(factor_data)  #categorical variables. 

#outlier dedection and deletion.
library("scales")
library("psych")
library("ggplot2")
library("gplot")
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "cnt"), data = subset(bike_df))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "blue" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="cnt")+
           ggtitle(paste("Box plot of cnt for",cnames[i])))
}
# Plotting plots together
gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
gridExtra::grid.arrange(gn4,gn5,ncol=2)
gridExtra::grid.arrange(gn6,gn7,gn8,ncol=2)

# removing outlier
for (i in cnames){
  print(i)
  value=bike_df[,i][bike_df[,i]%in%boxplot.stats(bike_df[,i])$out]
}
bike_df=bike_df[which(!bike_df[,i]%in%value),] 

# correlation plot for numerical variables. 
library(corrgram)
corrgram(bike_df[,cnames],order=F,upper.panel=panel.pie,text.panel=panel.txt,mean="correlation plot")

# chi^2 test for categorical variables.
for(i in 1:8 ){
  

  print(names(factor_data)[i])
  print(chisq.test(table(factor_data[,i])))
}

# deleting the features which are not wanting.
bike_del= subset(bike_df,select=-c(atemp,dteday,instant,season,yr,mnth,weekday))

# preparing the data for forecast and predict
str(bike_del)
train.index = sample(1:nrow(bike_del), 0.8 * nrow(bike_del))
train = bike_del[ train.index,]
test  = bike_del[-train.index,]



#defining the function (to find the error percentage)
mape=function(av,pv){
  mean(abs((av-pv)/av))*100 #av=actual value and pv= predicted value
}



#decision tree regression model
library(rpart)
data1=rpart(cnt~.,data=train,method="anova")
predictions_tree=predict(data1,test[,-9])
summary(data1)
mape(test[,9],predictions_tree)
#error rate=13.00%
#accuracy =87.00 %



#random forest
library(randomForest)
random_model = randomForest(cnt~ ., train, importance = TRUE, ntree = 2000)

#Extract rules fromn random forest
#transform rf /object to an inTrees' format
library(inTrees)
treeList = RF2List(random_model)  

#Extract rules
rules= extractRules(treeList, train[,-14])

#Visualize some rules
rules[1:2,]
#Make rules more readable:
readrules = presentRules(rules, colnames(train))
readrules[1:2,]

#Predict test data using random forest model
RF_Predictions = predict(random_model, test[,-9])

# accuracy check
mape((test[,9]),RF_Predictions)
#error=7.5 %
#accuracy=93.5

# EDA
qqnorm(bike_df$cnt)
hist(bike_df$cnt)# visualizing few graphs.

# writing the output to hard disk
write(capture.output(predictions_tree),"DTPR.txt")


#freezing the model random forest with high accuracy.

