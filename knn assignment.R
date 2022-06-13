
library(e1071)
library(caTools)
library(class)

dataset=read.csv("titanic.csv")
View(dataset)

#checking datatypes

str(dataset)

#datatype conversion
dataset$ticket = gsub("PC","",dataset$ticket) #removed brackets or symbol
View(dataset)
dataset$ticket = gsub("W.E.P.","",dataset$ticket) 

dataset$ticket = gsub("WE/P","",dataset$ticket) 

dataset$ticket = gsub("F.C.","",dataset$ticket) 

dataset$ticket = gsub("P/PP","",dataset$ticket) 

dataset$ticket = gsub("C.A.","",dataset$ticket) 

dataset$ticket = gsub("C.A./SOTON ","",dataset$ticket) 

dataset$ticket = gsub("S.O.P.","",dataset$ticket) 

dataset$ticket = gsub("F.C.C.","",dataset$ticket) 

dataset$ticket = gsub("SC/AH","",dataset$ticket) 

dataset$ticket = gsub("W./C.","",dataset$ticket) 

dataset$ticket = gsub("S.O.C.","",dataset$ticket) 

dataset$ticket = gsub("S.W./PP","",dataset$ticket) 

dataset$ticket = gsub("SC/PARIS","",dataset$ticket) 

dataset$ticket = gsub("SC/AH Basle","",dataset$ticket) 

dataset$ticket = gsub("S.O./P.P.","",dataset$ticket) 

dataset$ticket = gsub("SOTON/O2","",dataset$ticket) 

dataset$ticket = gsub("SC/AH Basle","",dataset$ticket) 

dataset$ticket = gsub("SOTON/O.Q.","",dataset$ticket) 

dataset$ticket = gsub("C.","",dataset$ticket) 


#converting categorical variables

dataset$sex=as.factor(dataset$sex)
View(dataset$sex)
dataset$sex=factor(dataset$sex,levels = c("female","male"),labels = c(1,2))

dataset$embarked=as.factor((dataset$embarked))
View(dataset$embarked)
dataset$embarked=factor(dataset$embarked,labels = c(1,2,3,4))


dataset$pclass=as.factor(dataset$pclass)
dataset$survived <- as.factor(dataset$survived)
dataset$embarked <- as.factor(dataset$embarked)



dataset$ticket=as.numeric(dataset$ticket)

str(dataset)

#checking na values

summary(is.na(dataset$ticket))

summary((is.na(dataset)))

is.na(dataset)


#filling na values with mean

dataset$ticket=ifelse(is.na(dataset$ticket),
                      ave(dataset$ticket,FUN=function(x) 
                        mean(x,na.rm=TRUE)),dataset$ticket)

dataset$age=ifelse(is.na(dataset$age),
                   ave(dataset$age,FUN=function(x) 
                     mean(x,na.rm=TRUE)),dataset$age)

dataset$fare=ifelse(is.na(dataset$fare),
                    ave(dataset$fare,FUN=function(x) 
                      mean(x,na.rm=TRUE)),dataset$fare)


str(dataset)


#removing column

dataset=subset(dataset,select= -(cabin))
dataset=subset(dataset,select= -(name))


#splitting the data into training and testing sets

split=sample.split(dataset,SplitRatio=0.7)

training_set=subset(dataset,split==TRUE)
testing_set=subset(dataset,split==FALSE)

View(training_set)
View(testing_set)

#feature scaling

train_scale=scale(training_set[,4:8])
test_scale=scale(testing_set[,4:8])
View(train_scale)
View(test_scale)

#fitting the knn model

classifier_knn=knn(train=train_scale,
                   test=test_scale,
                   cl=training_set$survived,
                   k=3)

View(classifier_knn)

classifier_knn


View(classifier_knn)

#confusion Matrix

cm=table(testing_set$survived,classifier_knn)
View(cm)
