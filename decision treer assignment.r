#import dataset

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

dataset$ticket[is.na(dataset$ticket)]=mean(dataset$ticket,na.rm = TRUE)

dataset$age=ifelse(is.na(dataset$age),
                     ave(dataset$age,FUN=function(x) 
                       mean(x,na.rm=TRUE)),dataset$age)

dataset$fare=ifelse(is.na(dataset$fare),
                   ave(dataset$fare,FUN=function(x) 
                     mean(x,na.rm=TRUE)),dataset$fare)


str(dataset)



beng=dataset

#removing column

dataset=subset(dataset,select= -(cabin))
dataset=subset(dataset,select= -(name))

# CREATING SUBDATASET

dataset=dataset[ ,2:11]
View(dataset)

#encoding the target feature as factor

dataset$survived=factor(dataset$survived,levels = c(0,1))


#splitting data into trainig and testing set

library(caTools)

set.seed(123)

split=sample.split(dataset$survived,SplitRatio = 0.75)

training_set=subset(dataset,split ==TRUE)
View(training_set)
testing_set=subset(dataset,split ==FALSE)
View(testing_set)


#fit the decision tree classification using training set

install.packages('rpart')

library(rpart)
classifier=rpart(formula = survived~sex+age+pclass,data=training_set)
summary(classifier)

#prediction
y_pred =predict(classifier,newdata = testing_set,type='class')
View(y_pred)

#making the confusion matrix
cm=table(testing_set[,2],y_pred)
View(cm)

#prediction of survival
a<-data.frame(age=c(50),pclass=c("1"),sex=c("female","male") )
result<-predict(classifier,a)
print(result)

#plot the tree
plot(classifier)
text(classifier)
