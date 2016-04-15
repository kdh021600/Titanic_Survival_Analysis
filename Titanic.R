# Read the data
train <- read.csv(file="train.csv",header=TRUE, sep=",");
test <- read.csv(file="test.csv",header=TRUE, sep=",");

# to combine both dataset make "surived" variable in test
test.survived <- data.frame(Survived = rep("None",nrow(test)),test[,])
data.combined <- rbind(train,test.survived)
str(data.combined)

#converting pclass into factor rather than integer
data.combined$Pclass <- as.factor(data.combined$Pclass)
data.combined$Survived <- as.factor(data.combined$Survived)

# Distribusion of Survival
table(data.combined$Survived)

#Distribution of pclass
table(data.combined$Pclass)
barplot(table(data.combined$Pclass))


##Distribution of sex by pclass
xtabs( ~Pclass+ Sex ,data=train)
table(train$Pclass,train$Sex)

#Hynothesis - Rich people(1st) survival is higher or not
library(ggplot2)
train$Pclass <- as.factor(train$Pclass)
str(train)

## X has to be continuos to plot this. that is why reading train data set again is reqiured
ggplot(data=train) +
  geom_histogram(aes(x=Pclass,fill=factor(Survived)),binwidth=0.5) +
  xlab("Pclass") +
  ylab("Count") +
  labs(fill="Survived")


## X is factor for the following code
ggplot(train,aes(x=Pclass,fill=factor(Survived))) +
  geom_bar(width=0.5) + ##stat_count(width=0.5) also works fine
  xlab("Pclass") +
  ylab("Count") +
  labs(fill="Survived")


# how many unique names are there in combined data
length(unique(as.character(data.combined$Name)))

#find out duplicated names
dup_name<-as.character(data.combined[which(duplicated(as.character(data.combined$Name))),"Name"])

#is the data duplicated  or they are different people
data.combined[which(data.combined$Name %in% dup_name),]

library(stringr)
# Miss,Mr., Mrs. has any correlation????
#get all the data that containes "Miss."
misses<-data.combined[which(str_detect(data.combined$Name,"Miss.")),]
misses[1:5,1:6]
# 127 misses were survived 55 were drowned
table(misses$Survived)

#same for missus
missus<-data.combined[which(str_detect(data.combined$Name,"Mrs.")),]
missus[1:5,1:8]


#check the same with the sex(man)
males<-data.combined[which(train$Sex== "male"),]
head(males[,1:5])
table(males$Survived)


#extracting the title out of the names to see correlation between title name survival and pclass
data.combined$Name <- as.character(data.combined$Name)
#finding Misses in the data with grep function
head(data.combined[grep("Miss.",data.combined$Name),])

#extract function
extract_title<- function(Name) {
  
  if(length(grep("Miss.",Name))>0)
  {
    return("Miss.")
  }
  else if(length(grep("Master",Name))>0)
  {
    return("Master")
  }
  else if(length(grep("Mrs.",Name))>0)
  {
    return("Mrs.")
  }
  else if(length(grep("Mr.",Name))>0)
  {
    return("Mr.")
  }
  else
  {
    return("Other")
  }
}

titles <- NULL
for (i in 1:nrow(data.combined))
{
  titles <- c(titles,extract_title(data.combined[i,"Name"]))
}
data.combined$title <- as.factor(titles)

#now, creating a graph to see the correlation
#using only 891 rows because we have survived variable in train only which has 891 rows
ggplot(data.combined[1:891,],aes(x=title,fill=Survived)) +
  geom_bar(width=0.5)+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Title") +
  ylab("Count") +
  labs(fill="Survived")

# table(data.combined$Sex)
# str(data.combined$Sex)

## ggplot for vizualization of sex and survived
ggplot(data.combined[1:891,],aes(x=Sex,fill=Survived)) +
  geom_bar(width=0.5)+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Sex") +
  ylab("Count") +
  labs(fill="Survived")

## for age and survival
## facet wrap will do for both  
ggplot(data.combined[1:891,],aes(x=Age,fill=Survived)) +
  geom_histogram(binwidth=5)+
  facet_wrap(~Sex +Pclass)+
  ggtitle("Pclass")+
  xlab("Age") +
  ylab("Count") 

## age of all the Master
## Master is a proxy for Male Children
boys<-data.combined[which(data.combined$title == "Master"),]
summary(boys$Age)
## 8 from the 263 missing values belongs to Master title

## same for Miss
miss <- data.combined[which(data.combined$title == "Miss."),]
summary(miss$Age)
## 50 of 263 missing values belongs to Miss title
## more complicated because age min=0.17 and age max=63

a<-miss[which(miss$SibSp == 0 & miss$Parch == 0),]
a[which(a$Age <= 14.5),]

## ggplot to see correlatoin between age and survival for miss title
ggplot(miss[miss$Survived != "None",],aes(x=Age,fill=Survived)) +
  facet_wrap(~Pclass) +
  geom_histogram(binwidth=5) +
  xlab("Age") +
  ylab("Count")

##Analysis of Siblings and Parents and child
unique(data.combined$SibSp)
data.combined$SibSp <- as.factor(data.combined$SibSp)

## vizualization of siblings and survival
ggplot(data.combined[1:891,],aes(x=SibSp,fill=Survived)) +
  geom_bar(width=0.5)+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Siblings") +
  ylab("Count") +
  labs(fill="Survived")

data.combined$Parch <- as.factor(data.combined$Parch)
## vizualization of parent chindren and survival
ggplot(data.combined[1:891,],aes(x=Parch,fill=Survived)) +
  geom_bar(width=0.5)+
  facet_wrap(~Pclass + title)+
  ggtitle("Pclass")+
  xlab("Parent/Chindren") +
  ylab("Count") +
  ylim(0,300) +
  labs(fill="Survived")


## feature engineering
## creting a family size feature
temp.SibSp <- c(train$SibSp,test$SibSp)
temp.Parch <- c(train$Parch,test$Parch)
data.combined$FamilySize <- as.factor(temp.SibSp + temp.Parch +1)
data.combined$FamilySize <- as.integer(data.combined$FamilySize)
max(data.combined$FamilySize)

## vizualization of family size and survival
ggplot(data.combined[1:891,],aes(x=FamilySize,fill=Survived)) +
  geom_bar(width=0.5)+
  facet_wrap(~Pclass )+
  ggtitle("Pclass")+
  xlab("Family Size") +
  ylab("Count") +
  ylim(0,300) +
  labs(fill="Survived")


## ticket variable
str(data.combined$Ticket)
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

## to see the patterns in the ticket number getting the first char in a var
substr(data.combined$Ticket,1,1)
ticket.first <- ifelse(data.combined$Ticket == "", " ",substr(data.combined$Ticket,1,1))
unique(ticket.first)
data.combined$ticket.first <- as.factor(ticket.first)

## vizualization of ticket first character and survival
ggplot(data.combined[1:891,],aes(x=ticket.first,fill=Survived)) +
  geom_bar(width=0.5)+
  ##facet_wrap(~Pclass )+
  ggtitle("Pclass")+
  xlab("Ticket Number") +
  ylab("Count") +
  ylim(0,300) +
  labs(fill="Survived")

## vizualization of Familysize and survival
ggplot(data.combined[1:891,],aes(x=FamilySize,fill=Survived)) +
  geom_bar(width=0.5)+
  facet_wrap(~Pclass )+
  ggtitle("Pclass")+
  xlab("Family Size") +
  ylab("Count") +
  ylim(0,300) +
  labs(fill="Survived")

## fare variable experiments
max(data.combined$Fare)
data.combined[which(data.combined$Fare < 515 & data.combined$Fare > 510),1:2]

##fare variable
summary(data.combined$Fare)
length(unique(data.combined$Fare))

## treat fare as numeric
ggplot(data.combined[1:891,],aes(x=Fare)) +
  geom_bar(width=5) +
  xlab("Fare") +
  ylab("Count") +
  ylim(0,50)

ggplot(data.combined[1:891,],aes(x=Fare,fill=Survived)) +
  geom_histogram(binwidth=5)+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Family Size") +
  ylab("Count") +
  ylim(0,50) +
  xlim(0,300) +
  labs(fill="Survived")


## Cabin variable
str(data.combined$Cabin)
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:20]

## replace empty cabin number to "U"
data.combined[is.na(data.combined$Cabin),"Cabin"] <- ""
data.combined[which(data.combined$Cabin == ""),"Cabin"] <- "U"

## getting first char out of cabin
cabin.first <- as.factor(substr(data.combined$Cabin,1,1))
levels(cabin.first)

## add it to data.combined

data.combined$cabin.first <- cabin.first
rm(data.combined$cabin.fisrt)

#Vizualize cabin first and survival
ggplot(data.combined[1:891,],aes(x=cabin.first,fill=Survived)) +
  geom_bar()+
  ggtitle("Pclass")+
  xlab("Cabin") +
  ylab("Count") 



ggplot(data.combined[1:891,],aes(x=cabin.first,fill=Survived)) +
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Cabin code") +
  ylab("Count") +
  ylim(0,50) +
  labs(fill="Survived")



## data modeling
## Linear Regression
str(data.combined)
data.combined$Survived <- as.factor(data.combined$Survived)


## Logistic Regression 
mylogit <- glm(Survived ~ Age + Pclass + Sex + SibSp + Parch + Fare + Embarked, data = data.combined, family = "binomial")
summary(mylogit)
exp(coef(mylogit))

## Chi square test
chisq.test(data.combined$Survived,data.combined$Sex)
summary(table(data.combined$Survived,data.combined$Sex))

##mosaic plot of class and survival
mosaicplot(train$Pclass ~ train$Survived,color= c("Red","Blue"),
           xlab="Class",ylab="Survival",main="Survival based on Class")


install.packages(randomForest)
library(randomForest) 

# installing/loading the package:
if(!require(installr)) {
  install.packages("installr"); require(installr)} 
updateR()

hist(train$Age)
struct<-lapply(train,class)

##handling null values 
sum(is.na(train))
train$Pclass[is.na(train$Pclass)] <-names(which.max(table(train$Pclass)))
train$Age[is.na(train$Age)] <- median(train$Age,na.rm=TRUE)
train$SibSp[is.na(train$SibSp)] <-names(which.max(table(train$SibSp)))
train$Parch[is.na(train$Parch)] <- median(train$Parch,na.rm=TRUE)
train$Fare[is.na(train$Fare)] <- median(train$Fare,na.rm=TRUE)
train$Embarked[is.na(train$Embarked) | train$Embarked == ""]<- names(which.max((table(train$Embarked))))

##data 
# train<- train[,!(names(train) %in% c("PassengerId","Name","Ticket","Cabin"))]
# cols <- c("Survived", "Pclass", "Sex", "SibSp","Embarked")
# train[cols] <- lapply(train[cols], factor)
# col2<- c("Survived","Sex","Fare","Pclass")
# train<- train[,col2]

## Random Forset
library(randomForest)

#model 1 (mod1) with Pclass and title
train.mod1 <-  data.combined[1:891,c("Pclass","title")]
mod1.predictor <- as.factor(train$Survived)

#To avoid different output on every run
#To introduce some reproducibility 
#To compare different runs of output
set.seed(1234)
mod1 <- randomForest(x = train.mod1,y=mod1.predictor,importance=TRUE,ntree=1000)
mod1
varImpPlot(mod1)


#model 2 with Pclass, title and Sibsp
train.mod2 <- data.combined[1:891,c("Pclass","title","SibSp")]
mod2.predictor <- as.factor(train$Survived)
set.seed(1234)
mod2 <- randomForest(x=train.mod2,y=mod2.predictor,importance=TRUE,ntree=1000)
mod2
varImpPlot(mod2)

#model 3 with Pclass, title and Parch 
train.mod3 <- data.combined[1:891,c("Pclass","title","Parch")]
mod3.predictor <- as.factor(train$Survived)
set.seed(1234)
mod3 <- randomForest(x=train.mod3,y=mod3.predictor,importance=TRUE,ntree=1000)
mod3
varImpPlot(mod3)

#model 4 with Pclass, title, Parch and SibSp
train.mod4 <- data.combined[1:891,c("Pclass","title","Parch","SibSp")]
mod4.predictor <- as.factor(train$Survived)
set.seed(1234)
mod4 <- randomForest(x=train.mod4,y=mod4.predictor,importance=TRUE,ntree=1000)
mod4
varImpPlot(mod4)


#model 5 with Pclass, title, Familysize
train.mod5 <- data.combined[1:891,c("Pclass","title","FamilySize")]
mod5.predictor <- as.factor(train$Survived)
set.seed(1234)
mod5 <- randomForest(x=train.mod5,y=mod5.predictor,importance=TRUE,ntree=1000)
mod5
varImpPlot(mod5)

#model 6 with Pclass, title, Familysize and Parch
train.mod6 <- data.combined[1:891,c("Pclass","title","FamilySize","Parch")]
mod6.predictor <- as.factor(train$Survived)
set.seed(1234)
mod6 <- randomForest(x=train.mod6,y=mod6.predictor,importance=TRUE,ntree=1000)
mod6
varImpPlot(mod6)

#model 7 with Pclass, title, Familysize and SibSp
train.mod7 <- data.combined[1:891,c("Pclass","title","FamilySize","SibSp")]
mod7.predictor <- as.factor(train$Survived)
set.seed(1234)
mod7 <- randomForest(x=train.mod7,y=mod7.predictor,importance=TRUE,ntree=1000)
mod7
varImpPlot(mod7)


## Cross Validation

#getting only 3 most predictable variable (Pclass,title and FamilySize) to final test dataset
test.final <- data.combined[892:1309,c("Pclass","title","FamilySize")]

#Predictions
mod5.predict <- predict(mod5,test.final)
table(mod5.predict)

#bulding and saving the results into CSV file 
mod5.csv <- data.frame(PassengerId = rep(892:1309),Survived = mod5.predict)
write.csv(mod5.csv,file="mod5_predict.csv",row.names=FALSE)


#10-fold Cross Validation
library(caret)
library(doSNOW)

set.seed(2348)

#to create 10 random folds 
folds <- createMultiFolds(mod5.predictor,k=10,times=10)

#Stratification
table(mod5.predictor)
342/549

table(mod5.predictor[folds[[29]]])
307/494

#Train control object for each observation
train.control1 <- trainControl(method = "repeatedcv",number = 10,repeats =10 ,index =folds)

#to stress the CPU power and multi-core training
cluster <- makeCluster(8,type="SOCK")
registerDoSNOW(cluster)

#train
set.seed(34324)

train.1 <- train(x=train.mod5,y=mod5.predictor,method="rf",tuneLength=3,ntree=1000,
                 trControl = train.control1)

#Stoping the cluster
stopCluster(cluster)

#checking the result
train.1


# now we re still at 76% accuracy which is slightly lower than acuracy in mod5
# so we train less data by creating less numbers of folds
folds2 <- createMultiFolds(mod5.predictor,k = 5,times = 10)
train.control2 <- trainControl(method = "repeatedcv",number = 5,repeats =10 ,index =folds2)
cluster <- makeCluster(8,type="SOCK")
registerDoSNOW(cluster)
set.seed(34324)
train.2 <- train(x=train.mod5,y=mod5.predictor,method="rf",tuneLength=3,ntree=1000,
                 trControl = train.control2)
stopCluster(cluster)
train.2

#let's train out data with 3-Folds CV
folds3 <- createMultiFolds(mod5.predictor,k = 3,times = 10)
train.control3 <- trainControl(method = "repeatedcv",number = 3,repeats =10 ,index =folds3)
cluster <- makeCluster(8,type="SOCK")
registerDoSNOW(cluster)
set.seed(34324)
train.3 <- train(x=train.mod5,y=mod5.predictor,method="rf",tuneLength=3,ntree=1000,
                 trControl = train.control3) 
stopCluster(cluster)
train.3



## CART and Decision Tree ALgorithm
library("rpart")
library("rpart.plot")

#based on random forest, train.2 has the highest accuracy
#so we will do rpart with 5-Folds CV

folds2.rpart <- createMultiFolds(mod5.predictor,k = 5,times = 10)
train.control2.rpart <- trainControl(method = "repeatedcv",number = 5,repeats =10 ,index =folds2)
cluster <- makeCluster(8,type="SOCK")
registerDoSNOW(cluster)
set.seed(34324)
train.2.rpart <- train(x=train.mod5,y=mod5.predictor,method="rpart",tuneLength=30,
                       trControl = train.control2.rpart)
stopCluster(cluster)
train.2.rpart
#plot
prp(train.2.rpart$finalModel,type=0,extra=1,under=TRUE)

#just to check the results for 3-Fold CV
folds3.rpart <- createMultiFolds(mod5.predictor,k = 3,times = 10)
train.control3.rpart <- trainControl(method = "repeatedcv",number = 3,repeats =10 ,index =folds3)
cluster <- makeCluster(8,type="SOCK")
registerDoSNOW(cluster)
set.seed(34324)
train.3.rpart <- train(x=train.mod5,y=mod5.predictor,method="rpart",tuneLength=30,
                       trControl = train.control3.rpart) 
stopCluster(cluster)
train.3.rpart 

prp(train.3.rpart$finalModel,type=0,extra=1,under=TRUE)



#both rpart and random forest says title is the most important feature
table(data.combined$title)

#taking last names and first names out of Name
name_split <- str_split(data.combined$Name,",")
name_split[1]

#last name
last_name <- sapply(name_split,"[",1)
last_name[1:5]
data.combined$Lastname <- as.factor(last_name)

#first name
name_split <- str_split(sapply(name_split,"[",2)," ")
name_split[1]
title_split <- sapply(name_split,"[",2) 
unique(title_split)
table(title_split)


#updaing the titles to get more accuracy
title_split[title_split %in% c("Dona.","the")] <- "Lady."
title_split[title_split %in% c("Mlle.","Ms.")] <- "Miss."
title_split[title_split %in% c("Mme.")] <- "Mrs."
title_split[title_split %in% c("Don.","Jonkheer.")] <- "Sir."
title_split[title_split %in% c("Major.","Capt.","Col.")] <- "Officer"

#adding title_split to data.combined
data.combined$title.split <- as.factor(title_split)

#Plotting survival rate for new splitted titles
ggplot(data.combined[1:891,],aes(x=title.split,fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("Survival rate for Splitted Titles")


#since we do not have many instances of Lady,Sir,Dr,Office and Rev
#we will merge them into Mrs. and Mr. respectively for accuracy 
#Also this will reduce the chances of over fiting the model
title_split[title_split %in% c("Lady.")] <- "Mrs."
title_split[title_split %in% c("Sir.","Dr.","Officer","Rev.")] <- "Mr."
table(title_split)
data.combined$title.split <- as.factor(title_split)

#visualization of new splitted titles
ggplot(data.combined[1:891,],aes(x=title.split,fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("Survival rate for Splitted Titles")


# Now, fitting rpart model into the newly created title_split
features <- c("Pclass","title.split","FamilySize")
best.predictor2 <- data.combined[1:891,features]

#Running a 3- Fold CV with rpart on Pclass,title.split and FamilySize
folds4.rpart <- createMultiFolds(mod5.predictor,k = 3,times = 10)
train.control4.rpart <- trainControl(method = "repeatedcv",number = 3,repeats =10 ,index =folds4.rpart)
cluster <- makeCluster(8,type="SOCK")
registerDoSNOW(cluster)
set.seed(34324)
train.4.rpart <- train(x=best.predictor2,y=mod5.predictor,method="rpart",tuneLength=30,
                       trControl = train.control4.rpart) 
stopCluster(cluster)
train.4.rpart 

#Plotting the output
prp(train.4.rpart$finalModel,type=0,extra=1,under=TRUE)


