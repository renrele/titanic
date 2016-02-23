library(caret)
library(rpart)
library(rpart.plot)
library(rattle)

#load data
train_url<-"https://raw.github.com/renrele/titanic/master/train.csv"
test_url<- "https://raw.github.com/renrele/titanic/master/test.csv"
training<-read.csv(train_url)
test<-read.csv(test_url)
test<- data.frame(PassengerId=test[,1],Survived=rep("NA",418),test[,2:11])

#replace blanks by NA
training[training==""]<-NA
test[test==""]<-NA

#combine training and test data sets for preprocessing
alldata <- rbind(training,test)

#transform numeric to factor
alldata$Pclass<-as.factor(alldata$Pclass)


#clean up ticket feature
tck<-sapply(as.character(alldata$Ticket),function(s) strsplit(s," "))
tick<-as.numeric(sapply(tck,function(s){
        if(length(s)==1) s 
        else if (length(s)==2) s[2]
        else s[3]
}))
alldata$Ticket <-tick

###it appears that fares for multiple tickets are a total fare for the group
###need to divide fare by number of times ticket number appears
tc<-table(alldata$Ticket)
counts<- data.frame(TickNum=names(tc),counts<-as.numeric(tc))
for (i in 1:nrow(alldata)){ 
        if (!is.na(alldata$Fare[i])&!is.na(alldata$Ticket[i]))
                alldata$FarePP[i]<-alldata$Fare[i]/counts[which(counts[,1]==alldata$Ticket[i]),2]
}

#look at ticket for passenger with missing fare and impute fare
alldata[which(alldata$Ticket==3701),]
fares<-alldata$FarePP[alldata$Pclass==3 & alldata$Embarked =="S"]
alldata$Fare[1044]<-median(fares,na.rm=T)
alldata$FarePP[1044]<-median(fares,na.rm=T)

#extract deck from cabin
alldata$Deck<- as.factor(sapply(alldata$Cabin,function(s) substr(s,1,1)))
#look at position of cabin on deck
alldata$CabNum<-sapply(alldata$Cabin,function(s) substr(s,1,2))

#look at passnegers with missing embarked
alldata$Embarked[c(62,830)]<-"S"

###first make Name into character vector instead of factor
alldata$Name<-as.character(alldata$Name)
#extract surnames
namesplit<-sapply(alldata$Name,function(x)unlist(strsplit(x,", ")))
alldata$Surname<-as.factor(namesplit[1,])

#extract titles
alldata$Title<-sapply(namesplit[2,],function(n)unlist(strsplit(n,".",fixed=TRUE))[1])
###combine "Capt","Col","Jonkheer","Major","Rev","Sir","Dr" into SpecialMr
t<-c("Capt","Col","Jonkheer","Major","Rev","Sir","Dr")
for (i in 1:7) alldata$Title<-gsub(t[i],"SpecialMr",alldata$Title)
###combine "Lady", "the Countess" into "SpecialMrs"
t<- c("Lady", "the Countess")
for (i in 1:2) alldata$Title<-gsub(t[i],"SpecialMrs",alldata$Title)
###replace "Don" by "Mr", "Mlle" by "Miss", "Mme" by "Mrs"
alldata$Title<-gsub("Dona","Mrs",alldata$Title)
alldata$Title<-gsub("Don","Mr",alldata$Title)
alldata$Title<-gsub("Mme","Mrs",alldata$Title)
alldata$Title<-gsub("Mlle","Miss",alldata$Title)
###make factor
alldata$Title <- as.factor(alldata$Title)


#Use gam to predict missing age values, based on Title, Sex, SibSp, Parch
fit <- train(Age~SibSp+Parch+Title+Sex,data=alldata[!is.na(alldata$Age),],method="gam")
fit$finalModel

newAge <- predict(fit,alldata[is.na(alldata$Age),])
alldata[is.na(alldata$Age),6]<-newAge
#make age a factor variable young (<-16)or not young  
alldata$Age[alldata$Age<=16]<-1
alldata$Age[alldata$Age >16 ]<-0
alldata$Age <-as.factor(alldata$Age)

#use rpart to predict missing deck values
set.seed(192)
cabfit <- train(Deck~Pclass+Fare+Surname+Ticket+FarePP+Embarked,data=alldata[!is.na(alldata$Deck),],method="rpart")
fancyRpartPlot(cabfit$finalModel)
alldata$Deck[is.na(alldata$Deck)]<-predict(cabfit,newdata=alldata[is.na(alldata$Deck),],type="raw")


###FamilySize is number of parents/children + number of spouse/siblings + oneself
alldata$FamilySize <- 1+alldata$Parch + alldata$SibSp

#train full model on training set
set.seed(375)
newtrain<- alldata[1:891,c(2,3,5,6,7,8,9,10,12,13,14,16,17,18)]
newtest<- alldata[892:1309,c(2,3,5,6,7,8,9,10,12,13,14,16,17,18)]
fit1 <- train(Survived~.,data=newtrain,method="rf")

#predict survival on test set
Survived<-predict(fit1,newdata=newtest)
PassengerId <- test$PassengerId
solution <- data.frame(PassengerId,Survived)