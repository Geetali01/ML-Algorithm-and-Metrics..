drug<-read.csv("C:/Users/user/Downloads/Imarticus related/IBM WATSON/DRUG1n.csv")
summary(drug) # No missing values
drug$Drug<-as.factor(drug$Drug)
#checking outliers - there seems to be no outliers
boxplot(drug$Age)
boxplot(drug$Na)
boxplot(drug$K)

quantile(drug$Age,c(0,0.1,0.2,0.3,0.4,0.5))

cor(drug$Age,drug$Na) #0.1003712
cor(drug$Age,drug$K) #0.1114912
cor(drug$K,drug$Na) #0.01733459

#divide the data into Train and test sets
require(caret)
set.seed(323)
train.rows<- createDataPartition(y= drug$Drug, p=0.7, list = FALSE)
train.drug<- drug[train.rows,]
test.drug<- drug[-train.rows,-7]
test.drug_label<-as.factor(drug[-train.rows,7])

require(randomForest)
model1<-randomForest(Drug~.,data = train.drug,ntree = 1000)
#model1<-randomForest(as.factor(Drug)~.,data = train.drug)
print(model1) # OOB error rate = 13.99%

preds<-predict(model1,test.drug)
confusionMatrix(preds,test.drug_label) #accuracy = 92.98
varImpPlot(model1)
#with cross validation 
#install.packages('doSNOW')
require(doSNOW)
set.seed(425)
#performing stratified repeated cross validation
#CreateMultifolds create 10 folds based on the drug variable for 5 times
#which means it creates 50 different partitions
cv.folds<-createMultiFolds(train.drug$Drug,k = 10,times=5)
help("createMultiFolds")
#setup the train parameters
#From the above 50 partitions, 10 partitions are selected and 9 of them are used for train and 1 for test and this process of cv is repeated for 10 times
cv.cntrl<-trainControl(method = "repeatedcv",number = 10,repeats = 10,index = cv.folds)

#setup dosnow package for ditributed processing
cl<-makeCluster(2,type = "SOCK")
registerDoSNOW(cl)

set.seed(25)
rpart.cv_1<-train(Drug~.,data=train.drug,method="rf",trControl = cv.cntrl,tuneGrid = expand.grid(nodesize = seq(1, 20, by = 1),ntree = seq(100, 1000, by = 100)))
stopCluster(cl)
View(rpart.cv_1)

#############
drug$Agegroup<-cut(drug$Age,5)
drug$Nagroup<-cut(drug$Na,5)
drug$Kgroup<-cut(drug$K,5)

set.seed(321)
train1.rows<- createDataPartition(y= drug$Drug, p=0.7, list = FALSE)
train1.drug<- drug[train1.rows,]
test1.drug<- drug[-train1.rows,-7]
test1.drug_label<-as.factor(drug[-train1.rows,7])

model2<-randomForest(Drug~Agegroup+Sex+BP+Cholesterol+Nagroup+Kgroup,data = train1.drug)
print(model2) #OOB error rate explains what percentage of records from OOB sample dataset that were wrongly classified by the forest.
preds1<-predict(model2,test1.drug)
confusionMatrix(preds1,test1.drug_label) #accuracy = 92.98
varImpPlot(model2)
model2$importance
#with cross validation 
set.seed(425)
cv1.folds<-createMultiFolds(train1.drug$Drug,k = 10,times=3)

#setup the train parameters
cv.cntrl1<-trainControl(method = "repeatedcv",number = 10,repeats = 3,index = cv.folds,search = "random")

#setup dosnow package for ditributed processing
cl<-makeCluster(2,type = "SOCK")
registerDoSNOW(cl)

set.seed(621)
rpart.cv_2<-train(Drug~.,data=train1.drug,method="rf",trControl = cv.cntrl1,tunelength =10)
#getModelInfo('rf')$rf$grid
stopCluster(cl)
rpart.cv_2 # why mtry = 19? After binning, each variable is one hot encoded
preds2<-predict(rpart.cv_2,test1.drug)
confusionMatrix(preds2,test1.drug_label)
