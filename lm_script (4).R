data_lm<-read.csv("C:/Users/user/Downloads/Imarticus related/R programming/Class/1 Linear_Regression/Dataset/insurance.csv")
View(data_lm)

summary(data_lm)

data_lm$sex<-as.factor(data_lm$sex)
data_lm$smoker<-as.factor(data_lm$smoker)
data_lm$region<-as.factor(data_lm$region)
data_lm$children<-as.factor(data_lm$children)
unique(is.na(data_lm)) # No missing values across the dataset

########univariate analysis on age######################
boxplot(data_lm$age) # No outliers
quantile(data_lm$age) #min and max ages look good
#binning age
data_lm$age_bin<-cut(data_lm$age,4,labels = c("A","B","C","D")) # bin the age
as.data.frame(aggregate(x = data_lm$age,by=list(data_lm$age_bin),FUN = min))
as.data.frame(aggregate(x = data_lm$age,by=list(data_lm$age_bin),FUN = max))
#above 2 lines of code show that age bins are as follows:
#18-29, 30-41, 42-52, 53-64

########univariate analysis on bmi######################
boxplot(data_lm$bmi)
quantile(data_lm$bmi,seq(from=0.1,to=1, by=0.1)) # outliers look to be present in 90% to 100% quantile
quantile(data_lm$bmi,seq(from=0.9,to=1, by=0.01)) # outliers look to be present in 98% to 100% quantile
outliers_bmi<-boxplot.stats(data_lm$bmi)$out # this gives the values that lie in 99% to 100% quantile
((length(outliers_bmi))/(nrow(data_lm)))*100
data_lm[which(data_lm$bmi>=47.41),] # all outliers are from south east region
x_df<-data_lm[which(data_lm$bmi<=47.41),]
x_summ<-as.data.frame(aggregate(x = x_df$bmi,by=list(x_df$region,x_df$smoker),FUN = mean))
names(x_summ)<-c("region","smoker","mean")
data_lm$bmi[which(data_lm$bmi>=47.41 & data_lm$smoker == 'no' & data_lm$region == 'southeast')]<- x_summ$mean[(which(x_summ$region == 'southeast' & x_summ$smoker == 'no'))]
data_lm$bmi[which(data_lm$bmi>=47.41 & data_lm$smoker == 'yes' & data_lm$region == 'southeast')]<-x_summ$mean[(which(x_summ$region == 'southeast' & x_summ$smoker == 'yes'))]
data_lm$bmi[which(data_lm$bmi>=47.41 & data_lm$smoker == 'yes' & data_lm$region == 'southwest')]<-x_summ$mean[(which(x_summ$region == 'southwest' & x_summ$smoker == 'yes'))]
boxplot(data_lm$bmi)# looks good
#binning bmi
data_lm$bmi_bin<-cut(data_lm$bmi,4,labels = c("bmi1","bmi2","bmi3","bmi4"))


#############univariate analysis on region#############
table(data_lm$region) #most people are from southeast
prop.table(table(data_lm$region))
prop.table(table(data_lm$region,data_lm$age_bin))
plot(data_lm$region)
require(ggplot2)
ggplot(data=data_lm,mapping = aes(x=region,color=smoker))+geom_bar()

###########univariate analysis on sex##################
table(data_lm$sex)
prop.table(table(data_lm$sex)) #almost male population = female population

############univariate analysis on smoker##############
table(data_lm$smoker) #most of the population are non-smokers

#############bivariate analysis on age and bmi#########
cor(data_lm$bmi,data_lm$age) # mild correation, can be ignored
#average bmi by age looks to be same across the age group
as.data.frame(aggregate(x = data_lm$bmi,by=list(data_lm$age_bin),FUN = mean))

############bivariate analysis on age and num of children#####
table(data_lm$age_bin,data_lm$children) # looks odd, people of Age bin A having 5 children
#It looks strange that people having 5 children have less avg age
as.data.frame(aggregate(x = data_lm$age,by=list(data_lm$children),FUN = mean))

############bivariate analysis on age and sex##########
#average age of males and females look almost same 
# - this looks like they are not correlated
as.data.frame(aggregate(x = data_lm$age,by=list(data_lm$sex),FUN = mean))

############bivariate analysis on age and smoker##########
#average age of males and females look almost same
# - this shows that age and smoke variables look to be independent**
as.data.frame(aggregate(x = data_lm$age,by=list(data_lm$smoker),FUN = mean))

############bivariate analysis on age and region##########
#average age of southeast region looks to be slightly less
as.data.frame(aggregate(x = data_lm$age,by=list(data_lm$region),FUN = mean))
#Below code tells that people of age window 18-29 are more in southeast region
prop.table(table(data_lm$region,data_lm$age_bin)) 

########bivariate analysis of age and charges##########
cor(data_lm$age,data_lm$charges) #0.299
plot(data_lm$age,data_lm$charges) # as the age increase, insurance cost looks to be increasing
#But there is some partition in the plots which tells that there is/are some
#other variable(s) that is causing the raise in charges - lets investigate

########bivariate analysis of bmi and sex##############
#almost both males and females have same bmi
as.data.frame(aggregate(x = data_lm$bmi,by=list(data_lm$sex),FUN = mean))

########bivariate analysis of bmi and children#########
#people with more 5 children have less bmi - strange/wierd
as.data.frame(aggregate(x = data_lm$bmi,by=list(data_lm$children),FUN = mean))

########bivariate analysis of bmi and smoker###########
#bmi is same for both smokers and non-smokers
as.data.frame(aggregate(x = data_lm$bmi,by=list(data_lm$smoker),FUN = mean))

########bivariate analysis of bmi and region############
#bmi of southeast region people look to be high
as.data.frame(aggregate(x = data_lm$bmi,by=list(data_lm$region),FUN = mean))

########bivariate analysis of bmi and charges############
cor(data_lm$bmi,data_lm$charges) #0.192
plot(data_lm$bmi,data_lm$charges) # no particular pattern

########bivariate analysis of children and smokers########
table(data_lm$children,data_lm$smoker)
chisq.test(data_lm$children,data_lm$smoker) # p-val>0.05; both are independent
#people with no children looks to smoke more - 85%
prop.table(table(data_lm$children,data_lm$smoker))

########bivariate analysis of children and region########
prop.table(table(data_lm$children,data_lm$region))*100
chisq.test(data_lm$children,data_lm$region) #p>0.05 - accept null hypothesis - both are independent
require(ggplot2)
ggplot(data_lm, aes(region, ..count..)) + geom_bar(aes(fill = children), position = "dodge")
#most people with 5 children are from southwest

########bivariate analysis of children and charges########
#charges for people with 5 children is less
as.data.frame(aggregate(x = data_lm$charges,by=list(data_lm$children),FUN = mean))

#######bivariate analysis of smoker and region############
y<-as.data.frame(table(data_lm$smoker,data_lm$region))
chisq.test(data_lm$smoker,data_lm$region) #p-val>0.05 -accept null hypothesis - both variable are independent

#########splitting the dataset############################
set.seed(1)
samp<-sample(c(TRUE, FALSE),nrow(data_lm),replace=TRUE,prob = c(0.7,0.3))
train_lm<-data_lm[samp,]
test_lm<-data_lm[!samp,-7]
test_lm_target<-data_lm[!samp,7]
#test_lm
######Model building############
lm1<-lm(formula = charges~age+sex+bmi+children+smoker+region,data = train_lm)
#F-test P-val shows that model is significant.
#region and children look to be not significant
summary(lm1)#R-squared:  0.752,	Adjusted R-squared:  0.7488

lm2<-lm(formula = charges~age_bin+sex+bmi+children+smoker+region,data = train_lm)
# looks like age_bin doesn't help us much - Accuracy reduced
summary(lm2)#R-Square = 0.752, Adj R-Square = 0.7488
train_lm$children<-as.numeric(train_lm$children)

lm_test<-lm(formula = charges~age+bmi+children+smoker+region,data = train_lm)
summary(lm_test)

lm3<-lm(formula = charges~age+bmi+smoker+children,data = train_lm)
summary(lm3)# R-squared:  0.74,	Adjusted R-squared:  0.7474

lm4<-lm(formula = charges~age+bmi+smoker,data = train_lm)
summary(lm4) # R-squared:  0.7482,	Adjusted R-squared:  0.7474 - Not much improvement

lm5<- lm(formula = charges~age+bmi_bin+sex+smoker,data = train_lm)
summary(lm5) # Not much improvement

#Check for multi-collinearity
require(car)
#install.packages("regclass")
require(regclass)
VIF(lm4)
vif(lm5) #none of the variables have vif = 5 - good sign
######K-fold cross validation########################
set.seed(2)
require(caret)
#specify the cross-validation method
ctrl <- trainControl(method = "cv", number = 10)

#fit a regression model and use k-fold CV to evaluate performance
lm6 <- train(charges~age+sex+bmi+smoker, data = train_lm, method = "lm", trControl = ctrl)
print(lm6) # R-squared:  0.745

require(car)
#install.packages(regclass)
require("regclass")
vif(lm6$finalModel) #none of the variables have vif = 5 - good sign

#####Testing the model######################
op_test<-predict(lm6,test_lm)
op_train<-predict(lm6,train_lm)

#######calculating model metrics############
#install.packages("Metrics")
require(Metrics)
rmse(train_lm$charges,op_train)#6039.086
rmse(test_lm_target,op_test)#6249.773

######Residual analysis####################
train_lm$Residuals<-rstudent(lm1)
par(mfrow=c(3,2))
hist(rstudent(lm1)) #Checking if residuals follow normal distribution
plot(lm1,which = 2) #Checking the normality assumption using qqplot
plot(lm1,which =1)
#How to infer residuals vs fit plot?
#The residuals "bounce randomly" around the 0 line. This suggests that the assumption that the relationship is linear is reasonable.
#The residuals roughly form a "horizontal band" around the 0 line. This suggests that the variances of the error terms are equal.
#No one residual "stands out" from the basic random pattern of residuals. This suggests that there are no outliers.
plot(train_lm$Residuals~train_lm$age)
plot(train_lm$Residuals~train_lm$bmi)
plot(train_lm$Residuals~train_lm$smoker)
plot(train_lm$Residuals~train_lm$children)
qqnorm(train_lm$age)
hatvalues(lm1)
cooks.distance(lm1)
par(mfrow=c(3,1))
plot(lm1,which = 4) #cooks distance plot
plot(lm1,which = 5) #residuals vs leverage plot
plot(lm1,which = 6) # Leverage vs cooks distance
nrow(unique(data_lm))


as.data.frame(aggregate(x = data_lm$charges,by=list(data_lm$region),FUN = mean))
as.data.frame(aggregate(x = data_lm$charges,by=list(data_lm$region),FUN = median))

table(data_lm$sex,data_lm$smoker) # Smokers are less and there are more number of male smokers

