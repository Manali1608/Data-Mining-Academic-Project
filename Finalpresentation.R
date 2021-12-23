employee.df<-read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv", stringsAsFactors = TRUE)
View(employee.df)
summary(employee.df)
str(employee.df)

#data pre-processing

#Changing the first column to make it more readable
colnames(employee.df)[1] <- "Age"

#Checking for missing values
is.na(employee.df)
sum(is.na(employee.df))
mean(is.na(employee.df))

#provide descriptive name to variables
employee.df$Education  <- factor(employee.df$Education)
levels(employee.df$Education)
levels(employee.df$Education) <- c("Below College", "College", "Bachelor", "Master", "Doctor")

employee.df$EnvironmentSatisfaction  <- factor(employee.df$EnvironmentSatisfaction)
levels(employee.df$EnvironmentSatisfaction)
levels(employee.df$EnvironmentSatisfaction) <- c("Low", "Medium", "High", "Very High")

employee.df$JobInvolvement<- factor(employee.df$JobInvolvement)
levels(employee.df$JobInvolvement)
levels(employee.df$JobInvolvement) <- c("Low", "Medium", "High", "Very High")

employee.df$JobSatisfaction<- factor(employee.df$JobSatisfaction)
levels(employee.df$JobSatisfaction)
levels(employee.df$JobSatisfaction) <- c("Low", "Medium", "High", "Very High")

employee.df$PerformanceRating<- factor(employee.df$PerformanceRating)
levels(employee.df$PerformanceRating)
levels(employee.df$PerformanceRating) <- c("Excellent", "Outstanding")

employee.df$RelationshipSatisfaction<- factor(employee.df$RelationshipSatisfaction)
levels(employee.df$RelationshipSatisfaction)
levels(employee.df$RelationshipSatisfaction) <- c("Low", "Medium", "High", "Very High")

employee.df$WorkLifeBalance<- factor(employee.df$WorkLifeBalance)
levels(employee.df$WorkLifeBalance)
levels(employee.df$WorkLifeBalance) <- c("Bad", "Good", "Better", "Best")

#DATA VISUALIZATION
# Bar graph

library(ggplot2)
ggplot(employee.df,aes(BusinessTravel,fill=Attrition))+ geom_bar(position = "fill", alpha=1) + 
  labs(title = "Busines Travel vs Attrition" )

ggplot(employee.df,aes(Department,fill=Attrition))+ geom_bar(position = "fill", alpha=1) + 
  labs(title = "Department vs Attrition" )

ggplot(employee.df,aes(Education,fill=Attrition))+ geom_bar(position = "fill", alpha=1) + 
  labs(title = "Education vs Attrition" )

ggplot(employee.df,aes(EducationField,fill=Attrition))+ geom_bar(position = "fill", alpha=1) + 
  labs(title = "Education Field vs Attrition" )

ggplot(employee.df,aes(EnvironmentSatisfaction,fill=Attrition))+ geom_bar(position = "fill", alpha=1) + 
  labs(title = "Environment Satisfaction vs Attrition" )

ggplot(employee.df,aes(Gender,fill=Attrition))+ geom_bar(position = "fill", alpha=1) + 
  labs(title = "Gender vs Attrition" )

ggplot(employee.df,aes(JobInvolvement,fill=Attrition))+ geom_bar(position = "fill", alpha=1) + 
  labs(title = "JobInvolvement vs Attrition" )

ggplot(employee.df,aes(JobRole,fill=Attrition))+ geom_bar(position = "fill",width=0.5, alpha=1) + 
  labs(title = "JobRole vs Attrition" )

ggplot(employee.df,aes(JobSatisfaction,fill=Attrition))+ geom_bar(position = "fill", alpha=1) + 
  labs(title = "Job Satisfaction vs Attrition" )

ggplot(employee.df,aes(MaritalStatus,fill=Attrition))+ geom_bar(position = "fill", alpha=1) + 
  labs(title = "Marital Status vs Attrition" )
ggplot(employee.df,aes(OverTime,fill=Attrition))+ geom_bar(position = "fill", alpha=1) + 
  labs(title = "Overtime vs Attrition" )

ggplot(employee.df,aes(PerformanceRating,fill=Attrition))+ geom_bar(position = "fill", alpha=1) + 
  labs(title = "PerformanceRating vs Attrition" )

ggplot(employee.df,aes(RelationshipSatisfaction,fill=Attrition))+ geom_bar(position = "fill", alpha=1) + 
  labs(title = "RelationshipSatisfaction vs Attrition" )

ggplot(employee.df,aes(WorkLifeBalance,fill=Attrition))+ geom_bar(position = "fill", alpha=1) + 
  labs(title = "WorkLifeBalance vs Attrition" )
ggplot(employee.df,aes(JobLevel,fill=Attrition))+ geom_bar(position = "fill", alpha=1) + 
  labs(title = "Job Level vs Attrition" )
ggplot(employee.df,aes(NumCompaniesWorked,fill=Attrition))+ geom_bar(position = "fill", alpha=1) + 
  labs(title = "Number of Companies Worked vs Attrition" )
ggplot(employee.df,aes(StockOptionLevel,fill=Attrition))+ geom_bar(position = "fill", alpha=1) + 
  labs(title = "Stock Option Level vs Attrition" )
ggplot(employee.df,aes(TrainingTimesLastYear,fill=Attrition))+ geom_bar(position = "fill", alpha=1) + 
  labs(title = "Training Times Last Year vs Attrition" )
ggplot(employee.df,aes(YearsWithCurrManager,fill=Attrition))+ geom_bar(position = "fill", alpha=1) + 
  labs(title = "Years with Current Manager vs Attrition" )
ggplot(employee.df,aes(YearsInCurrentRole,fill=Attrition))+ geom_bar(position = "fill", alpha=1) + 
  labs(title = "Years in Current Role vs Attrition" )
ggplot(employee.df,aes(YearsSinceLastPromotion,fill=Attrition))+ geom_bar(position = "fill", alpha=1) + 
  labs(title = "Years Since Last Promotion vs Attrition" )

#Boxplot

ggplot(data = employee.df)+ geom_boxplot(aes(x = Attrition, y = Age))
ggplot(data = employee.df)+ geom_boxplot(aes(x = Attrition, y = DailyRate))
ggplot(data = employee.df)+ geom_boxplot(aes(x = Attrition, y = DistanceFromHome))
ggplot(data = employee.df)+ geom_boxplot(aes(x = Attrition, y = HourlyRate))
ggplot(data = employee.df)+ geom_boxplot(aes(x = Attrition, y = MonthlyIncome))
ggplot(data = employee.df)+ geom_boxplot(aes(x = Attrition, y = PercentSalaryHike))
ggplot(data = employee.df)+ geom_boxplot(aes(x = Attrition, y = TotalWorkingYears))
ggplot(data = employee.df)+ geom_boxplot(aes(x = Attrition, y = YearsAtCompany))
ggplot(data = employee.df)+ geom_boxplot(aes(x = Attrition, y = YearsInCurrentRole))
ggplot(data = employee.df)+ geom_boxplot(aes(x = Attrition, y = YearsSinceLastPromotion))
ggplot(data = employee.df)+ geom_boxplot(aes(x = Attrition, y = YearsWithCurrManager))

# Breakup of Attrition
ggplot(employee.df, aes(factor(Attrition), fill = factor(Attrition))) + 
  geom_bar() + 
  stat_count(geom = "text", aes(label = ..count..), position=position_stack(vjust=0.5))

#Re-code the target variable to indicate which target value is associated with Class 1
employee.df$Attrition <- as.numeric(employee.df$Attrition == "Yes")
employee.df$Attrition

#Check the number of o and 1 values
length(which(employee.df$Attrition == 1))


#classification Tree
set.seed(2500)
train.index <- sample(1:nrow(employee.df), nrow(employee.df)*0.7) 
selected.var<-c(1,2,3,4,5,6,7,8,11,14,15,16,17,18,19,21,23,24,26,28,29,30,31,32,33,35)

selected.df<-employee.df[selected.var]
train.df <- selected.df[train.index, ]
valid.df <- selected.df[-train.index, ]
library(rpart)
library(rpart.plot)
default.ct <- rpart(Attrition ~ ., data = train.df, method = "class")
rpart.plot(default.ct, type=1, extra = 1, cex=0.6, varlen=0)
default.ct$variable.importance
default.ct.point.pred <- predict(default.ct, valid.df, type = "class")

library(caret)
library(ggplot2)
#install.packages("lattice")
library(lattice)
confusionMatrix(default.ct.point.pred,factor(valid.df$Attrition),positive='1')
# positive class 1 means sensitivity defines ability to predict variables belonging to class 1

#HANDLING THE CLASS IMBALANCE PROBLEM
#1.OVER SAMPLING Classification Tree

set.seed(2500)
table(train.df$Attrition)
prop.table(table(train.df$Attrition))
#install.packages("ROSE")
library(ROSE)
over<-ovun.sample(Attrition ~ ., data = train.df, method = "over",N=1728)$data
table(over$Attrition)
#train the model
over.ct <- rpart(Attrition ~ ., data = over, method = "class")
rpart.plot(over.ct, type=1, extra = 1, cex=0.6, varlen=0)
over.pred <- predict(over.ct, valid.df, type = "class")
confusionMatrix(over.pred,factor(valid.df$Attrition),positive='1')

#2.Under sampling Classification Tree
set.seed(2500)
under<-ovun.sample(Attrition ~ ., data = train.df, method = "under",N=330)$data
table(under$Attrition)
#train the model
under.ct<-rpart(Attrition ~ ., data = under, method = "class")
rpart.plot(under.ct, type=1, extra = 1, cex=0.6, varlen=0)
under.pred <- predict(under.ct, valid.df, type = "class")
confusionMatrix(under.pred,factor(valid.df$Attrition),positive='1')


#random forest
employee.df$Attrition=as.factor(employee.df$Attrition)
#install.packages("randomForest")
library(randomForest)
set.seed(2500)
train.random.index <- sample(1:nrow(employee.df), nrow(employee.df)*0.7) 
selected.random.var<-c(1,2,3,4,5,6,7,8,11,14,15,16,17,18,19,21,23,24,26,28,29,30,31,32,33,35)
selected.random.df<-employee.df[selected.random.var]
train.random.df <- selected.random.df[train.random.index, ]
valid.random.df <- selected.random.df[-train.random.index, ]
summary(train.random.df)
summary(valid.random.df)
model1 <- randomForest(Attrition ~ ., data = train.random.df)
model1
predict.valid<-predict(model1, valid.random.df, type = "class")
confusionMatrix(predict.valid,factor(valid.random.df$Attrition),positive='1')
importance(model1)        

varImpPlot(model1)

#Over Sampling Random Forest
set.seed(2500)
table(train.random.df$Attrition)
over<-ovun.sample(Attrition ~ ., data = train.random.df, method = "over",N=1728)$data
table(over$Attrition)
model.over<- randomForest(Attrition ~ ., data =over)
predict.valid<-predict(model.over, valid.random.df, type = "class")
confusionMatrix(predict.valid,factor(valid.random.df$Attrition),positive='1')

# Under Sampling Random Forest
set.seed(2500)
table(train.random.df$Attrition)
under<-ovun.sample(Attrition ~ ., data = train.random.df, method = "under",N=330)$data
table(under$Attrition)
model.under<- randomForest(Attrition ~ ., data =under)
predict.valid<-predict(model.under, valid.random.df, type = "class")
confusionMatrix(predict.valid,factor(valid.random.df$Attrition),positive='1')

