
install.packages("corrplot")
library(corrplot)

# Let’s build our R code here 
attribution = read.csv("WA_Fn-UseC_-HR-Employee-Attrition.xls")

# Overview: 1470 obs. of  35 variables
str(attribution)

# change type chr to factor
attribution$Attrition = as.factor(attribution$Attrition)
attribution$BusinessTravel = as.factor(attribution$BusinessTravel)
attribution$Department = as.factor(attribution$Department)
attribution$EducationField = as.factor(attribution$EducationField)
attribution$Gender = as.factor(attribution$Gender)
attribution$JobRole = as.factor(attribution$JobRole)
attribution$MaritalStatus = as.factor(attribution$MaritalStatus)
attribution$OverTime = as.factor(attribution$OverTime)

# check na
any(is.na(attribution))

#remove Over18, EmployeeCount, EmployeeNumber, standardhour
attribution = attribution[ , c(-9,-10,-22, -27)]

# check correlation among numerical variables
# As a result:
# JobLevel & MonthlyIncome: 0.95
# JobLevel & WorkingYears: 0.78
# PercentSalaryHike & Performance Rating: 0.77
# MonthlyIncome & Working Years: 0.77
# YearsatCompany & YearsCurrManager: 0.77
# YearsatCompany & YearInCurrentRole: 0.76
# YearInCurrentRole & YearsCurrManager: 0.71
# Age & WorkingYears: 0.68
# WorkingYears & YearsatCompany: 0.63
# YearsatCompany & YearssinceLastPromotion: 0.62

library(corrplot)
numerical = unlist(lapply(attribution, is.numeric))
M = cor(attribution[, numerical])
corrplot.mixed(M, tl.cex=0.6)

## visualization: Education, Monthly Rate, Performance Rating, StockOptionLevel, TrainingTimeLastYear, YearsinCurrent Role, YearsSinceLastPromotion are not significant.  
library(ggplot2)
# attrition under age: younger people tend to leave
ggplot(attribution, aes(x=ï..Age, fill=Attrition)) + geom_density(alpha=0.3)

# Those left travel more frequently
g = ggplot(attribution, aes(x = BusinessTravel, group = Attrition)) + geom_bar(aes(y = ..prop.., fill = BusinessTravel), stat="count", fill="#FFA500", alpha=0.3)
g +facet_grid(.~Attrition) + ggtitle("Attrition")+theme_bw()+geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), stat= "count", vjust = -.5)

# attrition under dailyRate: people with lower dailyrate tend to leave
ggplot(attribution, aes(x=DailyRate, fill=Attrition)) + geom_density(alpha=0.3)

# People in Sales tend to leave
g = ggplot(attribution, aes(x =Department , group = Attrition))+geom_bar(aes(y = ..prop.., fill = BusinessTravel), stat="count", fill = "#FFA500", alpha=0.3)
g +facet_grid(.~Attrition) + ggtitle("Attrition")+theme_bw() + geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), stat= "count", vjust = -.5)

# People live far way tend to leave
ggplot(attribution, aes(x=DistanceFromHome, fill=Attrition)) + geom_density(alpha=0.3)


# Education seems to be not important
g = ggplot(attribution, aes(x = Education, group = Attrition)) + geom_bar(aes(y = ..prop.., fill = Education), stat="count", fill="#FFA500", alpha=0.3)
g + facet_grid(.~Attrition) + ggtitle("Attrition")+theme_bw()+geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), stat= "count", vjust = -.5)

# Education field is also hard to tell
g = ggplot(attribution, aes(x = EducationField, group = Attrition)) + geom_bar(aes(y = ..prop.., fill = Education), stat="count", fill="#FFA500", alpha=0.3)
g + facet_grid(.~Attrition) + ggtitle("Attrition")+theme_bw()+geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), stat= "count", vjust = -.5)

# Environment satisfaction is important
g = ggplot(attribution, aes(x = EnvironmentSatisfaction, group = Attrition)) + geom_bar(aes(y = ..prop.., fill = Education), stat="count", fill="#FFA500", alpha=0.3)
g + facet_grid(.~Attrition) + ggtitle("Attrition")+theme_bw()+geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), stat= "count", vjust = -.5)

# Male tend to leave
g = ggplot(attribution, aes(x = Gender, group = Attrition)) + geom_bar(aes(y = ..prop.., fill = Education), stat="count", fill="#FFA500", alpha=0.3)
g + facet_grid(.~Attrition) + ggtitle("Attrition")+theme_bw()+geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), stat= "count", vjust = -.5)

# Hourly Rate: lower rate made people leave
ggplot(attribution, aes(x=HourlyRate, fill=Attrition)) + geom_density(alpha=0.3)


# Job involvement matters
g = ggplot(attribution, aes(x = JobInvolvement, group = Attrition)) + geom_bar(aes(y = ..prop.., fill = Education), stat="count", fill="#FFA500", alpha=0.3)
g + facet_grid(.~Attrition) + ggtitle("Attrition")+theme_bw()+geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), stat= "count", vjust = -.5)


# Job Level matters
g = ggplot(attribution, aes(x = JobLevel, group = Attrition)) + geom_bar(aes(y = ..prop.., fill = Education), stat="count", fill="#FFA500", alpha=0.3)
g + facet_grid(.~Attrition) + ggtitle("Attrition")+theme_bw()+geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), stat= "count", vjust = -.5)


# Job role matters
g = ggplot(attribution, aes(x = JobRole, group = Attrition)) + geom_bar(aes(y = ..prop.., fill = Education), stat="count", fill="#FFA500", alpha=0.3)
g + facet_grid(.~Attrition) + ggtitle("Attrition")+theme_bw()+geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), stat= "count", vjust = -.5)

# Job satisfaction matters
g = ggplot(attribution, aes(x = RelationshipSatisfaction, group = Attrition)) + geom_bar(aes(y = ..prop.., fill = Education), stat="count", fill="#FFA500", alpha=0.3)
g + facet_grid(.~Attrition) + ggtitle("Attrition")+theme_bw()+geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), stat= "count", vjust = -.5)

# Single people tend to leave
g = ggplot(attribution, aes(x = MaritalStatus, group = Attrition)) + geom_bar(aes(y = ..prop.., fill = Education), stat="count", fill="#FFA500", alpha=0.3)
g + facet_grid(.~Attrition) + ggtitle("Attrition")+theme_bw()+geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), stat= "count", vjust = -.5)

# Monthly income Yes
ggplot(attribution, aes(x=MonthlyIncome, fill=Attrition)) + geom_density(alpha=0.3)

# Monthly Rate : not clear
ggplot(attribution, aes(x=MonthlyRate, fill=Attrition)) + geom_density(alpha=0.3)

# Num of company worked: Yes
ggplot(attribution, aes(x=NumCompaniesWorked, fill=Attrition)) + geom_density(alpha=0.3)


# OverTime: Yes
g = ggplot(attribution, aes(x = OverTime, group = Attrition)) + geom_bar(aes(y = ..prop.., fill = Education), stat="count", fill="#FFA500", alpha=0.3)
g + facet_grid(.~Attrition) + ggtitle("Attrition")+theme_bw()+geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), stat= "count", vjust = -.5)


# Percent Salary Hike: yes
ggplot(attribution, aes(x=PercentSalaryHike, fill=Attrition)) + geom_density(alpha=0.3)
# Performance Rating： no?
ggplot(attribution, aes(x=PerformanceRating, fill=Attrition)) + geom_density(alpha=0.3)
# StockOptionLevel: not sure
ggplot(attribution, aes(x=StockOptionLevel, fill=Attrition)) + geom_density(alpha=0.3)
# TotalWorkingYears: yeah?
ggplot(attribution, aes(x=TotalWorkingYears, fill=Attrition)) + geom_density(alpha=0.3)
# TrainingTimesLastYear: no
ggplot(attribution, aes(x=TrainingTimesLastYear, fill=Attrition)) + geom_density(alpha=0.3)



# Worklifebalance
g = ggplot(attribution, aes(x = WorkLifeBalance, group = Attrition)) + geom_bar(aes(y = ..prop.., fill = Education), stat="count", fill="#FFA500", alpha=0.3)
g + facet_grid(.~Attrition) + ggtitle("Attrition")+theme_bw()+geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), stat= "count", vjust = -.5)

# Years at Company matters
ggplot(attribution, aes(x=YearsAtCompany, fill=Attrition)) + geom_density(alpha=0.3)

# YearsInCurrentRole: no
ggplot(attribution, aes(x=YearsInCurrentRole, fill=Attrition)) + geom_density(alpha=0.3)
# YearSinceLastPromotion: no
ggplot(attribution, aes(x=YearsSinceLastPromotion, fill=Attrition)) + geom_density(alpha=0.3)
# YearswithCurrent manager matters
ggplot(attribution, aes(x=YearsWithCurrManager, fill=Attrition)) + geom_density(alpha=0.3)



# We can consider logistic regression, LDA, QDA, Naive Bayes, knn, classification trees.
----------------------------------------------------------------------------------------------------------------------
  
  There are warning message with this one and I don’t know how to fix it
# first, logistic regression
calc_acc = function(actual, predicted) {
  mean(actual == predicted)
}
library(caret)
index = createDataPartition(attribution$Attrition, p=0.8, list=FALSE)
train_data = attribution[index, ]
test_data = attribution[-index, ]
cv_5 = trainControl(method = "cv", number = 5)
glm_1 = train(form=Attrition~., data = train_data, method = "glm", trControl = cv_5)
calc_acc(actual = test_data$Attrition,
         predicted = predict(glm_1, newdata = test_data))












========================Checking the Accuracy Problem=======================
  #I tried so, and when including all factors, the accuracy did change. However, the accuracy remains the same when model contains only 1 factor, no matter which factor it is. 
  #the coefficients and fitted values are different 
  
  #model by Edu
  calc_acc = function(actual, predicted) {
    mean(actual == predicted)
  }
library(caret)
index = createDataPartition(attribution$Attrition, p=0.8, list=FALSE)
train_data = attribution[index, ]
test_data = attribution[-index, ]
cv_5 = trainControl(method = "cv", number = 5)
glm_1 = train(form=Attrition~Education, data = train_data, method = "glm", trControl = cv_5)
calc_acc(actual = test_data$Attrition,
         predicted = predict(glm_1, newdata = test_data))

#[1] 0.8395904

#check coefficients 
glm_1$finalModel[1]
$coefficients
(Intercept)   Education 
-1.42683283 -0.07661314 

#store fitted value for later check
edufitted<- unname(unlist(glm_1$finalModel[3]))


#model by Age
calc_acc = function(actual, predicted) {
  mean(actual == predicted)
}
library(caret)
index = createDataPartition(attribution$Attrition, p=0.8, list=FALSE)
train_data = attribution[index, ]
test_data = attribution[-index, ]
cv_5 = trainControl(method = "cv", number = 5)
glm_1 = train(form=Attrition~Age, data = train_data, method = "glm", trControl = cv_5)
calc_acc(actual = test_data$Attrition,
         predicted = predict(glm_1, newdata = test_data))

#[1] 0.8395904
#exactly the same acc

#check coefficients 
glm_1$finalModel[1]
$coefficients
(Intercept)         Age 
0.51750547 -0.06155373 
#note that the coefficients are different from the model by Education

#compare the fitted value here
agefitted<- unname(unlist(glm_1$finalModel[3]))

all.equal(agefitted,edufitted)
[1] "Mean relative difference: 0.3569537"

count = 0
for(i in 1:length(agefitted)){
  if(agefitted[i] == edufitted[i]){
    count <<- count + 1
  } 
}
count
#0

#this suggests that fitted values of the two models are perfectly different
#So I turned to check the comparing process, letting the calc_acc print out

calc_acc = function(actual, predicted) {
  cat("now compareing: ", actual, predicted, "\n")
  mean(actual == predicted)
}

index = createDataPartition(attribution$Attrition, p=0.8, list=FALSE)
train_data = attribution[index, ]
test_data = attribution[-index, ]
cv_5 = trainControl(method = "cv", number = 5)
glm_1 = train(form=Attrition~Age, data = train_data, method = "glm", trControl = cv_5)
calc_acc(actual = test_data$Attrition,
         predicted = predict(glm_1, newdata = test_data))

index = createDataPartition(attribution$Attrition, p=0.8, list=FALSE)
train_data = attribution[index, ]
test_data = attribution[-index, ]
cv_5 = trainControl(method = "cv", number = 5)
glm_1 = train(form=Attrition~Education, data = train_data, method = "glm", trControl = cv_5)
calc_acc(actual = test_data$Attrition,
         predicted = predict(glm_1, newdata = test_data))

======================Checking the Accuracy Problem Ends=====================
  
