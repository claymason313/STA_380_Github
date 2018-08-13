cat(paste("10. This exercise involves the Boston housing data set.",
"(a) To begin, load in the Boston data set. The Boston data set is",
"part of the MASS library in R. > library(MASS)",
"Now the data set is contained in the object Boston. > Boston",
"Read about the data set:",
"  > ?Boston",
"How many rows are in this data set? How many columns? What do the rows and columns represent?",
"(b) Make some pairwise scatterplots of the predictors (columns) in this data set. Describe your findings.",
"(c) Are any of the predictors associated with per capita crime rate? If so, explain the relationship.",
"(d) Do any of the suburbs of Boston appear to have particularly high crime rates? Tax rates? Pupil-teacher ratios? Comment on the range of each predictor.",
"(e) How many of the suburbs in this data set bound the Charles river?",
"(f) What is the median pupil-teacher ratio among the towns in this data set?",
"(g) Which suburb of Boston has lowest median value of owner- occupied homes? What are the values of the other predictors for that suburb, and how do those values compare to the overall ranges for those predictors? Comment on your findings.",
"(h) In this data set, how many of the suburbs average more than seven rooms per dwelling? More than eight rooms per dwelling? Comment on the suburbs that average more than eight rooms per dwelling.",sep="\n"))


#"(a)"How many rows are in this data set? How many columns? What do the rows and columns represent?",
rm(list=ls())
library(MASS)

#https://stat.ethz.ch/R-manual/R-devel/library/base/html/nrow.html
#https://stackoverflow.com/questions/24072315/add-line-break-in-print-statement-in-r


# 506 rows, 14 columns
# rows = individual observations of suburbs' housing data and each individual variable/column description is below

cat(paste("Chapter 2: #10(a)", "How many rows are in this data set? ", 
          nrow(Boston), "How many columns?", 
          ncol(Boston), "What do the rows and columns represent?",
          "rows = individual observations of suburbs' housing data and each individual variable/column description is below",
          "",
          "1 crim - per capita crime rate by town.",
          "2 zn - proportion of residential land zoned for lots over 25,000 sq.ft.",
          "3 indus -proportion of non-retail business acres per town.",
          "4 chas - Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).",
          "5 nox - nitrogen oxides concentration (parts per 10 million).",
          "6 rm - average number of rooms per dwelling.",
          "7 age - proportion of owner-occupied units built prior to 1940.",
          "8 dis - weighted mean of distances to five Boston employment centres.",
          "9 rad - index of accessibility to radial highways.",
          "10 tax - full-value property-tax rate per $10,000.",
          "11 ptratio - pupil-teacher ratio by town.",
          "12 black - 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town.",
          "13 lstat - lower status of the population (percent).",
          "14 medv - median value of owner-occupied homes in $1000s.",sep="\n"))


?Boston




cat(paste("Chapter 2: #10(b)", "Make some pairwise scatterplots of the predictors (columns) in this data set. Describe your findings.",sep="\n"))


#All pairs scatter plot
library(lattice)
library(ggplot2)
#install.packages("GGally")
require(GGally)
data(Boston, package="reshape")
ggpairs(data=Boston) # aesthetics, ggplot2 style

attach(Boston)


#included wrong/target columns on these
# plot(lstat,medv) 
# cat(paste("formula: plot(lstat,medv)", "median value is negatively coorelated with % of population in a lower social class. The relationships is non-linear",sep="\n"))
# 
# plot(crim,medv) 
# cat(paste("formula: plot(crim,medv)", "crime rates are too low in many suburbs to visually make an observation about median value. If the crime rate is greater than about 20 (crime rate per capita) then the median home value is less than 20.",sep="\n")) 
# 
# plot(rm,medv) 
# cat(paste("formula: plot(rm,medv)", "positive correlation between average number of rooms and median values. More rooms leads to higher median value.",sep="\n"))

#limit to numeric data
my_data = Boston[, c(1,2,3,5,6,7,8,9,11,12,13)]
# print the first 5 rows
head(my_data, 5)
#create correlation matrix to identify some varianbles to plot against eachother
matrixxx = cor(my_data)
round(matrixxx, 2)

#check out visual format of relationships
pairs(Boston)

plot(crim,rm)
cat(paste("formula: plot(crim,rm)", "crime rates are too low in many suburbs to visually make an observation about average rooms. HOwever, if the average rooms is greater than 7.5 then the crime rate is likely to be close to zero per capita.",sep="\n"))

plot(lstat,age)
cat(paste("formula: plot(lstat,age)", "suburbs with greater than 20% of the population containing lower status people are likely to have at least ~80% of the homes built before 1940",sep="\n"))

plot(age,dis)
cat(paste("formula: plot(age,dis)", "the closer to the employment centers, the higher the proportion of houses built before 1940",sep="\n"))

plot(age,crim)
cat(paste("formula: plot(age,crim)", "places with higher proportions of older homes tend to have more crime",sep="\n"))

plot(dis,crim)
cat(paste("formula: plot(dis,crim)", "places closer to the employment centers tend to have higher rates of crime",sep="\n"))


cat(paste("Chapter 2: #10(c)", "are any of the predictors associated with per capita crime rate? If so, explain the relationship.",sep="\n"))
cat(paste("formula: plot(age,crim)", "places with higher proportions of older homes tend to have more crime",sep="\n"))
cat(paste("formula: plot(crim,rm)", "crime rates are too low in many suburbs to visually make an observation about average rooms. However, if the average rooms is greater than 7.5 then the crime rate is likely to be close to zero per capita.",sep="\n"))
cat(paste("formula: plot(dis,crim)", "places closer to the employment centers tend to have higher rates of crime",sep="\n"))
cat(paste("Chapter 2: #10(d) Do any of the suburbs of Boston appear to have particularly high crime rates? Tax rates? Pupil-teacher ratios? Comment on the range of each predictor.",sep="\n"))

#
?hist
#look at data points to figure out best way to chart this in a histogram
hist(Boston$crim,breaks = 20,plot = FALSE)
mean(Boston$crim[Boston$crim<2.5])

#plot histograms and split into two populations to get better detail on lower crime per capita
hist(Boston$crim,breaks = 20,plot = TRUE, labels = TRUE)
hist(Boston$crim[Boston$crim<2.5],breaks = 20)
hist(Boston$crim[Boston$crim>=2.5],breaks = 20)

#count suburbs with crime rates greater than 10
length(Boston$crim[Boston$crim>10])





cat(paste("Chapter 2: #10(d) Do any of the suburbs of Boston appear to have particularly high crime rates?","yes, 54 suburbs have a per capita crime rate above 10",sep="\n"))
cat(paste("Chapter 2: #10(d) Do any of the suburbs of Boston appear to have particularly high Tax rates?","Yes, most(340) of the distribtuion is in between 180 and 420, but there is a population of 132 suburbs with rates of 666  ",sep="\n"))
cat(paste("Chapter 2: #10(d) Do any of the suburbs of Boston appear to have particularly high Pupil-teacher ratios?","Yes, but this distribution was a little more uniform across the spectrum than the other two.
          However, a single ratio of 20.2 occurs 140 times" ,sep="\n"))

#look at data points to figure out best way to chart this in a histogram
hist(Boston$tax,breaks = 20,plot = FALSE)
mean(Boston$tax)
          
hist(Boston$tax,breaks = 20,plot = TRUE)

#plot histograms and split into two populations to get better detail
hist(Boston$tax,breaks = 20,plot = TRUE, labels = TRUE)
hist(Boston$tax[Boston$tax<420],breaks = 20)
hist(Boston$tax[Boston$tax>=420],breaks = 20)

#count suburbs with taxe rates greater/less than 420
length(Boston$tax[Boston$tax>420])
length(Boston$tax[Boston$tax<420])
print(Boston$tax[Boston$tax>420])
length(Boston$tax[Boston$tax==666])

#look at data points to figure out best way to chart this in a histogram
hist(Boston$ptratio,breaks = 20,plot = FALSE)
mean(Boston$ptratio)

hist(Boston$ptratio,breaks = 20,plot = TRUE, labels = TRUE)

#plot histograms and split into two populations to get better detail
hist(Boston$ptratio,breaks = 20,plot = TRUE, labels = TRUE)
hist(Boston$ptratio[Boston$ptratio<20],breaks = 20)

hist(Boston$ptratio[Boston$ptratio>=20],breaks = 20)
     
#count suburbs with ptratioe rates greater than 20
length(Boston$ptratio[Boston$ptratio>20])
length(Boston$ptratio[Boston$ptratio==20.2])
        






Suburbs_on_River = length(Boston$chas[Boston$chas==1])
cat(paste("Chapter 2: #10(e) How many of the suburbs in this data set bound the Charles river?","\n", Suburbs_on_River, "suburbs on the river"))          








median_answer = median(Boston$ptratio)
cat(paste("Chapter 2: #10(f) What is the median pupil-teacher ratio among the towns in this data set?","\n","the median is ",median_answer))









cat(paste("Chapter 2: #10(g) Which suburb of Boston has lowest median value of owner- occupied homes?", 
          "What are the values of the other predictors for that suburb?",
          "how do those values compare to the overall ranges for those predictors?",sep="\n"))
min(Boston$medv)
?subset
t(subset(Boston, medv == min(Boston$medv)))
length(Boston$medv[Boston$medv==5])
colMeans(Boston)
quant_percentile = c(0.25,0.50,0.75)

"What are the values of the other predictors for that suburb?"
t(apply( Boston , 2 , quantile , probs = quant_percentile))





cat(paste("Chapter 2: #10(g) Which suburb of Boston has lowest median value of owner- occupied homes?","Suburb 399 and 406 both have the minimum median value in the dataset",
          "What are the values of the other predictors for that suburb?",t(subset(Boston, medv == min(Boston$medv))),"how do those values compare to the overall ranges for those predictors?","Both suburbs have top quartile rates of non-retail business, high nox concentrations, low average room number, older homes, closer to employment centers, top quartile accessibilty to railways, high full-value property tax, high pupil-teacher ratio, above median proportion of black population, high percent of lower status population"
          ,sep="\n"))

#             25%       50%        75%
# crim      0.082045   0.25651   3.677083 
# zn        0.000000   0.00000  12.500000
# indus     5.190000   9.69000  18.100000
# chas      0.000000   0.00000   0.000000
# nox       0.449000   0.53800   0.624000
# rm        5.885500   6.20850   6.623500
# age      45.025000  77.50000  94.075000
# dis       2.100175   3.20745   5.188425
# rad       4.000000   5.00000  24.000000
# tax     279.000000 330.00000 666.000000
# ptratio  17.400000  19.05000  20.200000
# black   375.377500 391.44000 396.225000
# lstat     6.950000  11.36000  16.955000
# medv     17.025000  21.20000  25.000000

#           399      406
# crim     38.3518  67.9208
# zn        0.0000   0.0000
# indus    18.1000  18.1000
# chas      0.0000   0.0000
# nox       0.6930   0.6930
# rm        5.4530   5.6830
# age     100.0000 100.0000
# dis       1.4896   1.4254
# rad      24.0000  24.0000
# tax     666.0000 666.0000
# ptratio  20.2000  20.2000
# black   396.9000 384.9700
# lstat    30.5900  22.9800
# medv      5.0000   5.0000


# 1 crim - per capita crime rate by town.",
#           "2 zn - proportion of residential land zoned for lots over 25,000 sq.ft.",
# "3 indus -proportion of non-retail business acres per town.",
# "4 chas - Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).",
# "5 nox - nitrogen oxides concentration (parts per 10 million).",
# "6 rm - average number of rooms per dwelling.",
# "7 age - proportion of owner-occupied units built prior to 1940.",
# "8 dis - weighted mean of distances to five Boston employment centres.",
# "9 rad - index of accessibility to radial highways.",
# "10 tax - full-value property-tax rate per $10,000.",
# "11 ptratio - pupil-teacher ratio by town.",
# "12 black - 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town.",
# "13 lstat - lower status of the population (percent).",
# "14 medv - median value of owner-occupied homes in $1000s.",sep="\n"))



length(Boston$rm[Boston$rm>7])
cat(paste("Chapter 2: #10(h) In this data set, how many of the suburbs average more than seven rooms per dwelling?",
          length(Boston$rm[Boston$rm>7]),"More than eight rooms per dwelling?", length(Boston$rm[Boston$rm>8]),
          "Comment on the suburbs that average more than eight rooms per dwelling.","low crime, high retail business zoning, newer buildings",sep="\n"))

summary(subset(Boston,Boston$rm>7))











###################################################################

# CHAPT 3 #15
# This problem involves the Boston data set, which we saw in the lab for this chapter. We will now try to predict per capita crime rate using the other variables in this data set. In other words, per capita crime rate is the response, and the other variables are the predictors.
# (a) For each predictor, fit a simple linear regression model to predict the response. Describe your results. In which of the models is there a statistically significant association between the predictor and the response? Create some plots to back up your assertions.
# (b) Fit a multiple regression model to predict the response using all of the predictors. Describe your results. For which predictors can we reject the null hypothesis H0 : βj = 0?
# (c) How do your results from (a) compare to your results from (b)? Create a plot displaying the univariate regression coefficients from (a) on the x-axis, and the multiple regression coefficients from (b) on the y-axis. That is, each predictor is displayed as a single point in the plot. Its coefficient in a simple linear regres- sion model is shown on the x-axis, and its coefficient estimate in the multiple linear regression model is shown on the y-axis.
# (d) Is there evidence of non-linear association between any of the predictors and the response? To answer this question, for each predictor X, fit a model of the form
# Y = β0 +β1X +β2X2 +β3X3 +ε.


#3 (a) For each predictor, fit a simple linear regression model to predict the response. Describe your results. In which of the models is there a statistically significant association between the predictor and the response? Create some plots to back up your assertions.


library(MASS)
summary(Boston)
Boston$chas = factor(Boston$chas, labels = c("N","Y"))
attach(Boston)

lm.zn = lm(crim~zn)
summary(lm.zn) 
plot(lm.zn)


lm.indus = lm(crim~indus)
summary(lm.indus) 
plot(lm.indus)


lm.chas = lm(crim~chas) 
summary(lm.chas) 
plot(lm.chas)


lm.nox = lm(crim~nox)
summary(lm.nox) 
plot(lm.nox)


lm.rm = lm(crim~rm)
summary(lm.rm) 
plot(lm.rm)

lm.age = lm(crim~age)
summary(lm.age) 
plot(lm.age)

lm.dis = lm(crim~dis)
summary(lm.dis) 
plot(lm.dis)


lm.rad = lm(crim~rad)
summary(lm.rad) 
plot(lm.rad)

lm.tax = lm(crim~tax)
summary(lm.tax) 
plot(lm.tax)

lm.ptratio = lm(crim~ptratio)
summary(lm.ptratio) 
plot(lm.ptratio)

lm.black = lm(crim~black)
summary(lm.black) 
plot(lm.black)


lm.lstat = lm(crim~lstat)
summary(lm.lstat) 
plot(lm.lstat)

lm.medv = lm(crim~medv)
summary(lm.medv) 
plot(lm.medv)

# lm.zn	Yes
# lm.indus	Yes
# lm.chas	No
# lm.nox	Yes
# lm.rm	Yes
# lm.age	Yes
# lm.dis	Yes
# lm.rad	Yes
# lm.tax	Yes
# lm.ptratio	Yes
# lm.black	Yes
# lm.lstat	Yes
# lm.medv	Yes


# (b) Fit a multiple regression model to predict the response using all of the predictors. 
#Describe your results. For which predictors can we reject the null hypothesis H0 : βj = 0?
lm.allvariables = lm(crim~., data=Boston)
summary(lm.allvariables)


# (c) How do your results from (a) compare to your results from (b)? Create a plot displaying the univariate regression coefficients from (a) on the x-axis, and the multiple regression coefficients from (b) on the y-axis. That is, each predictor is displayed as a single point in the plot. Its coefficient in a simple linear regres- sion model is shown on the x-axis, and its coefficient estimate in the multiple linear regression model is shown on the y-axis.
x = c(coefficients(lm.zn)[2], coefficients(lm.indus)[2],
      coefficients(lm.chas)[2],
      coefficients(lm.nox)[2],
      coefficients(lm.rm)[2],
      coefficients(lm.age)[2],
      coefficients(lm.dis)[2],
      coefficients(lm.rad)[2],
      coefficients(lm.tax)[2],
      coefficients(lm.ptratio)[2],
      coefficients(lm.black)[2],
      coefficients(lm.lstat)[2],
      coefficients(lm.medv)[2])
y = coefficients(lm.allvariables)[2:14]
plot(x, y)

# similar with the exception of nox highlighted in the graph below. (simple =31, multi =-11)


# (d) Is there evidence of non-linear association between any of the predictors and the response? 
#To answer this question, for each predictor X, fit a model of the form
# Y = β0 +β1X +β2X2 +β3X3 +ε.

lm.zn = lm(crim~poly(zn,3))
summary(lm.zn) 

lm.indus = lm(crim~poly(indus,3))
summary(lm.indus) 

#: N/A - dummy variable
# lm.chas = lm(crim~poly(chas,3)) 
# summary(lm.chas)

lm.nox = lm(crim~poly(nox,3))
summary(lm.nox) 

lm.rm = lm(crim~poly(rm,3))
summary(lm.rm) 

lm.age = lm(crim~poly(age,3))
summary(lm.age) 

lm.dis = lm(crim~poly(dis,3))
summary(lm.dis) 


lm.rad = lm(crim~poly(rad,3))
summary(lm.rad) 


lm.tax = lm(crim~poly(tax,3))
summary(lm.tax) 

lm.ptratio = lm(crim~poly(ptratio,3))
summary(lm.ptratio) 


lm.black = lm(crim~poly(black,3))
summary(lm.black) # 1


lm.lstat = lm(crim~poly(lstat,3))
summary(lm.lstat) 

lm.medv = lm(crim~poly(medv,3))
summary(lm.medv) 

#Show below: Absolute value of T-Values greater than two 
# lm.zn	1	2	
# lm.indus	1	2	3
# lm.chas	1	2	3
# lm.nox	1	2	3
# lm.rm	1	2	
# lm.age	1	2	3
# lm.dis	1	2	3
# lm.rad	1	2	
# lm.tax	1	2	
# lm.ptratio	1	2	3
# lm.black	1		
# lm.lstat	1	2	
# lm.medv	1	2	3













###################################################################

# CHAPTER 6 #9
# 9. In this exercise, we will predict the number of applications received using the other variables in the College data set.
# (a) Split the data set into a training set and a test set.
#install.packages('ISLR') 

#https://faculty.mccombs.utexas.edu/carlos.carvalho/teaching/Section2_MSBA.txt
#has PCR, Lasso, Ridge


rm(list=ls())
library(ISLR)
set.seed(5)

my_data = College
sum(is.na(my_data))

attach(my_data)


#set up training and test data
train = sample(1:dim(my_data)[1], dim(my_data)[1] / 2)
test = (-train)
my_data.train = my_data[train, ]
my_data.test = my_data[test, ]





# (b) Fit a linear model using least squares on the training set, and
# report the test error obtained.

lm.fit = lm(Apps~., data=my_data.train)

lm.pred = predict(lm.fit, my_data.test)

#target variable versus model
lm.error.calc = mean((my_data.test[, "Apps"] - lm.pred)^2)
lm.error.calc
#mean((my_data.test[, "Apps"] - lm.pred)^2)
#Test RSS 1835615

RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}
RMSE(lm.pred,my_data.test[, "Apps"])
#1354.849

# (c) Fit a ridge regression model on the training set, with λ chosen
# by cross-validation. Report the test error obtained.
#install.packages(pkgs = "glmnet") 
library(glmnet)
train.matrix = model.matrix(Apps~., data=my_data.train)
test.matrix = model.matrix(Apps~., data=my_data.test)
grid = 10 ^ seq(4, -2, length=100)


cv.out.ridge = cv.glmnet(train.matrix, my_data.train[, "Apps"], alpha=0, lambda=grid, thresh=1e-12)
lambda.best = cv.out.ridge$lambda.min
lambda.best
#1] 24.77076

ridge.pred = predict(cv.out.ridge, newx=test.matrix, s=lambda.best)
ridge.errors.calc = mean((my_data.test[, "Apps"] - ridge.pred)^2)
ridge.errors.calc 
#ridge errors RSS 1870783

RMSE(ridge.pred,my_data.test[, "Apps"])
#RMSE 1367.766

# (d) Fit a lasso model on the training set, with λ chosen by cross- validation. 
#Report the test error obtained, along with the number of non-zero coefficient estimates.
lasso.fit = cv.glmnet(train.matrix, my_data.train[, "Apps"], alpha=1, lambda=grid, thresh=1e-12)
lambda.best2 = lasso.fit$lambda.min
lambda.best2

#12.32847

lasso.pred = predict(lasso.fit, newx=test.matrix, s=lambda.best2)
errors_calc2 = mean((my_data.test[, "Apps"] - lasso.pred)^2)
errors_calc2
#  1874525

RMSE(lasso.pred,my_data.test[, "Apps"])
#1369.133



lasso.fit2 = glmnet(model.matrix(Apps~., data=my_data), my_data[, "Apps"], alpha=1)
lasso.pred2 = predict(lasso.fit2, s=lambda.best2, type="coefficients")




# (e) Fit a PCR model 
#install.packages("pls")
library(pls)
pcr.fit = pcr(Apps~., data=my_data.train, scale=T, validation="CV")
validationplot(pcr.fit, val.type="MSEP")

pcr.pred = predict(pcr.fit, my_data.test, ncomp=10)
pcr.error = mean((my_data.test[, "Apps"] - pcr.pred)^2)
pcr.error

#test rss for pcr
#3280999
RMSE(pcr.pred,my_data.test[, "Apps"])
#1811.353


#on the training set, with M chosen by cross- validation. Report the test error obtained, along with the value of M selected by cross-validation.
# (f) Fit a PLS model on the training set, with M chosen by cross- validation. Report the test error obtained, along with the value of M selected by cross-validation.
pls.fit = plsr(Apps~., data=my_data.train, scale=T, validation="CV")
validationplot(pls.fit, val.type="MSEP")

pls.pred = predict(pls.fit, my_data.test, ncomp=10)


pls.error = mean((my_data.test[, "Apps"] - pls.pred)^2)
pls.error
# RSS 1833469
RMSE(pls.pred, my_data.test[, "Apps"])
#1354.057


# (g) Comment on the results obtained. How accurately can we pre- dict the number of college applications received? 
#Is there much difference among the test errors resulting from these five ap- proaches?
#   

#R Squared Calculation
test.avg = mean(my_data.test[, "Apps"])
lm.test.r2 = 1 - mean((my_data.test[, "Apps"] - lm.pred)^2) /mean((my_data.test[, "Apps"] - test.avg)^2)
ridge.test.r2 = 1 - mean((my_data.test[, "Apps"] - ridge.pred)^2) /mean((my_data.test[, "Apps"] - test.avg)^2)
lasso.test.r2 = 1 - mean((my_data.test[, "Apps"] - lasso.pred)^2) /mean((my_data.test[, "Apps"] - test.avg)^2)
pcr.test.r2 = 1 - mean((my_data.test[, "Apps"] - pcr.pred)^2) /mean((my_data.test[, "Apps"] - test.avg)^2)
pls.test.r2 = 1 - mean((my_data.test[, "Apps"] - pls.pred)^2) /mean((my_data.test[, "Apps"] - test.avg)^2)
table
lm.test.r2
ridge.test.r2
lasso.test.r2
pcr.test.r2
pls.test.r2

#RMSE Calc
RMSE(lm.pred,my_data.test[, "Apps"])
RMSE(ridge.pred,my_data.test[, "Apps"])
RMSE(lasso.pred,my_data.test[, "Apps"])
RMSE(pcr.pred,my_data.test[, "Apps"])
RMSE(pls.pred,my_data.test[, "Apps"])

#Test r squared is consistent across models except PCR is lower. 
#Therefore, all of the models are accurate predictors (except PCR)
















###################################################################
# CHAPTER 6 #11
# 11. We will now try to predict per capita crime rate in the Boston data set.
# (a) Try out some of the regression methods explored in this chapter, 
#such as best subset selection, the lasso, ridge regression, and PCR. Present and discuss results for the approaches that you consider.

#install.packages("leaps")
rm(list=ls())
set.seed(5)
library(MASS)
library(leaps)
library(glmnet)

my_data = Boston
attach(my_data)


#subset selection
predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}


#parameter setting
k = 10
p = ncol(my_data) - 1
folds = sample(rep(1:k, length = nrow(my_data)))
cv.errors.matrix = matrix(NA, k, p)

for (i in 1:k) {
  bestsubs.fit = regsubsets(crim ~ ., data = my_data[folds != i, ], nvmax = p)
 
  
   for (j in 1:p) {
    bestsubs.pred = predict(bestsubs.fit, my_data[folds == i, ], id = j)
    cv.errors.matrix[i, j] = mean((my_data$crim[folds == i] -  bestsubs.pred)^2)
  }
}


#error term
rmse.cv = sqrt(apply(cv.errors.matrix, 2, mean))


#testing this calculation versus the one above
RMSE(bestsubs.pred,my_data$crim[folds == i])


#plot the error term 
plot(rmse.cv, pch = 23, type = "b")
which.min(rmse.cv)
#12


#minimum error term
rmse.cv[which.min(rmse.cv)]
#6.545087




#Lasso
x = model.matrix(crim ~ . - 1, data = my_data)
y = my_data$crim

cv.lasso = cv.glmnet(x, y, type.measure = "mse")
plot(cv.lasso)
coef(cv.lasso)
sqrt(cv.lasso$cvm[cv.lasso$lambda == cv.lasso$lambda.1se])
#7.580322




#RIDGE
# dev.off()
x = model.matrix(crim ~ . - 1, data = my_data)
y = my_data$crim
cv.ridge = cv.glmnet(x, y, type.measure = "mse", alpha = 0)
plot(cv.ridge)
coef(cv.ridge)
sqrt(cv.ridge$cvm[cv.ridge$lambda == cv.ridge$lambda.1se])
# 7.8333


#PCR
library(pls)
pcr.fit = pcr(crim ~ ., data = my_data, scale = TRUE, validation = "CV")
summary(pcr.fit)
plot(pcr.fit)
# 13-component pcr fit = lowest Cross Validated RMSE

# (b) Propose a model (or set of models) that seem to perform well on this data set, and justify your answer. Make sure that you are evaluating model performance using validation set error, cross- validation, or some other reasonable alternative, as opposed to using training error.
# (c) Does your chosen model involve all of the features in the data set? Why or why not?

# I would choose the 12 component best subset selection model because it has the lowest cross validated rmse while having fewer components than the other best model (13 component pcr) 
# 
# However, I do notice on the graph of the subset selection that there is a diminishing return around the 9 component mark. It would probably make sense to choose a model with fewer components while maintaining a lowe RMSE. 9,10, or 11 component best subset selection would be ideal 
# 












###################################################################
# CHAPTER 4 #10 (not "e" and "f")
# 10. This question should be answered using the Weekly data set, which is part of the ISLR package. This data is similar in nature to the Smarket data from this chapter’s lab, except that it contains 1,089 weekly returns for 21 years, from the beginning of 1990 to the end of 2010.
# (a) Produce some numerical and graphical summaries of the Weekly data. Do there appear to be any patterns?
library(ISLR)
summary(Weekly)
pairs(Weekly)
cor(Weekly[, -9])
pairs(Weekly)

# (b) Use the full data set to perform a logistic regression with Direction as the response 
#and the five lag variables plus Volume as predictors. Use the summary function to print the results. Do any of the predictors appear to be statistically significant? If so, which ones?
attach(Weekly)
glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, 
              family = binomial)
summary(glm.fit)

# (c) Compute the confusion matrix and overall fraction of correct predictions. 
#Explain what the confusion matrix is telling you about the types of mistakes made by logistic regression.
glm.probs = predict(glm.fit, type = "response")
glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction)


# (d) Now fit the logistic regression model using a training data period from 1990 to 2008, 
#with Lag2 as the only predictor. Compute the confusion matrix and the overall fraction of correct predictions for the held out data 
#(that is, the data from 2009 and 2010).
train = (Year < 2009)
Weekly.0910 = Weekly[!train, ]
glm.fit = glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
glm.probs = predict(glm.fit, Weekly.0910, type = "response")
glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] = "Up"
Direction.0910 = Direction[!train]
table(glm.pred, Direction.0910)


mean(glm.pred == Direction.0910)

# SKIP (E) & (F)
# (g) Repeat (d) using KNN with K = 1.

library(class)
train.X = as.matrix(Lag2[train])
test.X = as.matrix(Lag2[!train])
train.Direction = Direction[train]
set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.0910)


# (h) Which of these methods appears to provide the best results on
# this data?

#Logistic and LDA methods 

# (i) Experiment with different combinations of predictors, including possible 
#transformations and interactions, for each of the methods. 
#Report the variables, method, and associated confu- sion matrix that 
#appears to provide the best results on the held out data. 
#Note that you should also experiment with values for K in the KNN classifier.
# Logistic regression with Lag2:Lag1






###################################################################

# # Chapter 8 #8
# 8.	In the lab, a classification tree was applied to the Carseats data set af- ter converting Sales into a qualitative response variable. Now we will seek to predict Sales using regression trees and related approaches, treating the response as a quantitative variable. 
# (a)  Split the data set into a training set and a test set.
install.packages("tree")

library(MASS)
library(randomForest)
set.seed(5)

train = sample(dim(Carseats)[1], dim(Carseats)[1]/2)
Carseats.train = Carseats[train, ]
Carseats.test = Carseats[-train, ]
# (b)  Fit a regression tree to the training set. Plot the tree, and inter- pret the results. What test error rate do you obtain? 
library(tree)
tree.carseats = tree(Sales ~ ., data = Carseats.train)
summary(tree.carseats)



pred.carseats = predict(tree.carseats, Carseats.test)
mean((Carseats.test$Sales - pred.carseats)^2)

# MSE is about 4.101



# (c)  Use cross-validation in order to determine the optimal level of tree complexity. Does pruning the tree improve the test error rate? 
cv.carseats = cv.tree(tree.carseats, FUN = prune.tree)
par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")

pruned.carseats = prune.tree(tree.carseats, best = 9)
par(mfrow = c(1, 1))
plot(pruned.carseats)
text(pruned.carseats, pretty = 0)


pred.pruned = predict(pruned.carseats, Carseats.test)
mean((Carseats.test$Sales - pred.pruned)^2)




# (d)  Use the bagging approach in order to analyze this data. 
#What test error rate do you obtain? Use the importance() function to 
#determine which variables are most important. 
# Best size = 9
library(randomForest)


bag.carseats = randomForest(Sales ~ ., data = Carseats.train, mtry = 10, ntree = 500, 
    importance = T)
bag.pred = predict(bag.carseats, Carseats.test)
mean((Carseats.test$Sales - bag.pred)^2)

importance(bag.carseats)

# (e) Use random forests to analyze this data. What test error rate do you obtain? Use the importance() function to determine which variables are most important. Describe the effect of m, the num- ber of variables considered at each split, on the error rate obtained
# 

rf.carseats = randomForest(Sales ~ ., data = Carseats.train, mtry = 5, ntree = 500, 
    importance = T)
rf.pred = predict(rf.carseats, Carseats.test)
mean((Carseats.test$Sales - rf.pred)^2)


importance(rf.carseats)












###################################################################

# Chapter 8 #11
# 11.	This question uses the Caravan data set. 
# (a)  Create a training set consisting of the first 1,000 observations, 
# and a test set consisting of the remaining observations.
rm(list=ls())
library(ISLR)
library(gbm)
train = 1:1000
Caravan$Purchase = ifelse(Caravan$Purchase == "Yes", 1, 0)
Caravan.train = Caravan[train, ]
Caravan.test = Caravan[-train, ]


# (b)  Fit a boosting model to the training set with Purchase as the response and 
#the other variables as predictors. Use 1,000 trees, and a shrinkage value of 0.01. 
#Which predictors appear to be the most important? 

set.seed(5)
boost.caravan = gbm(Purchase ~ ., data = Caravan.train, n.trees = 1000, shrinkage = 0.01, 
    distribution = "bernoulli")
summary(boost.caravan)



# (c)  Use the boosting model to predict the response on the test data. 
#Predict that a person will make a purchase if the estimated prob- ability of purchase is greater than 20 %. Form a confusion ma- trix. What fraction of the people predicted to make a purchase do in fact make one? How does this compare with the results obtained from applying KNN or logistic regression to this data set? 
# 
boost.prob = predict(boost.caravan, Caravan.test, n.trees = 1000, type = "response")
boost.pred = ifelse(boost.prob > 0.2, 1, 0)
table(Caravan.test$Purchase, boost.pred)

35/(123 + 35)
# ~22% of the predictions to make a purchase actuallly do make a purchase



lm.caravan = glm(Purchase ~ ., data = Caravan.train, family = binomial)

lm.prob = predict(lm.caravan, Caravan.test, type = "response")
lm.pred = ifelse(lm.prob > 0.2, 1, 0)
table(Caravan.test$Purchase, lm.pred)


58/(350 + 58)
#~14% of the predictions to make a purchase actuallly do make a purchase
# Boosting is superior to the linear model


###################################################################
# Problem 1: Beauty Pays!
#   Professor Daniel Hamermesh from UT’s economics department has been studying 
#the impact of beauty in labor income (yes, this is serious research!!).
# First, watch the following video:
#   http://thedailyshow.cc.com/videos/37su2t/ugly-people-prejudice
# It turns out this is indeed serious research and Dr. Hamermesh has demonstrated the effect of beauty into income in a 
#variety of different situations. Here’s an example: in the paper “Beauty in the Classroom” 
#they showed that “...instructors who are viewed as better looking receive higher 
#instructional ratings” leading to a direct impact in the salaries in the long run.
# By now, you should know that this is a hard effect to measure. Not only one has to work 
#hard to figure out a way to measure “beauty” objectively (well, the video said it all!) 
#but one also needs to “adjust for many other determinants” (gender, lower division class, 
#native language, tenure track status).
# So, Dr. Hamermesh was kind enough to share the data for this paper with us. 
#It is available in our class website in the file “BeautyData.csv”. In the file you will 
#find, for a number of UT classes, course ratings, a relative measure of beauty for the 
#instructors, and other potentially relevant variables.
# 1. Using the data, estimate the effect of “beauty” into course ratings. Make sure to 
#think about the potential many “other determinants”. Describe your analysis and your 
#conclusions.
# 2. In his paper, Dr. Hamermesh has the following sentence: “Disentangling whether 
#this outcome represents productivity or discrimination is, as with the issue generally, 
#probably impossible”. Using the concepts we have talked about so far, what does he mean 
#by that?


#Regression Script & notes
#https://faculty.mccombs.utexas.edu/carlos.carvalho/teaching/Section2_MSBA.txt


Beauty = read.csv("BeautyData.csv")
attach(Beauty)

Beauty1 = lm(CourseEvals~., data = Beauty)
head(Beauty,5)

summary(Beauty1)
confint(Beauty1)
plot(CourseEvals,BeautyScore,pch=19,cex=1,xlab="Course Evals",ylab = "Beauty Score")
abline(lm(BeautyScore ~ CourseEvals))
        

# dev.off()
# Beauty2 = lm(CourseEvals~ BeautyScore, data = beauty)
# summary(Beauty2)
# # 
# Beauty3 = lm(CourseEvals~. -BeautyScore, data = beauty)
# summary(Beauty3)






##############################################
# Problem 2: Housing Price Structure
# The file MidCity.xls, available on the class website, contains data on 128 recent 
##sales of houses in a town. For each sale, the file shows the

#neighborhood in which the house is located, 1 and 2 are more traditional whereas 3 is a more modern, newer and more prestigious part of town
#the number of offers made on the house, #
#the square footage, 
#whether the house is made out of brick, 
#the number of bathrooms, the number of bedrooms, 
#and the selling price. 

#Use regression models to estimate the pricing structure of houses in this town and answer the following questions:
#   1. Is there a premium for brick houses everything else being equal?
#   2. Is there a premium for houses in neighborhood 3?
#   3. Is there an extra premium for brick houses in neighborhood 3?
#   4. For the purposes of prediction could you combine the 
#neighborhoods 1 and 2 into a single “older” neighborhood?


#https://faculty.mccombs.utexas.edu/carlos.carvalho/teaching/Section2_MSBA.txt
rm(list=ls())

house = read.csv("MidCity.csv",header=T)
attach(house)
head(house,10)

#Neihborhood Variable Transformation - 1 and 2 are more traditional
n = dim(house)[1]
dn1 = rep(0,n)
dn1[Nbhd==1]=1


#Neihborhood Variable Transformation - 1 and 2 are more traditional
dn2 = rep(0,n)
dn2[Nbhd==2]=1

#Neihborhood Variable Transformation - 3 is modern and prestigious
dn3 = rep(0,n)
dn3[Nbhd==3]=1

#Brick Variable Transformation
BR = rep(0,n)
BR[Brick=="Yes"]=1

#Reduce price to more manageable digits
Price = Price/1000
SqFt = SqFt/1000

#All variables included except for Home(row count) and the variables that were transformed to dummy variables
testmodel = lm(Price~. -Home -Nbhd -Brick +BR +dn2 +dn3, data = house)
summary(testmodel)
confint(testmodel)

plot(SqFt,Price,xlab="Square Feet / 1,000")
abline(lm(Price ~ SqFt),col=1,lwd=2,lty=2)

Nbhd = factor(Nbhd)


#Add Brick and Neighbordhood 3 interaction
testmodel2 = lm(Price~. -Home -Nbhd -Brick +BR +dn2 +dn3 +BR:dn3, data = house)
summary(testmodel2)
confint(testmodel2)

#check out combining neighborhood 1 and 2
rm(list=ls())
house = read.csv("MidCity.csv",header=T)
attach(house)
head(house,10)

#Brick Dummy Variable
n = dim(house)[1]
BR = rep(0,n)
BR[Brick=="Yes"]=1

#Neihborhood Variable Transformation - 1 and 2 are more traditional

dn12 = rep(0,n)
dn12[Nbhd==1]=1
dn12[Nbhd==2]=1

#Neihborhood Variable Transformation - 3 is modern and prestigious
dn3 = rep(0,n)
dn3[Nbhd==3]=1

testmodel3 = lm(Price~. -Home -Nbhd -Brick +BR +dn12 +dn3 +BR:dn3, data = house)
summary(testmodel3)
confint(testmodel3)


###################################################################
# Problem 3: What causes what??
#   Listen to this podcast:
#   http://www.npr.org/blogs/money/2013/04/23/178635250/episode-453-what-causes-what
# 1. Why can’t I just get data from a few different cities and run the regression of 
#“Crime” on “Police” to understand how more cops in the streets affect crime? 
#(“Crime” refers to some measure of crime rate and “Police” measures the number 
#of cops in a city)
# 2. How were the researchers from UPENN able to isolate this effect? 
#Briefly describe their approach and discuss their result in the “Table 2” below.
# 3. Why did they have to control for METRO ridership? What was that trying to capture?
# 4. In the next page, I am showing you “Table 4” from the research paper. 
#Just focus on the first column of the table. Can you describe the model being 
#estimated here? What is the conclusion?
# effect of police on crime 271





###################################################################
# Problem 4: BART
# Apply BART to the California Housing Data example of Section 4. Does BART outperform RF or Boosting?

#--------------------------------------------------
#read-in data
rm(list=ls())
ca <- read.csv("CAhousing.csv",header=TRUE)
attach(ca)

#add some variables
ca$AveBedrms <- ca$totalBedrooms/ca$households
ca$AveRooms <- ca$totalRooms/ca$households
ca$AveOccupancy <- ca$population/ca$households
logMedVal <- log(ca$medianHouseValue)


head(ca,10)
ca <- ca[,-c(4,5,9)] # lose lmedval and the room totals
ca$logMedVal = logMedVal
summary(ca)

#assign independent variables to x and dependent variables to y
x = ca[,1:9]
y = logMedVal
head(cbind(x,y))

set.seed(99) #MCMC, so set the seed
nd=200 # number of kept draws
burn=50 # number of burn in draws

#BART 
bf = wbart(x,y,nskip=burn,ndpost=nd)

summary(bf)

#linear model
lmf = lm(y~.,data.frame(x,y))
fitmat = cbind(y,bf$yhat.train.mean,lmf$fitted.values)
colnames(fitmat)=c("y","BART","Linear")
cor(fitmat)

summary(lmf)
confint(lmf)


n=length(y) #total sample size
set.seed(14) #
ii = sample(1:n,floor(.75*n)) # indices for train data, 75% of data
xtrain=x[ii,]; ytrain=y[ii] # training data
xtest=x[-ii,]; ytest=y[-ii] # test data
cat("train sample size is ",length(ytrain)," and test sample size is ",length(ytest),"\n")

set.seed(99)
bf_train = wbart(xtrain,ytrain)
yhat = predict(bf_train,as.matrix(xtest))

yhat.mean = apply(yhat,2,mean)

plot(ytest,yhat.mean)
abline(0,1,col=2)



finbartrmse = sqrt(sum((ytest-yhat)^2)/length(ytest))
finbartrmse


RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}
RMSE(yhat,ytest)




RSS <- c(crossprod(lmf$residuals))
RSS
MSE <- RSS / length(lmf$residuals)
MSE
RMSE2 <- sqrt(MSE)
RMSE2

summary(lmf)
finrfrmse = sqrt(sum((catest$logMedVal-finrfpred)^2)/nrow(catest))

################################################################### 
# Problem 5: Neural Nets
# Re-run the boston housing data example using a single layer neural net. 
#Cross validate for a few choices of Size and decay parameters
rm(list=ls())
library(MASS)
my_data = Boston
attach(my_data)
set.seed(5)

summary(my_data)


### nn library
library(nnet)



###standardize the x's
minv = rep(0,14)
maxv = rep(0,14)
my_datac = my_data
for(i in 1:14) {
  minv[i] = min(my_data[[i]])
  maxv[i] = max(my_data[[i]])
  my_datac[[i]] = (my_data[[i]]-minv[i])/(maxv[i]-minv[i])
}


###fit nn with just one x
bostonNN = nnet(medv~crim,data = my_datac,size=3,decay=.1,linout=T)


###get fits, print summary,  and plot fit
bostonNN.fit = predict(bostonNN,my_datac)
plot(my_datac$crim,my_datac$medv)
oo = order(my_datac$crim)
lines(my_datac$crim[oo],bostonNN[oo],col="red",lwd=2)
abline(lm(medv~crim,my_datac)$coef)

###  5 units
set.seed(5)
bostonNN = nnet(medv~crim,data = my_datac,size=5,decay=.1,linout=T)
print(summary(bostonNN))

### all  x's
bostonNN2 = nnet(medv~.,my_datac,size=5,decay=.1,linout=T)
bostonNN2.fit = predict(bostonNN2,my_datac)

boston.lm = lm(medv~.,my_datac)
boston.lm.fit = predict(boston.lm,my_datac)
temp = data.frame(y=my_datac$medv,fnn=bostonNN2.fit,flm=boston.lm.fit)
summary(boston.lm)
pairs(temp)
print(cor(temp))


#different sizes and decays
bostonNN3 = nnet(medv~crim,my_datac,size=5,decay=.5,linout=T)
bostonNN4 = nnet(medv~crim,my_datac,size=5,decay=.00001,linout=T)
bostonNN5 = nnet(medv~crim,my_datac,size=50,decay=.5,linout=T)
bostonNN6 = nnet(medv~crim,my_datac,size=50,decay=.00001,linout=T)
temp = data.frame(price = my_datac$medv, crime = my_datac$crim)
bostonNNf1 = predict(bostonNN3,temp)
bostonNNf2 = predict(bostonNN4,temp)
bostonNNf3 = predict(bostonNN5,temp)
bostonNNf4 = predict(bostonNN6,temp)

par(mfrow=c(2,2))
plot(my_datac$crim,my_datac$medv)
lines(my_datac$crim[oo],bostonNNf1[oo],lwd=2)
title("size=3, decay=.5")
plot(my_datac$crim,my_datac$medv)
lines(my_datac$crim[oo],bostonNNf2[oo],lwd=2)
title("size=3, decay=.00001")
plot(my_datac$crim,my_datac$medv)
lines(my_datac$crim[oo],bostonNNf3[oo],lwd=2)
title("size = 50, decay = .5")
plot(my_datac$crim,my_datac$medv)
lines(my_datac$crim[oo],bostonNNf4[oo],lwd=2)
title("size = 50, decay = .00001")


#different sizes and decays - All Variables
bostonNN7 = nnet(medv~.,my_datac,size=5,decay=.5,linout=T)
bostonNN8 = nnet(medv~.,my_datac,size=5,decay=.00001,linout=T)
bostonNN9 = nnet(medv~.,my_datac,size=50,decay=.5,linout=T)
bostonNN10 = nnet(medv~.,my_datac,size=50,decay=.00001,linout=T)
temp = data.frame(price = my_datac$medv, crime = my_datac)
bostonNNf5 = predict(bostonNN7,temp)
bostonNNf6 = predict(bostonNN8,temp)
bostonNNf7 = predict(bostonNN9,temp)
bostonNNf8 = predict(bostonNN10,temp)

par(mfrow=c(2,2))
plot(my_datac,my_datac$medv)
lines(my_datac[oo],bostonNNf1[oo],lwd=2)
title("size=3, decay=.5")
plot(my_datac,my_datac$medv)
lines(my_datac[oo],bostonNNf2[oo],lwd=2)
title("size=3, decay=.00001")
plot(my_datac,my_datac$medv)
lines(my_datac[oo],bostonNNf3[oo],lwd=2)
title("size = 50, decay = .5")
plot(my_datac,my_datac$medv)
lines(my_datac[oo],bostonNNf4[oo],lwd=2)
title("size = 50, decay = .00001")









###################################################################
# Problem 6: final project
# describe your contribution in one page or less
 



