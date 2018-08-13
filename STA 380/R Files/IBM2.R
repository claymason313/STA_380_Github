install.packages("corrplot")
install.packages("ggplot2")
library(corrplot)
library(ggplot2)
# Let’s build our R code here 
attribution = read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")
#remove Over18, EmployeeCount, EmployeeNumber, standardhour
attribution = attribution[ , c(-9,-10,-22, -27)]

# change type chr to factor
attribution$Attrition = as.factor(attribution$Attrition)
attribution$BusinessTravel = as.factor(attribution$BusinessTravel)
attribution$Department = as.factor(attribution$Department)
attribution$EducationField = as.factor(attribution$EducationField)
attribution$Gender = as.factor(attribution$Gender)
attribution$JobRole = as.factor(attribution$JobRole)
attribution$MaritalStatus = as.factor(attribution$MaritalStatus)
attribution$OverTime = as.factor(attribution$OverTime)

head(attribution)
library(corrplot)
numerical = unlist(lapply(attribution, is.numeric))
M = cor(attribution[, numerical])
corrplot.mixed(M, tl.cex=0.6)

dev.off()


source("http://www.sthda.com/upload/rquery_cormat.r")
cormat<-rquery.cormat(attribution[, numerical], graphType="heatmap")

source("http://www.sthda.com/upload/rquery_cormat.r")
cormat<-rquery.cormat(attribution[, numerical], type="full")



# Let’s build our R code here 
attribution = read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")
library(corrplot)
numerical = unlist(lapply(attribution, is.numeric))
M = cor(attribution[, numerical])
corrplot.mixed(M, tl.cex=0.6)

#  relationship
g = ggplot(attribution, aes(x = Attrition, group = RelationshipSatisfaction)) + geom_bar(aes(y = ..prop..), stat="count", fill="#FFA500", alpha=0.3)
g + facet_grid(.~RelationshipSatisfaction) + ggtitle("RelationshipSatisfaction")+theme_bw()+geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), stat= "count", vjust = -.3)


# YearswithCurrent manager matters
ggplot(attribution, aes(x=RelationshipSatisfaction, fill=Attrition)) + geom_density(alpha=0.3)

# Relationship Satisfaction
g = ggplot(attribution, aes(x = RelationshipSatisfaction, group = Attrition)) + geom_bar(aes(y = ..prop.., fill = Education), stat="count", fill="#FFA500", alpha=0.3)
g + facet_grid(.~Attrition) + ggtitle("Attrition")+theme_bw()+geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), stat= "count", vjust = -.5)

