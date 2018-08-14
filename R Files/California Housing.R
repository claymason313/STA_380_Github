###############################################################################
## California Housing data, fit regression trees on train, get loss on validation
## for a vector of cp values.
## Get cp values from rpart.
## Write fits (on validation) from best tree to file (thetreepred.txt)
################################################################################
rm(list=ls())

source('cal_setup.txt')
#install.packages("rpart")
library(rpart)
#--------------------------------------------------
#get big tree
ca <- read.csv("CAhousing.csv",header=TRUE)
ca$AveBedrms <- ca$totalBedrooms/ca$households
ca$AveRooms <- ca$totalRooms/ca$households
ca$AveOccupancy <- ca$population/ca$households
logMedVal <- log(ca$medianHouseValue)
ca <- ca[,-c(4,5,9)] # lose lmedval and the room totals
ca$logMedVal = logMedVal

#train, val, test
set.seed(99)
n=nrow(ca)
n1=floor(n/2)
n2=floor(n/4)
n3=n-n1-n2
ii = sample(1:n,n)
catrain=ca[ii[1:n1],]
caval = ca[ii[n1+1:n2],]
catest = ca[ii[n1+n2+1:n3],]



big.tree = rpart(logMedVal~.,method="anova",data=catrain,
                 control=rpart.control(minsplit=5,cp=.0001)) 
nbig = length(unique(big.tree$where))
cat('size of big tree: ',nbig,'\n')
#--------------------------------------------------
#fit on train, predict on val for vector of cp.
cpvec = big.tree$cptable[,"CP"] #cp values to try
ntree = length(cpvec) #number of cv values = number of trees fit.
iltree = rep(0,ntree) #in-sample loss
oltree = rep(0,ntree) #out-of-sample loss
sztree = rep(0,ntree) #size of each tree
for(i in 1:ntree) {
  if((i %% 10)==0) cat('tree i: ',i,'\n')
  temptree = prune(big.tree,cp=cpvec[i])
  sztree[i] = length(unique(temptree$where))
  iltree[i] = sum((catrain$logMedVal-predict(temptree))^2)
  ofit = predict(temptree,caval)
  oltree[i] = sum((caval$logMedVal-ofit)^2)
}
oltree=sqrt(oltree/nrow(caval)); iltree = sqrt(iltree/nrow(catrain))
oltree
#--------------------------------------------------
#plot losses

rgl = range(c(iltree,oltree))
plot(range(sztree),rgl,type='n',xlab='tree size',ylab='loss')
points(sztree,iltree,pch=15,col='red')
points(sztree,oltree,pch=16,col='blue')
legend("topright",legend=c('in-sample','out-of-sample'),lwd=3,col=c('red','blue'))

#--------------------------------------------------
#write val preds
iitree = which.min(oltree)
thetree = prune(big.tree,cp=cpvec[iitree])
thetreepred = predict(thetree,caval)
write(thetreepred,file='thetreepred.txt',ncol=1)
#--------------------------------------------------
rm(list=ls())


################################################################################
## California Housing data: fit boosting for values of 
##   (i) depth (ii) number of trees (iii) lamda = shrinkage.
## Fit on train, get loss on validation.
## Write fits on validition from best to file thebpred-2.txt.
################################################################################



source('cal_setup.txt')
install.packages("gbm")
library(gbm)
#--------------------------------------------------
set.seed(1)
idv = c(4,10)
ntv = c(1000,5000)
lamv=c(.001,.2)
parmb = expand.grid(idv,ntv,lamv)
colnames(parmb) = c('tdepth','ntree','lam')
print(parmb)
nset = nrow(parmb)
olb = rep(0,nset)
ilb = rep(0,nset)
bfitv = vector('list',nset)
for(i in 1:nset) {
  cat('doing boost ',i,' out of ',nset,'\n')
  tempboost = gbm(logMedVal~.,data=catrain,distribution='gaussian',
                  interaction.depth=parmb[i,1],n.trees=parmb[i,2],shrinkage=parmb[i,3])
  ifit = predict(tempboost,n.trees=parmb[i,2])
  
  ofit=predict(tempboost,newdata=caval,n.trees=parmb[i,2])
  olb[i] = sum((caval$logMedVal-ofit)^2)
  ilb[i] = sum((catrain$logMedVal-ifit)^2)
  bfitv[[i]]=tempboost
}
ilb = round(sqrt(ilb/nrow(catrain)),3); olb = round(sqrt(olb/nrow(caval)),3)
#--------------------------------------------------
#print losses

print(cbind(parmb,olb,ilb))

#--------------------------------------------------
#write val preds
iib=which.min(olb)
theb = bfitv[[iib]] 
thebpred = predict(theb,newdata=caval,n.trees=parmb[iib,2])
write(thebpred,file='thebpred-2.txt',ncol=1)


rm(list=ls())


################################################################################
## California Housing Data: fit random forests on train, get loss on validation
## using values of (i) mtry (ii) number of trees.
## Write out fits on validation from best t file  therfpred.txt.
################################################################################



source('cal_setup.txt')
install.packages("randomForest")
library(randomForest)
#--------------------------------------------------
set.seed(1)
p=ncol(catrain)-1
mtryv = c(p,sqrt(p))
ntreev = c(100,500)
parmrf = expand.grid(mtryv,ntreev)
colnames(parmrf)=c('mtry','ntree')
nset = nrow(parmrf)
olrf = rep(0,nset)
ilrf = rep(0,nset)
rffitv = vector('list',nset)
for(i in 1:nset) {
  cat('doing rf ',i,' out of ',nset,'\n')
  temprf = randomForest(logMedVal~.,data=catrain,mtry=parmrf[i,1],ntree=parmrf[i,2])
  ifit = predict(temprf)
  ofit=predict(temprf,newdata=caval)
  olrf[i] = sum((caval$logMedVal-ofit)^2)
  ilrf[i] = sum((catrain$logMedVal-ifit)^2)
  rffitv[[i]]=temprf
}
ilrf = round(sqrt(ilrf/nrow(catrain)),3); olrf = round(sqrt(olrf/nrow(caval)),3)
#----------------------------------------
#print losses

print(cbind(parmrf,olrf,ilrf))

#----------------------------------------
#write val preds
iirf=which.min(olrf)
therf = rffitv[[iirf]]
therfpred=predict(therf,newdata=caval)
write(therfpred,file='therfpred.txt',ncol=1)

rm(list=ls())


################################################################################
## California Housing Data: fit best boosting on (train, val), get fit on test.
## Get rmse on test.
## Write file: finbrmse.txt with just rmse on test.
## Plot: fit vs. y on test.
## Plot: variable importance.
## Plot: partial dependence plots. 
## Write file: cal-boost-varimport.txt with variable importance measures.
################################################################################


source('cal_setup.txt')
library(gbm)

#--------------------------------------------------
#fit on train+val
set.seed(1)
catrainval = rbind(catrain,caval)
ntrees=5000
finb = gbm(logMedVal~.,data=catrainval,distribution='gaussian',
           interaction.depth=4,n.trees=ntrees,shrinkage=.2)
finbpred=predict(finb,newdata=catest,n.trees=ntrees)
#--------------------------------------------------
#plot y vs yhat for test data and compute rmse on test.


finbrmse = sqrt(sum((catest$logMedVal-finbpred)^2)/nrow(catest))
cat('finbrmse: ',finbrmse,'\n')
plot(catest$logMedVal,finbpred,xlab='test logMedVal',ylab='boost pred')
abline(0,1,col='red',lwd=2)

#--------------------------------------------------
#plot variable importance
p=ncol(catrain)-1 #want number of variables for later
vsum=summary(finb) #this will have the variable importance info
row.names(vsum)=NULL #drop varable names from rows.



#write variable importance table
cat('\\begin{verbatim}\n')
print(vsum)
cat('\\end{verbatim}\n')

#plot variable importance
#the package does this automatically, but I did not like the plot
plot(vsum$rel.inf,axes=F,pch=16,col='red')
axis(1,labels=vsum$var,at=1:p)
axis(2)
for(i in 1:p) lines(c(i,i),c(0,vsum$rel.inf[i]),lwd=4,col='blue')

#--------------------------------------------------
#partial dependence plots

par(mfrow=c(3,3))
nms = names(catrain)[1:9]
for(i in 1:9) plot(finb,i=nms[i])


#--------------------------------------------------
rm(list=ls())


################################################################################
## California Housing: fit random forests on train+validation using best.
## Predict on test.
## Plot: fits vs test y.
## Plot: Variable importance.
## Write: rmse on test to file finrfrmse.txt. 
################################################################################


source('cal_setup.txt')
library(randomForest)

#--------------------------------------------------
#fit on train+val
set.seed(1)
catrainval = rbind(catrain,caval)
finrf = randomForest(logMedVal~.,data=catrainval,mtry=3,ntree=500)
finrfpred=predict(finrf,newdata=catest)
#--------------------------------------------------
#plot y vs yhat for test data

finrfrmse = sqrt(sum((catest$logMedVal-finrfpred)^2)/nrow(catest))
cat('finrfrmse: ',finrfrmse,'\n')
plot(catest$logMedVal,finrfpred,xlab='test logMedVal',ylab='rf pred')
abline(0,1,col='red',lwd=2)
#dev.off()

#--------------------------------------------------
#plot variable importance

varImpPlot(finrf)

g#--------------------------------------------------
rm(list=ls())