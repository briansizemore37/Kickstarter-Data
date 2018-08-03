library(tree)
#tree with just past variables---------------------------------------------------------
mydatapv=kickstarter_pv
#summary(kickstarter_pv)
mydata=mydatapv
set.seed(123)
#find number of rows and columns
dimensions=dim(mydata)
#isolate rows
n=dimensions[1]
#randomely select 70% on indexes
samples = sample(1:n, (2/3)*n,rep=F)
#create train data with 70%
mydata.train = mydata[samples,]
#create test data with the other 30%
mydata.test = mydata[-samples,]
#build initial tree
kick_treepv=tree(pledged_usd~.,mydata)
#get summary of results
summary(kick_treepv)
plot(kick_treepv);text(kick_treepv)
#use cross validation tree function
cv_treepv= cv.tree(kick_treepv, FUN=prune.tree, K=10)
cv_treepv 
plot(cv_treepv$size, cv_treepv$dev)

#prun to the best option of 5
kick_prunpv=prune.tree(kick_treepv, best =5)
kick_prunpv
plot(kick_prunpv);text(kick_prunpv)

tree.predpv=predict(kick_prunpv, mydata.test)
test_actuals=mydata.test$pledged_usd
msepv=mean((tree.predpv-test_actuals)^2)
r2pv=1-msepv/var(test_actuals)
msepv
sqrt(msepv) #only past variables rmse=54,482
r2pv
#------------------------------------------------------------------------------------
#Tree with past and present variables------------------------------------------------
mydatapvfv=kickstarter_pv_fv
mydata=mydatapvfv
set.seed(123)
#find number of rows and columns
dimensions=dim(mydata)
#isolate rows
n=dimensions[1]
#randomely select 70% on indexes
samples = sample(1:n, (2/3)*n,rep=F)
#create train data with 70%
mydata.train = mydata[samples,]
#create test data with the other 30%
mydata.test = mydata[-samples,]
#build initial tree
kick_treepvfv=tree(pledged_usd~.,mydata)
#get summary of results
summary(kick_treepvfv)
plot(kick_treepvfv);text(kick_treepvfv)
#use cross validation tree function
cv_treepvfv= cv.tree(kick_treepvfv, FUN=prune.tree, K=10)
cv_treepvfv
plot(cv_treepvfv$size, cv_treepvfv$dev)
#prun to the best option of 7
kick_prunpvfv=prune.tree(kick_treepvfv, best =9)
kick_prunpvfv
plot(kick_prunpvfv);text(kick_prunpvfv)

tree.predpvfv=predict(kick_prunpvfv, mydata.test)
test_actuals=mydata.test$pledged_usd
msepvfv=mean((tree.predpvfv-test_actuals)^2)
r2pvfv=1-msepvfv/var(test_actuals)
msepvfv
sqrt(msepvfv) #only past variables rmse=32,726
r2pvfv

#------------------------------------------------------------------------------------
#Tree given pledged<X
testtree=sqldf("select * from kickstarter_pv where pledged_usd<=100000")
mydata100=testtree
mydata=mydata100
set.seed(123)
#find number of rows and columns
dimensions=dim(mydata)
#isolate rows
n=dimensions[1]
#randomely select 70% on indexes
samples = sample(1:n, (2/3)*n,rep=F)
#create train data with 70%
mydata.train = mydata[samples,]
#create test data with the other 30%
mydata.test = mydata[-samples,]
#build initial tree
kick_treepvfv100=tree(pledged_usd~.,mydata)
#get summary of results
summary(kick_treepvfv100)
plot(kick_treepvfv100);text(kick_treepvfv100)
#use cross validation tree function
cv_treepvfv100= cv.tree(kick_treepvfv100, FUN=prune.tree, K=10)
cv_treepvfv100
plot(cv_treepvfv100$size, cv_treepvfv100$dev, main="Tree Size Tuning", xlab="Size", ylab="Cross Validation Dev")
#prun to the best option of 11
options(scipen = TRUE)
kick_prunpvfv100=prune.tree(kick_treepvfv100, best= 8)
kick_prunpvfv100
plot(kick_prunpvfv100);text(kick_prunpvfv100)

tree.predpvfv100=predict(kick_prunpvfv100, mydata.test)
test_actuals=mydata.test$pledged_usd
msepvfv100=mean((tree.predpvfv100-test_actuals)^2)
r2pvfv100=1-msepvfv100/var(test_actuals)
msepvfv100
sqrt(msepvfv100) #only past variables rmse=8,182
r2pvfv100

#------------------------------------------------------------------------------------
#Tree given pledged<X just past variables
testtree=sqldf("select * from kickstarter_pv where pledged_usd<=100000")
mydata100=testtree
mydata=mydata100
set.seed(123)
#find number of rows and columns
dimensions=dim(mydata)
#isolate rows
n=dimensions[1]
#randomely select 70% on indexes
samples = sample(1:n, (2/3)*n,rep=F)
#create train data with 70%
mydata.train = mydata[samples,]
#create test data with the other 30%
mydata.test = mydata[-samples,]
#build initial tree
kick_treepvfv100=tree(pledged_usd~.,mydata)
#get summary of results
summary(kick_treepvfv100)
plot(kick_treepvfv100);text(kick_treepvfv100)
#use cross validation tree function
cv_treepvfv100= cv.tree(kick_treepvfv100, FUN=prune.tree, K=10)
cv_treepvfv100
plot(cv_treepvfv100$size, cv_treepvfv100$dev)
#prun to the best option of 11
kick_prunpvfv100=prune.tree(kick_treepvfv100, best= 8)
kick_prunpvfv100
plot(kick_prunpvfv100);text(kick_prunpvfv100)

tree.predpvfv100=predict(kick_prunpvfv100, mydata.test)
test_actuals=mydata.test$pledged_usd
msepvfv100=mean((tree.predpvfv100-test_actuals)^2)
r2pvfv100=1-msepvfv100/var(test_actuals)
msepvfv100
sqrt(msepvfv100) #only past variables rmse=9,666
r2pvfv100

ci_tree=predict(kick_prunpvfv100, mydata.test, interval="confidence", level=.90)
ci_tree

plot(tree.predpvfv100, mydata.test$pledged_usd, xlab="Prediction", ylab="Actual", main="Tree Actual vs Predicted" )
plot( mydata.test$pledged_usd,tree.predpvfv100 )
plot(mydata.test$pledged_usd,(tree.predpvfv100-mydata.test$pledged_usd)^2, main="Actuals by Residuals^2")
plot(mydata.test$pledged_usd,(mydata.test$pledged_usd-tree.predpvfv100), main="Actuals vs Residuals", xlab="Actuals", ylab="Actual - Predicted")
plot(tree.predpvfv100,(mydata.test$pledged_usd-tree.predpvfv100), main="Fitted vs Residuals", xlab="Fitted", ylab="Residuals")

#Random Forest----------------------------------------------------------------
library(randomForest)
testtree=sqldf("select * from kickstarter_pv where pledged_usd<=100000")
mydata100=testtree
mydata=mydata100
dim(mydata)
y=mydata$pledged_usd
x=subset(mydata, select = -c(pledged_usd))

tuner=tuneRF(x, y, ntreeTry=25, stepFactor=2, plot=TRUE)

tuner
plot(tuner, main="Error by MTRY")
rf_500=randomForest(pledged_usd~., data=mydata,subset=samples, mtry=20, importance=TRUE, ntree=500)
sqrt(46085384)
plot(rf_500, main="Error by Trees")
rf_50=randomForest(pledged_usd~., data=mydata,subset=samples, mtry=20, importance=TRUE, ntree=50)
predictions_forrest50 = predict(rf_50,newdata=mydata.test)
rmse_forest50 = sqrt(mean((mydata.test$pledged_usd-predictions_forrest50)^2))
rmse_forest50 

varImpPlot(rf_50, type=1, n.var=10, sort=TRUE, main="Random Forest Importance")

plot(mydata.test$pledged_usd,predictions_forrest50, xlim=c(0,100000), ylim=c(0,100000), main= "Forest Actuals vs Predictions", xlab="Actuals", ylab="Predictions")
plot(predictions_forrest50,(mydata.test$pledged_usd-predictions_forrest50), xlim=c(0,100000), ylim=c(-50000,50000), main= "Forest Predicted vs Residuals", xlab="Predicted", ylab="Residuals")
