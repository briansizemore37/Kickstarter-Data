

library(MASS)
k_fold_lm= function(data, model, yname, k, seed) {
  n=nrow(data)
  set.seed(seed)
  datay=data.frame(data[,yname])
  
  f=ceiling(n/k)
  s=sample(rep(1:k, f), n)
  
  cv_lm=NULL
  cv_r2=NULL
  for (i in 1:k) {
    test.index=seq_len(n)[s==i]
    train.index=seq_len(n)[s!=i]
    
    lm.fit= lm(model, data=data[train.index,])
    lm.y=data[test.index, yname]
    lm.predy=predict(lm.fit, data[test.index,])
    mse=mean((lm.predy-lm.y)^2)
    r2=1-mse/var(lm.y)
    cv_lm=c(cv_lm,mse)
    cv_r2=c(cv_r2,r2)
  }
  
  
  
  list(call=model, k=k, lm_mse=mean(cv_lm), lm_r2=mean(cv_r2), seed=seed)
}

k_fold_lm_log= function(data, model, yname, k, seed) {
  n=nrow(data)
  set.seed(seed)
  datay=data.frame(data[,yname])
  
  f=ceiling(n/k)
  s=sample(rep(1:k, f), n)
  
  cv_lm=NULL
  cv_r2=NULL
  for (i in 1:k) {
    test.index=seq_len(n)[s==i]
    train.index=seq_len(n)[s!=i]
    
    lm.fit= lm(model, data=data[train.index,])
    lm.y=data[test.index, yname]
    lm.predy=predict(lm.fit, data[test.index,])
    lm.predy_nonlog=exp(lm.predy)
    lm.y_nonlog=exp(lm.y)
    #mse=mean((lm.predy-lm.y)^2)
    mse=mean((lm.predy_nonlog-lm.y_nonlog)^2)
    r2=1-mse/var(lm.y_nonlog)
    cv_lm=c(cv_lm,mse)
    cv_r2=c(cv_r2,r2)
  }
  
  
  
  list(call=model, k=k, lm_mse=mean(cv_lm), lm_r2=mean(cv_r2), seed=seed)
}

#only past predictors------------------------------------------------------------------------------------------------
kickstarter_pv=kickstarter[,c('Currency', 'Top.Category', 'Updates', 'Rewards', 'goal_usd', 'pledged_usd', 'Duration.in.Days', 'Facebook.Connected', 'Facebook.Friends', 'Has.Video', 'Creator.....Projects.Created', 'Creator.....Projects.Backed', 'X..Videos', 'X..Images','X..Words..Description.', 'X..Words..Risks.and.Challenges.','X..FAQs', 'D.Anger', 'D.Anticipation', 'D.Disgust', 'R.Fear', 'R.Sadness', 'R.Surprise', 'R.Trust', 'R.Positive', 'R.Negative', 'R.Joy', 'R.syuzhet.score', 'R.syuzhet.sum', 'R.syuzhet.mean')]

#----------------------------------------------------------------LOG LINEAR ANALYSIS--------------------------------
#transform response to log
kickstarter_log=kickstarter_pv
kickstarter_log$pledged_usd=log(kickstarter_log$pledged_usd)

#this data has all past variables with a log response
Linear_Model_Past_Variables_LOG_Response1=k_fold_lm_log(data=kickstarter_log,model=pledged_usd~., yname="pledged_usd", k=10, seed=123)
Linear_Model_Past_Variables_LOG_Response1
sqrt(1772670000000000) #all RMSE=42MM

#transform all variables that are right skew
kickstarter_log_all_20k=kickstarter_pv 
kickstarter_log_all_20k$pledged_usd=log(kickstarter_log_all_20k$pledged_usd)
kickstarter_log_all_20k$Updates=sqrt(kickstarter_log_all_20k$Updates)
kickstarter_log_all_20k$Rewards=log(kickstarter_log_all_20k$Rewards)
kickstarter_log_all_20k$goal_usd=log(kickstarter_log_all_20k$goal_usd)
kickstarter_log_all_20k$Facebook.Friends=sqrt(kickstarter_log_all_20k$Facebook.Friends)
kickstarter_log_all_20k$Creator.....Projects.Created=log(kickstarter_log_all_20k$Creator.....Projects.Created)
kickstarter_log_all_20k$Creator.....Projects.Backed=sqrt(kickstarter_log_all_20k$Creator.....Projects.Backed)
kickstarter_log_all_20k$X..Videos=sqrt(kickstarter_log_all_20k$X..Videos)
kickstarter_log_all_20k$X..Images=sqrt(kickstarter_log_all_20k$X..Images)
kickstarter_log_all_20k$X..Words..Description.=log(kickstarter_log_all_20k$X..Words..Description.)
kickstarter_log_all_20k$X..Words..Risks.and.Challenges.=sqrt(kickstarter_log_all_20k$X..Words..Risks.and.Challenges.)
kickstarter_log_all_20k$X..FAQs=sqrt(kickstarter_log_all_20k$X..FAQs)
kickstarter_log_all_20k$D.Anger=sqrt(kickstarter_log_all_20k$D.Anger)
kickstarter_log_all_20k$D.Anticipation=sqrt(kickstarter_log_all_20k$D.Anticipation)
kickstarter_log_all_20k$D.Disgust=sqrt(kickstarter_log_all_20k$D.Disgust)
kickstarter_log_all_20k$R.Fear=sqrt(kickstarter_log_all_20k$R.Fear)
kickstarter_log_all_20k$R.Sadness=sqrt(kickstarter_log_all_20k$R.Sadness)
kickstarter_log_all_20k$R.Surprise=sqrt(kickstarter_log_all_20k$R.Surprise)
kickstarter_log_all_20k$R.Trust=log(kickstarter_log_all_20k$R.Trust)
kickstarter_log_all_20k$R.Positive=log(kickstarter_log_all_20k$R.Positive)
kickstarter_log_all_20k$R.Negative=sqrt(kickstarter_log_all_20k$R.Negative)
kickstarter_log_all_20k$R.Joy=sqrt(kickstarter_log_all_20k$R.Joy)

#Graphs for presentation
histogram=sqldf("select * from kickstarter where pledged_usd<100000")
hist(histogram$pledged_usd, breaks = "FD", xlim=c(0,20000), main="Response Frequency Distribution")
hist(kickstarter_log_all_20k$pledged_usd, main="Log of Response")
hist(kickstarter$Facebook.Shares, breaks = "FD", xlim=c(0,1000), main="Facebook Shares Frequency Distribution")
hist(log(kickstarter$Facebook.Shares), main="Log of Predictor", breaks="FD", xlim=c(0,10))
summary(kickstarter_log_all_20k$Backers)
#this data has all past variables with a log response and log predictors
Linear_Model_Past_Variables_LOG_Response_ALL1=k_fold_lm_log(data=kickstarter_log_all_20k,model=pledged_usd~., yname="pledged_usd", k=10, seed=123)
Linear_Model_Past_Variables_LOG_Response_ALL1
sqrt(8318758301)#RMSE 91,207



#-----------------------------------take the best linear model, refine and discover------------

mydata=kickstarter_log_all_20k
summary(mydata)
set.seed(123)
dimensions=dim(mydata)
n=dimensions[1]
samples = sample(1:n, (2/3)*n,rep=F)
mydata.train = mydata[samples,]
mydata.test = mydata[-samples,]
linear_m=lm(data=mydata.train, formula=pledged_usd~Top.Category+Updates+Rewards+goal_usd+
              Facebook.Connected+Facebook.Friends+Has.Video+Creator.....Projects.Created+Creator.....Projects.Backed+X..Images+X..FAQs+D.Anger+D.Anticipation+R.Sadness)
summary(linear_m)
lm.pred=predict(linear_m, mydata.test)
test_actuals=mydata.test$pledged_usd
mse=mean((exp(lm.pred)-exp(test_actuals))^2)
r2=1-mse/var(exp(test_actuals))
mse
sqrt(mse) #RMSE 69011
options(sic)
plot(exp(test_actuals), abs(exp(test_actuals)-exp(lm.pred)), xlim=c(0,1000000), ylim=c(0,1000000), main="Actuals vs Absolute Residuals", xlab="Actuals", ylab="Absolute Residuals")
plot(exp(test_actuals), exp(test_actuals)-exp(lm.pred), xlim=c(0,100000), ylim=c(-100000,100000), main="Linear Model (with Log) - Residuals vs Actuals", xlab="Actuals", ylab="Actual - Predicted")
plot(exp(lm.pred), exp(test_actuals)-exp(lm.pred), xlim=c(0,1000000), ylim=c(-500000,500000), main="Actuals vs Absolute Residuals", xlab="Actuals", ylab="Absolute Residuals")
plot(linear_m)
plot(resid(linear_m), exp(lm.pred))




#NOT LOG----------------------------------------------------------------------------------------------------------
Linear_Model_Past_Variables=k_fold_lm(data=kickstarter_pv,model=pledged_usd~., yname="pledged_usd", k=10, seed=123)
Linear_Model_Past_Variables
sqrt(6277847928)
kickstarterp_100=sqldf("select * from kickstarter_pv where pledged_usd<=100000")
Linear_Model_Past_Variables100=k_fold_lm(data=kickstarterp_100,model=pledged_usd~., yname="pledged_usd", k=10, seed=123)
Linear_Model_Past_Variables100
sqrt(106158075)#10303 for <100K
kickstarterp_50=sqldf("select * from kickstarter_pv where pledged_usd<=50000")
Linear_Model_Past_Variables50=k_fold_lm(data=kickstarterp_50,model=pledged_usd~., yname="pledged_usd", k=10, seed=123)
Linear_Model_Past_Variables50
sqrt(45106770)#6716 for <50K
kickstarterp_20=sqldf("select * from kickstarter_pv where pledged_usd<=20000")
Linear_Model_Past_Variables20=k_fold_lm(data=kickstarterp_20,model=pledged_usd~., yname="pledged_usd", k=10, seed=123)
Linear_Model_Past_Variables20
sqrt(13227725)#3636 for <20K
#this dataset does not include Category and Location because there are too many options.
kickstarter_pv_fv=kickstarter[,c('Currency', 'Top.Category', 'Updates', 'Comments', 'Rewards', 'goal_usd', 'pledged_usd', 'Duration.in.Days', 'Facebook.Connected', 'Facebook.Friends', 'Facebook.Shares', 'Has.Video', 'Creator.....Projects.Created', 'Creator.....Projects.Backed', 'X..Videos', 'X..Images','X..Words..Description.', 'X..Words..Risks.and.Challenges.','X..FAQs', 'D.Anger', 'D.Anticipation', 'D.Disgust', 'R.Fear', 'R.Sadness', 'R.Surprise', 'R.Trust', 'R.Positive', 'R.Negative', 'R.Joy', 'R.syuzhet.score', 'R.syuzhet.sum', 'R.syuzhet.mean')]
#model run agains full dataset
Linear_Model_Past_Variables_Future_Variables=k_fold_lm(data=kickstarter_pv_fv,model=pledged_usd~., yname="pledged_usd", k=10, seed=123)
Linear_Model_Past_Variables_Future_Variables
sqrt(4465981134)#66,828 for all
kickstarter_100=sqldf("select * from kickstarter_pv_fv where pledged_usd<=100000")
Linear_Model_Past_Variables_Future_Variables100=k_fold_lm(data=kickstarter_100,model=pledged_usd~., yname="pledged_usd", k=10, seed=123)
Linear_Model_Past_Variables_Future_Variables100
sqrt(87884922)#9374 for <100K
kickstarter_50=sqldf("select * from kickstarter_pv_fv where pledged_usd<=50000")
Linear_Model_Past_Variables_Future_Variables50=k_fold_lm(data=kickstarter_50,model=pledged_usd~., yname="pledged_usd", k=10, seed=123)
Linear_Model_Past_Variables_Future_Variables50
sqrt(39628388)#6295 for <50K
kickstarter_20=sqldf("select * from kickstarter_pv_fv where pledged_usd<=20000")
Linear_Model_Past_Variables_Future_Variables20=k_fold_lm(data=kickstarter_20,model=pledged_usd~., yname="pledged_usd", k=10, seed=123)
Linear_Model_Past_Variables_Future_Variables20
sqrt(11266503)#3356 for <20K
#----------------------------------------Analyze Best Result----------------------
mydata=kickstarterp_100
summary(mydata)
set.seed(123)
dimensions=dim(mydata)
n=dimensions[1]
samples = sample(1:n, (2/3)*n,rep=F)
mydata.train = mydata[samples,]
mydata.test = mydata[-samples,]
linear_m=lm(data=mydata.train, formula=pledged_usd~.)
linear_m=lm(data=mydata.train, formula=pledged_usd~Top.Category+Updates+Rewards+Facebook.Friends+Has.Video+Creator.....Projects.Backed+X..Images+X..FAQs+D.Anticipation+R.Sadness)
summary(linear_m)
lm.pred=predict(linear_m, mydata.test)
test_actuals=mydata.test$pledged_usd
mse=mean((lm.pred-test_actuals)^2)
r2=1-mse/var(test_actuals)
mse
sqrt(mse) #RMSE 10,363
options(scipen=999)
plot(test_actuals, lm.pred, xlim=c(0,100000), ylim=c(0,100000), main="Linear Model Actuals vs Predicted", xlab="Actuals", ylab="Predictions")
plot(test_actuals, test_actuals-lm.pred, xlim=c(0,100000), ylim=c(-50000,50000), main="Linear Model Actuals vs Residuals", xlab="Actuals", ylab="Actuals - Predictions")
plot(linear_m, which=3)


hist(resid(linear_m), main="Residual Distribution", xlim=c(-10000,10000), breaks="FD")

#QQ Plot
lm.stdres = rstandard(linear_m)
qqnorm(lm.stdres, ylab="Standardized Residuals", xlab="Normal Scores", main="Normal QQ") 
qqline(lm.stdres, col="Red")

plot(lm.pred, test_actuals-lm.pred, xlim=c(0,100000), ylim=c(-50000,50000), main="Linear Model Fitted vs Residuals", xlab="Actuals", ylab="Residuals")


boxplot(kickstarter$Pledged)
summary(kickstarter$Pledged)
