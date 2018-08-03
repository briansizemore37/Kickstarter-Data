load(file="C:/Users/brian/Downloads/KickstarterAnalysis.Rda")

kickstarter=Kickstarter.Analysis
numberic_variables=kickstarter[,c(  'Updates', 'Comments', 'Rewards', 'Goal', 'Pledged', 'Duration.in.Days', 'Facebook.Friends', 'Creator.....Projects.Created', 'Creator.....Projects.Backed', 'X..Videos', 'X..Images','X..Words..Description.', 'X..Words..Risks.and.Challenges.','X..FAQs')]
#, 'D.Anger', 'D.Anticipation', 'D.Disgust', 'R.Fear', 'R.Sadness', 'R.Surprise', 'R.Trust', 'R.Positive', 'R.Negative', 'R.Joy', 'R.syuzhet.score', 'R.syuzhet.sum', 'R.syuzhet.mean'
cor(numberic_variables)

cor(kickstarter)
#Transform and reformat data
#dollar values are in different currencies. Going to normalize the dollar values to usd with approximate 2014 estimates
AUD_Conversion=.85
CAD_Conversion=.90
EUR_Conversion=1.35
GBP_Conversion=1.65
NZD_Conversion=.82

library(sqldf)
kickstarter=sqldf("select a.*
                      ,case when a.currency='AUD' then pledged*.85 when a.currency='CAD' then pledged*.90 when currency='EUR' then pledged*1.35 when currency='GBP' then pledged*1.65 when currency='NZD' then pledged*.82 else pledged end as pledged_usd
                      ,case when a.currency='AUD' then goal*.85 when a.currency='CAD' then goal*.90 when currency='EUR' then goal*1.35 when currency='GBP' then goal*1.65 when currency='NZD' then goal*.82 else goal end as goal_usd
                      from kickstarter a ")

#enter 0's for ordinal data which make sense
kickstarter$Facebook.Friends=ifelse(is.na(kickstarter$Facebook.Friends), 0, kickstarter$Facebook.Friends)
kickstarter$Creator.....Projects.Backed=ifelse(is.na(kickstarter$Creator.....Projects.Backed), 0, kickstarter$Creator.....Projects.Backed)
kickstarter$X..Videos=ifelse(is.na(kickstarter$X..Videos), 0, kickstarter$X..Videos)
kickstarter$X..Words..Risks.and.Challenges.=ifelse(is.na(kickstarter$X..Words..Risks.and.Challenges.), 0, kickstarter$X..Words..Risks.and.Challenges.)

#impute the median value for the sentiment data
#since there are only a few NA values for the sentiment data, we are imputing the median for the NAs
meds=median(kickstarter$D.Anger, na.rm=TRUE)
kickstarter$D.Anger=ifelse(is.na(kickstarter$D.Anger), meds, kickstarter$D.Anger)
meds=median(kickstarter$D.Anticipation, na.rm=TRUE)
kickstarter$D.Anticipation=ifelse(is.na(kickstarter$D.Anticipation), meds, kickstarter$D.Anticipation)
meds=median(kickstarter$D.Disgust, na.rm=TRUE)
kickstarter$D.Disgust=ifelse(is.na(kickstarter$D.Disgust), meds, kickstarter$D.Disgust)
meds=median(kickstarter$R.Fear, na.rm=TRUE)
kickstarter$R.Fear=ifelse(is.na(kickstarter$R.Fear), meds, kickstarter$R.Fear)
meds=median(kickstarter$R.Sadness, na.rm=TRUE)
kickstarter$R.Sadness=ifelse(is.na(kickstarter$R.Sadness), meds, kickstarter$R.Sadness)
meds=median(kickstarter$R.Surprise, na.rm=TRUE)
kickstarter$R.Surprise=ifelse(is.na(kickstarter$R.Surprise), meds, kickstarter$R.Surprise)
meds=median(kickstarter$R.Trust, na.rm=TRUE)
kickstarter$R.Trust=ifelse(is.na(kickstarter$R.Trust), meds, kickstarter$R.Trust)
meds=median(kickstarter$R.Positive, na.rm=TRUE)
kickstarter$R.Positive=ifelse(is.na(kickstarter$R.Positive), meds, kickstarter$R.Positive)
meds=median(kickstarter$R.Negative, na.rm=TRUE)
kickstarter$R.Negative=ifelse(is.na(kickstarter$R.Negative), meds, kickstarter$R.Negative)
meds=median(kickstarter$R.Joy, na.rm=TRUE)
kickstarter$R.Joy=ifelse(is.na(kickstarter$R.Joy), meds, kickstarter$R.Joy)
meds=median(kickstarter$R.syuzhet.mean, na.rm=TRUE)
kickstarter$R.syuzhet.mean=ifelse(is.na(kickstarter$R.syuzhet.mean), meds, kickstarter$R.syuzhet.mean)
meds=median(kickstarter$R.syuzhet.score, na.rm=TRUE)
kickstarter$R.syuzhet.score=ifelse(is.na(kickstarter$R.syuzhet.score), meds, kickstarter$R.syuzhet.score)
meds=median(kickstarter$R.syuzhet.sum, na.rm=TRUE)
kickstarter$R.syuzhet.sum=ifelse(is.na(kickstarter$R.syuzhet.sum), meds, kickstarter$R.syuzhet.sum)

# remove the 69 records with multiple ids
kickstarter=sqldf("select a.* from kickstarter a left join (
            select id, count(*) as counter from kickstarter group by 1 having count(*)>1) b on a.id=b.id
            where b.id is null
            ")


######################################################################################################


#Kfold function to use with linear models
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
    lm.predy=exp(lm.predy)
    lm.y=exp(lm.y)
    #mse=mean((lm.predy-lm.y)^2)
    mse=mean((lm.predy-lm.y)^2)
    r2=1-mse/var(lm.y)
    cv_lm=c(cv_lm,mse)
    cv_r2=c(cv_r2,r2)
  }
  
  
  
  list(call=model, k=k, lm_mse=mean(cv_lm), lm_r2=mean(cv_r2), seed=seed)
}

#this dataset does not include Category and Location because there are too many options.
kickstarter_pv_fv=kickstarter[,c('Currency', 'Top.Category', 'Updates', 'Comments', 'Rewards', 'goal_usd', 'pledged_usd', 'Duration.in.Days', 'Facebook.Connected', 'Facebook.Friends', 'Facebook.Shares', 'Has.Video', 'Creator.....Projects.Created', 'Creator.....Projects.Backed', 'X..Videos', 'X..Images','X..Words..Description.', 'X..Words..Risks.and.Challenges.','X..FAQs', 'D.Anger', 'D.Anticipation', 'D.Disgust', 'R.Fear', 'R.Sadness', 'R.Surprise', 'R.Trust', 'R.Positive', 'R.Negative', 'R.Joy', 'R.syuzhet.score', 'R.syuzhet.sum', 'R.syuzhet.mean')]
#model run agains full dataset
Linear_Model_Past_Variables_Future_Variables=k_fold_lm(data=kickstarter_pv_fv,model=pledged_usd~., yname="pledged_usd", k=10, seed=123)
Linear_Model_Past_Variables_Future_Variables
sqrt(4465981134)

#model run against full dataset with interactions-this is now in the negatives..not sure how
Linear_Model_Past_Variables_Future_Variables_INT=k_fold_lm(data=kickstarter_pv_fv,model=pledged_usd~.^2, yname="pledged_usd", k=10, seed=123)
Linear_Model_Past_Variables_Future_Variables_INT

#this dataset does not include future predictors-notice it goes to shit (in the negatives)
kickstarter_pv=kickstarter[,c('Currency', 'Top.Category', 'Updates', 'Rewards', 'goal_usd', 'pledged_usd', 'Duration.in.Days', 'Facebook.Connected', 'Facebook.Friends', 'Has.Video', 'Creator.....Projects.Created', 'Creator.....Projects.Backed', 'X..Videos', 'X..Images','X..Words..Description.', 'X..Words..Risks.and.Challenges.','X..FAQs', 'D.Anger', 'D.Anticipation', 'D.Disgust', 'R.Fear', 'R.Sadness', 'R.Surprise', 'R.Trust', 'R.Positive', 'R.Negative', 'R.Joy', 'R.syuzhet.score', 'R.syuzhet.sum', 'R.syuzhet.mean')]
Linear_Model_Past_Variables=k_fold_lm(data=kickstarter_pv,model=pledged_usd~., yname="pledged_usd", k=10, seed=123)
Linear_Model_Past_Variables
sqrt(6277847928)

#transform response to log
kickstarter_log=kickstarter_pv
kickstarter_log$pledged_usd=log(kickstarter_log$pledged_usd)
#summary(kickstarter_pv$pledged_usd)
#summary(kickstarter_log$pledged_usd)
#cbind(kickstarter_pv$pledged_usd,kickstarter_log$pledged_usd)

#this data has all past variables with a log response
Linear_Model_Past_Variables_LOG_Response=k_fold_lm(data=kickstarter_log,model=pledged_usd~., yname="pledged_usd", k=10, seed=123)
Linear_Model_Past_Variables_LOG_Response1=k_fold_lm_log(data=kickstarter_log,model=pledged_usd~., yname="pledged_usd", k=10, seed=123)
Linear_Model_Past_Variables_LOG_Response
Linear_Model_Past_Variables_LOG_Response1
exp(3.673725)
sqrt(1772670000000000)

#transform the other skewed variables to log
kickstarter_log_all_20k=kickstarter_log 
kickstarter_log_all_20k$Updates=sqrt(kickstarter_log_all_20k$Updates)
#hist((kickstarter_log$Updates)^(3/4))
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



#this data has all past variables with a log response and log predictors
Linear_Model_Past_Variables_LOG_Response_ALL=k_fold_lm(data=kickstarter_log_all_20k,model=pledged_usd~., yname="pledged_usd", k=10, seed=123)
Linear_Model_Past_Variables_LOG_Response_ALL1=k_fold_lm_log(data=kickstarter_log_all_20k,model=pledged_usd~., yname="pledged_usd", k=10, seed=123)
Linear_Model_Past_Variables_LOG_Response_ALL
Linear_Model_Past_Variables_LOG_Response_ALL1
sqrt(8318758301)

#removing the outliers does slightly improve the mse and r2
#remove outliers outside 1.5IQR based on the log of the pledged amount and re-run best model
outlier_values=boxplot.stats(kickstarter_log_all_20k$pledged_usd)$out
#i can see that anything with a log(pledged) value <=0.6931472 or >=13.5596139 is an outlier by IQR
sort(outlier_values)
#this did not bring additional value to the model
no_outliers=sqldf("select * from kickstarter_log_all_20k where pledged_usd>0.6931472 and pledged_usd<13.5596139")

Linear_Model_Past_Variables_LOG_Response_ALL_No_OUT=k_fold_lm(data=no_outliers,model=pledged_usd~., yname="pledged_usd", k=10, seed=123)
Linear_Model_Past_Variables_LOG_Response_ALL_No_OUT1=k_fold_lm_log(data=no_outliers,model=pledged_usd~., yname="pledged_usd", k=10, seed=123)
Linear_Model_Past_Variables_LOG_Response_ALL_No_OUT
Linear_Model_Past_Variables_LOG_Response_ALL_No_OUT1
sqrt(2411933028)




#limit the all past variables model wtih log response and log predictors to only have pledged <=20K

kickstarter_log_20=kickstarter_pv
kickstarter_log_20=sqldf("select * from kickstarter_log_20 where pledged_usd<=100000")
#kickstarter_log_20$pledged_usd=ifelse(kickstarter_log_20$pledged_usd>20000, 20000, kickstarter_log_20$pledged_usd)
#hist((kickstarter_log_20$pledged_usd))
kickstarter_log_20$pledged_usd=log(kickstarter_log_20$pledged_usd)

kickstarter_log_all_20_me=kickstarter_log_20
kickstarter_log_all_20_me$Updates=sqrt(kickstarter_log_all_20_me$Updates)
kickstarter_log_all_20_me$Rewards=log(kickstarter_log_all_20_me$Rewards)
kickstarter_log_all_20_me$goal_usd=log(kickstarter_log_all_20_me$goal_usd)
kickstarter_log_all_20_me$Facebook.Friends=sqrt(kickstarter_log_all_20_me$Facebook.Friends)
kickstarter_log_all_20_me$Creator.....Projects.Created=log(kickstarter_log_all_20_me$Creator.....Projects.Created)
kickstarter_log_all_20_me$Creator.....Projects.Backed=sqrt(kickstarter_log_all_20_me$Creator.....Projects.Backed)
kickstarter_log_all_20_me$X..Videos=sqrt(kickstarter_log_all_20_me$X..Videos)
kickstarter_log_all_20_me$X..Images=sqrt(kickstarter_log_all_20_me$X..Images)
kickstarter_log_all_20_me$X..Words..Description.=log(kickstarter_log_all_20_me$X..Words..Description.)
kickstarter_log_all_20_me$X..Words..Risks.and.Challenges.=sqrt(kickstarter_log_all_20_me$X..Words..Risks.and.Challenges.)
kickstarter_log_all_20_me$X..FAQs=sqrt(kickstarter_log_all_20_me$X..FAQs)
kickstarter_log_all_20_me$D.Anger=sqrt(kickstarter_log_all_20_me$D.Anger)
kickstarter_log_all_20_me$D.Anticipation=sqrt(kickstarter_log_all_20_me$D.Anticipation)
kickstarter_log_all_20_me$D.Disgust=sqrt(kickstarter_log_all_20_me$D.Disgust)
kickstarter_log_all_20_me$R.Fear=sqrt(kickstarter_log_all_20_me$R.Fear)
kickstarter_log_all_20_me$R.Sadness=sqrt(kickstarter_log_all_20_me$R.Sadness)
kickstarter_log_all_20_me$R.Surprise=sqrt(kickstarter_log_all_20_me$R.Surprise)
kickstarter_log_all_20_me$R.Trust=log(kickstarter_log_all_20_me$R.Trust)
kickstarter_log_all_20_me$R.Positive=log(kickstarter_log_all_20_me$R.Positive)
kickstarter_log_all_20_me$R.Negative=sqrt(kickstarter_log_all_20_me$R.Negative)
kickstarter_log_all_20_me$R.Joy=sqrt(kickstarter_log_all_20_me$R.Joy)

Linear_Model_Past_Variables_LOG_Response_ALL_20=k_fold_lm(data=kickstarter_log_all_20_me,model=pledged_usd~., yname="pledged_usd", k=10, seed=123)
Linear_Model_Past_Variables_LOG_Response_ALL_201=k_fold_lm_log(data=kickstarter_log_all_20_me,model=pledged_usd~., yname="pledged_usd", k=10, seed=123)
Linear_Model_Past_Variables_LOG_Response_ALL_20
#data less than 20K - 3,636
sqrt(13227725)
#data less than 50K-6,716
sqrt(45106770)
#data less than 100K-10,303
sqrt(106158075)


#run model with transforms over predictors but not the response
#transform the other skewed variables to log
kickstarter_log_predictors=kickstarter_pv
kickstarter_log_predictors$Updates=sqrt(kickstarter_log_predictors$Updates)
kickstarter_log_predictors$Rewards=log(kickstarter_log_predictors$Rewards)
kickstarter_log_predictors$goal_usd=log(kickstarter_log_predictors$goal_usd)
kickstarter_log_predictors$Facebook.Friends=sqrt(kickstarter_log_predictors$Facebook.Friends)
kickstarter_log_predictors$Creator.....Projects.Created=log(kickstarter_log_predictors$Creator.....Projects.Created)
kickstarter_log_predictors$Creator.....Projects.Backed=sqrt(kickstarter_log_predictors$Creator.....Projects.Backed)
kickstarter_log_predictors$X..Videos=sqrt(kickstarter_log_predictors$X..Videos)
kickstarter_log_predictors$X..Images=sqrt(kickstarter_log_predictors$X..Images)
kickstarter_log_predictors$X..Words..Description.=log(kickstarter_log_predictors$X..Words..Description.)
kickstarter_log_predictors$X..Words..Risks.and.Challenges.=sqrt(kickstarter_log_predictors$X..Words..Risks.and.Challenges.)
kickstarter_log_predictors$X..FAQs=sqrt(kickstarter_log_predictors$X..FAQs)
kickstarter_log_predictors$D.Anger=sqrt(kickstarter_log_predictors$D.Anger)
kickstarter_log_predictors$D.Anticipation=sqrt(kickstarter_log_predictors$D.Anticipation)
kickstarter_log_predictors$D.Disgust=sqrt(kickstarter_log_predictors$D.Disgust)
kickstarter_log_predictors$R.Fear=sqrt(kickstarter_log_predictors$R.Fear)
kickstarter_log_predictors$R.Sadness=sqrt(kickstarter_log_predictors$R.Sadness)
kickstarter_log_predictors$R.Surprise=sqrt(kickstarter_log_predictors$R.Surprise)
kickstarter_log_predictors$R.Trust=log(kickstarter_log_predictors$R.Trust)
kickstarter_log_predictors$R.Positive=log(kickstarter_log_predictors$R.Positive)
kickstarter_log_predictors$R.Negative=sqrt(kickstarter_log_predictors$R.Negative)
kickstarter_log_predictors$R.Joy=sqrt(kickstarter_log_predictors$R.Joy)

Linear_Model_Past_Variables_LOG_predictor=k_fold_lm(data=kickstarter_log_predictors,model=pledged_usd~., yname="pledged_usd", k=10, seed=123)
Linear_Model_Past_Variables_LOG_predictor
sqrt(5824854104)

#limit dataset to pledged<20000 and run all log model
twenty_less=sqldf("select * from kickstarter_pv where pledged_usd<=40000 and pledged_usd>20000")
twenty_less$pledged_usd=log(twenty_less$pledged_usd)
#transform the other skewed variables to log
kickstarter_log_all_20k_20k=twenty_less 
kickstarter_log_all_20k$Updates=sqrt(kickstarter_log_all_20k$Updates)
#kickstarter_log_all_20k$Rewards=sqrt(kickstarter_log_all_20k$Rewards)
#kickstarter_log_all_20k$goal_usd=sqrt(kickstarter_log_all_20k$goal_usd)
kickstarter_log_all_20k$Facebook.Friends=sqrt(kickstarter_log_all_20k$Facebook.Friends)
#kickstarter_log_all_20k$Creator.....Projects.Created=log(kickstarter_log_all_20k$Creator.....Projects.Created)
kickstarter_log_all_20k$Creator.....Projects.Backed=sqrt(kickstarter_log_all_20k$Creator.....Projects.Backed)
kickstarter_log_all_20k$X..Videos=sqrt(kickstarter_log_all_20k$X..Videos)
kickstarter_log_all_20k$X..Images=sqrt(kickstarter_log_all_20k$X..Images)
#kickstarter_log_all_20k$X..Words..Description.=log(kickstarter_log_all_20k$X..Words..Description.)
kickstarter_log_all_20k$X..Words..Risks.and.Challenges.=sqrt(kickstarter_log_all_20k$X..Words..Risks.and.Challenges.)
kickstarter_log_all_20k$X..FAQs=sqrt(kickstarter_log_all_20k$X..FAQs)
kickstarter_log_all_20k$D.Anger=sqrt(kickstarter_log_all_20k$D.Anger)
kickstarter_log_all_20k$D.Anticipation=sqrt(kickstarter_log_all_20k$D.Anticipation)
kickstarter_log_all_20k$D.Disgust=sqrt(kickstarter_log_all_20k$D.Disgust)
kickstarter_log_all_20k$R.Fear=sqrt(kickstarter_log_all_20k$R.Fear)
kickstarter_log_all_20k$R.Sadness=sqrt(kickstarter_log_all_20k$R.Sadness)
kickstarter_log_all_20k$R.Surprise=sqrt(kickstarter_log_all_20k$R.Surprise)
#kickstarter_log_all_20k$R.Trust=log(kickstarter_log_all_20k$R.Trust)
#kickstarter_log_all_20k$R.Positive=sqrt(kickstarter_log_all_20k$R.Positive)
kickstarter_log_all_20k$R.Negative=sqrt(kickstarter_log_all_20k$R.Negative)
kickstarter_log_all_20k$R.Joy=sqrt(kickstarter_log_all_20k$R.Joy)

summary(kickstarter_log_all_20k)

Linear_Model_Past_Variables_LOG_all_20k=k_fold_lm(data=kickstarter_log_all_20k,model=pledged_usd~., yname="pledged_usd", k=10, seed=123)
Linear_Model_Past_Variables_LOG_all_20k









#-----------------------------------take the best linear model, refine and discover------------

#Separate data into test and train
#mydata=kickstarter_log_all_20k
mydata=no_outliers
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

#Linear_Model_Past_Variables_LOG_Response_ALL_No_OUT=k_fold_lm(data=no_outliers,model=pledged_usd~., yname="pledged_usd", k=10, seed=123)
#Linear_Model_Past_Variables_LOG_Response_ALL_No_OUT
linear_m=lm(data=mydata.train, formula=pledged_usd~.)
linear_m=lm(data=mydata.train, formula=pledged_usd~Top.Category+Updates+Rewards+goal_usd+Duration.in.Days+Facebook.Friends+Has.Video+Creator.....Projects.Backed+X..Images+X..FAQs+D.Anger+D.Anticipation+R.Sadness+R.Positive)
summary(linear_m)
lm.pred=predict(linear_m, mydata.test)
ci=predict(linear_m, mydata.test, interval="confidence", level=.99)
test_actuals=mydata.test$pledged_usd
mse=mean((lm.pred-test_actuals)^2)
r2=1-mse/var(test_actuals)
mse
r2
mean((exp(lm.pred)-exp(test_actuals))^2)
var(exp(test_actuals))
(mean((exp(lm.pred)-exp(test_actuals))^2))/(var(exp(test_actuals)))
cbind(test_actuals, exp(test_actuals), lm.pred, exp(lm.pred))
summary(lm.pred)
ci[,3]
?predict
dataset=data.frame(cbind(exp(ci[,1]), exp(ci[,2]), exp(ci[,3]), exp(test_actuals), exp(mydata.test$goal_usd)))
final_dataframe=sqldf("select X1 as fit, X2 as lower, X3 as upper, X4 as actual from dataset")

final_dataframe1=sqldf("select *, case when actual between lower and upper then 'YES' when actual<lower then 'HIGHPRED' when actual>lower then 'LOWPRED' else 'WTF' end as goodenough 
                       , fit-actual as diffy
                       from final_dataframe order by (fit-actual)")
sqldf("Select goodenough, count(*) as counter from final_dataframe1 group by 1")
sqldf("select * from final_dataframe1 where goodenough='YES'")

plot(exp(lm.pred), (exp(lm.pred)-exp(test_actuals)), xlim=c(0,20000), ylim=c(-20000,20000))
plot(density(resid(linear_m)))

exp(10)
exp(12)
#assumption that the mean of the residuals is 0
#mean(linear_m$residuals)
mean(resid(linear_m))
hist(resid(linear_m), main='Residual Distribution', xlab="residual")
mean(exp(resid(linear_m)))
cbind(resid(linear_m), exp(resid(linear_m)))
log(4000)
log(5000)-log(1000)
#plot(resid(linear_m))
#check for homosckedacity
#the residuals vs fitted and scale-location show homosckedacity
plot(linear_m)
#no correlation between residuals and predictors-the results below show correlations all close to 0 so assumption is passed
cor.test(mydata.train$Updates,linear_m$residuals)
cor.test(mydata.train$Rewards,linear_m$residuals)
cor.test(mydata.train$goal_usd,linear_m$residuals)
cor.test(mydata.train$Duration.in.Days,linear_m$residuals)
cor.test(mydata.train$Facebook.Friends,linear_m$residuals)
cor.test(mydata.train$Creator.....Projects.Backed,linear_m$residuals)
cor.test(mydata.train$X..Images,linear_m$residuals)
cor.test(mydata.train$X..FAQs,linear_m$residuals)
cor.test(mydata.train$D.Anger,linear_m$residuals)
cor.test(mydata.train$D.Anticipation,linear_m$residuals)
cor.test(mydata.train$R.Sadness,linear_m$residuals)

#no multicolinearity between predicotrs--corr does not show any correlation i am worried about
model_variables=mydata[,c(  'Updates', 'Rewards', 'goal_usd', 'pledged_usd', 'Duration.in.Days', 'Facebook.Friends', 'Creator.....Projects.Backed', 'X..Images','X..FAQs', 'D.Anger', 'R.Sadness')]
cor(model_variables)
#numberic_variables=kickstarter[,numeric_variables=c(  'Updates', 'Comments', 'Rewards', 'Goal', 'Pledged', 'Duration.in.Days', 'Facebook.Friends', 'Creator.....Projects.Created', 'Creator.....Projects.Backed', 'X..Videos', 'X..Images','X..Words..Description.', 'X..Words..Risks.and.Challenges.','X..FAQs', 'D.Anger', 'D.Anticipation', 'D.Disgust', 'R.Fear', 'R.Sadness', 'R.Surprise', 'R.Trust', 'R.Positive', 'R.Negative', 'R.Joy', 'R.syuzhet.score', 'R.syuzhet.sum', 'R.syuzhet.mean')]
#add future variables to the model to see how the tool would work

sellme=kickstarter_pv_fv
sellme$pledged_usd=log(sellme$pledged_usd)
sellme$Updates=sqrt(sellme$Updates)
sellme$Comments=sqrt(sellme$Comments)
sellme$Facebook.Shares=sqrt(sellme$Facebook.Shares)
#hist(log(sellme$Comments))
#hist(log(sellme$Facebook.Shares))
sellme$Rewards=log(sellme$Rewards)
sellme$goal_usd=log(sellme$goal_usd)
sellme$Facebook.Friends=sqrt(sellme$Facebook.Friends)
sellme$Creator.....Projects.Created=log(sellme$Creator.....Projects.Created)
sellme$Creator.....Projects.Backed=sqrt(sellme$Creator.....Projects.Backed)
sellme$X..Videos=sqrt(sellme$X..Videos)
sellme$X..Images=sqrt(sellme$X..Images)
sellme$X..Words..Description.=log(sellme$X..Words..Description.)
sellme$X..Words..Risks.and.Challenges.=sqrt(sellme$X..Words..Risks.and.Challenges.)
sellme$X..FAQs=sqrt(sellme$X..FAQs)
sellme$D.Anger=sqrt(sellme$D.Anger)
sellme$D.Anticipation=sqrt(sellme$D.Anticipation)
sellme$D.Disgust=sqrt(sellme$D.Disgust)
sellme$R.Fear=sqrt(sellme$R.Fear)
sellme$R.Sadness=sqrt(sellme$R.Sadness)
sellme$R.Surprise=sqrt(sellme$R.Surprise)
sellme$R.Trust=log(sellme$R.Trust)
sellme$R.Positive=log(sellme$R.Positive)
sellme$R.Negative=sqrt(sellme$R.Negative)
sellme$R.Joy=sqrt(sellme$R.Joy)


sellme_lm=k_fold_lm(data=sellme,model=pledged_usd~., yname="pledged_usd", k=10, seed=123)
sellme_lm
#summary(sellme)

mydata=sellme
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

#Linear_Model_Past_Variables_LOG_Response_ALL_No_OUT=k_fold_lm(data=no_outliers,model=pledged_usd~., yname="pledged_usd", k=10, seed=123)
#Linear_Model_Past_Variables_LOG_Response_ALL_No_OUT
#linear_m=lm(data=mydata.train, formula=pledged_usd~.)
sellmodel=lm(data=mydata.train, formula=pledged_usd~.)
summary(sellmodel)
lm.pred=predict(sellmodel, mydata.test)
ci=predict(linear_m, mydata.test, interval="confidence", level=.99)
test_actuals=mydata.test$pledged_usd
mse=mean((lm.pred-test_actuals)^2)
r2=1-mse/var(test_actuals)
mse
r2

result_set=cbind(test_actuals, lm.pred)
plot( lm.pred, test_actuals)
results=data.frame(cbind(exp(test_actuals), exp(lm.pred)))
result_set=sqldf("select X1 as actual, X2 as prediction, X2-X12 as diff from results where actual<=20000")
mse20=mean((result_set$prediction-result_set$actual)^2)
r220=mse20/var(result_set$actual)

#---------------------------------------TREE MODEL-------------------------------------
library(tree)
testtree=sqldf("select * from kickstarter_pv_fv where pledged_usd<=30000")
mydata=kickstarter_log_all_20k
mydata=kickstarter_pv_fv
mydata=kickstarter_pv
mydata=testtree
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
kick_tree=tree(pledged_usd~.,mydata.train)
#get summary of results
#4 variabless used in construction: faQ, goal, images, anticipation

summary(kick_tree)
plot(kick_tree);text(kick_tree)

#use cross validation tree function
#looks like a size of 10 and dev 33,258,510,000,000 is out best option------Ummmm fuck
cv_tree= cv.tree(kick_tree, FUN=prune.tree)
cv_tree 
?cv.tree
plot(cv_tree$size, cv_tree$dev)

#prun to our best option of 7
kick_prun=prune.tree(kick_tree, best =9)
kick_prun
plot(kick_prun);text(kick_prun)

#apply to test data---------okay fuck

tree.pred=predict(kick_prun, mydata.test)
test_actuals=mydata.test$pledged_usd
mse=mean((tree.pred-test_actuals)^2)
r2=1-mse/var(test_actuals)
mse
r2

results=data.frame(cbind(exp(test_actuals), exp(tree.pred)))
sqldf("select X1 as actual, X2 as prediction from results where actual<=20000")

library(randomForest)
nvar <- dim(mydata.train)[2]-1
sqr_500=randomForest(pledged_usd~., data=mydata,subset=samples, mtry=sqrt(nvar), importance=TRUE, ntree=500)
sqr_1000=randomForest(pledged_usd~., data=mydata,subset=samples, mtry=sqrt(nvar), importance=TRUE, ntree=1000)
sqr_2000=randomForest(pledged_usd~., data=mydata,subset=samples, mtry=sqrt(nvar), importance=TRUE, ntree=2000)
sqr_m1_500=randomForest(pledged_usd~., data=mydata,subset=samples, mtry=sqrt(nvar)-1, importance=TRUE, ntree=500)
sqr_m1_1000=randomForest(pledged_usd~., data=mydata,subset=samples, mtry=sqrt(nvar)-1, importance=TRUE, ntree=1000)
sqr_m1_2000=randomForest(pledged_usd~., data=mydata,subset=samples, mtry=sqrt(nvar)-1, importance=TRUE, ntree=2000)
sqr_p1_500=randomForest(pledged_usd~., data=mydata,subset=samples, mtry=sqrt(nvar)+1, importance=TRUE, ntree=500)
sqr_p1_1000=randomForest(pledged_usd~., data=mydata,subset=samples, mtry=sqrt(nvar)+1, importance=TRUE, ntree=1000)
sqr_p1_2000=randomForest(pledged_usd~., data=mydata,subset=samples, mtry=sqrt(nvar)+1, importance=TRUE, ntree=2000)

sqr_500
sqr_1000
sqr_2000
sqr_m1_500
sqr_m1_1000
sqr_m1_2000
sqr_p1_500
sqr_p1_1000
#this shows that the sqrt of the mean squared residual is about 41,000
sqr_p1_2000$mse
plot(importance(sqr_p1_2000))
plot(importance(sqr_500)[,1])
plot(sqr_500,log="x",main="black default, red samplesize, green tree depth")
plot(sqr_500,log="y",main="black default, red samplesize, green tree depth")
plot(sqr_500,main="black default, red samplesize, green tree depth")
sqr_500$mtry
log(1345)
exp(7.2)
exp(9.93)

model1 <- randomForest(pledged_usd ~ ., data = mydata.train, importance = TRUE)


sqr_500a=randomForest(pledged_usd~., data=mydata,subset=samples, mtry=20, importance=TRUE, ntree=500)
summary(sqr_500a)
plot(sqr_500a)
plot(sqr_500a);text(sqr_500a)
y=mydata$pledged_usd
x=subset(mydata, select = -c(pledged_usd))
summary(x)
tuner=tuneRF(x, y, ntreeTry=50, stepFactor=2, plot=TRUE)
tuner
