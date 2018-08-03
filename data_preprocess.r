load(file="C:/Users/brian/Downloads/KickstarterAnalysis.Rda")

kickstarter=Kickstarter.Analysis

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

kickstarter_pv=kickstarter[,c('Currency', 'Top.Category', 'Updates', 'Rewards', 'goal_usd', 'pledged_usd', 'Duration.in.Days', 'Facebook.Connected', 'Facebook.Friends', 'Has.Video', 'Creator.....Projects.Created', 'Creator.....Projects.Backed', 'X..Videos', 'X..Images','X..Words..Description.', 'X..Words..Risks.and.Challenges.','X..FAQs', 'D.Anger', 'D.Anticipation', 'D.Disgust', 'R.Fear', 'R.Sadness', 'R.Surprise', 'R.Trust', 'R.Positive', 'R.Negative', 'R.Joy', 'R.syuzhet.score', 'R.syuzhet.sum', 'R.syuzhet.mean')]
kickstarter_pv_fv=kickstarter[,c('Currency', 'Top.Category', 'Updates', 'Comments', 'Rewards', 'goal_usd', 'pledged_usd', 'Duration.in.Days', 'Facebook.Connected', 'Facebook.Friends', 'Facebook.Shares', 'Has.Video', 'Creator.....Projects.Created', 'Creator.....Projects.Backed', 'X..Videos', 'X..Images','X..Words..Description.', 'X..Words..Risks.and.Challenges.','X..FAQs', 'D.Anger', 'D.Anticipation', 'D.Disgust', 'R.Fear', 'R.Sadness', 'R.Surprise', 'R.Trust', 'R.Positive', 'R.Negative', 'R.Joy', 'R.syuzhet.score', 'R.syuzhet.sum', 'R.syuzhet.mean')]
numberic_variables=kickstarter[,c(  'Updates', 'Comments', 'Rewards', 'Goal', 'Pledged', 'Duration.in.Days', 'Facebook.Friends', 'Creator.....Projects.Created', 'Creator.....Projects.Backed', 'X..Videos', 'X..Images','X..Words..Description.', 'X..Words..Risks.and.Challenges.','X..FAQs')]
sentimate_variables=kickstarter[,c('Pledged','D.Anger', 'D.Anticipation', 'D.Disgust', 'R.Fear', 'R.Sadness', 'R.Surprise', 'R.Trust', 'R.Positive', 'R.Negative', 'R.Joy', 'R.syuzhet.score', 'R.syuzhet.sum', 'R.syuzhet.mean')]
colnames(numberic_variables) <- c("UP", "CO", "RE", "Goal", "Pledged", "DUR","FF","Created", "Backed", "Vid", "Image", "Words", "WRisk", "FAQ")
library(corrplot)
corrplot(cor(numberic_variables), order = "hclust")

sentimate_variables=kickstarter[,c('Pledged','D.Anger', 'D.Anticipation', 'D.Disgust', 'R.Fear', 'R.Sadness', 'R.Surprise', 'R.Trust', 'R.Positive', 'R.Negative', 'R.Joy', 'R.syuzhet.score', 'R.syuzhet.sum', 'R.syuzhet.mean')]
corrplot(cor(sentimate_variables), order = "hclust")
