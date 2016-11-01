
#working space setup, reading in the data and preparing the libaries that will be used later, data frame is converted into data table

setwd('C:/Users/r4ndomw4lk/Desktop/Zadanie Pearson/EA_audition_for_sharing')
df=read.csv('EA_Audition_Data.csv')
library(data.table)
library(ggplot2)
df=data.table(df)

#data summary

summary(df)

#data summary countinued

str(df)

#first 6 rows examination

head(df)

#puts factors for units in the right order

df$unit=factor(df$unit, levels=c("1","2","3","4","5","6","7","8","9","10","11","12","REVIEW 1","REVIEW 2","REVIEW 3","REVIEW 4", "VIDEO PODCASTS"))

#creates new column in_course_mod with more understandable names than in the in_course column 

df[in_course=='t',in_course_mod:='studying with teacher']
df[in_course=='f',in_course_mod:='studying alone']


#average score by unit plot, median scores for unit 11 and 12 seems to be lower than for the rest of the units

ggplot(df,aes(x=unit,y=avg_score))+geom_boxplot(fill="#4271AE",colour="#1F3552",outlier.shape = NA)+
scale_y_continuous(name = "Average Score")+scale_x_discrete(name = "Unit")+ ggtitle("Average Score by Unit")+theme(axis.text.x=element_text(angle=90, hjust=1))

#average score by unit and course type, a teacher seems to have only little impact on the average scores

ggplot(df,aes(x=unit,y=avg_score))+geom_boxplot(fill="#4271AE",colour="#1F3552",outlier.shape = NA)+facet_grid(in_course_mod~.)+scale_y_continuous(name = "Average Score")+scale_x_discrete(name = "Unit")+ ggtitle("Average Score by Unit and Course Type")+theme(axis.text.x=element_text(angle=90, hjust=1))

#checking if means bring additional insight, they tend to exhibit similar trend as median

means=df[,.(calc_mean=mean(avg_score, na.rm=TRUE)),by=unit]
ggplot(df,aes(x=unit,y=avg_score))+geom_boxplot(fill="#4271AE",colour="#1F3552",outlier.shape = NA)+stat_summary(fun.y=mean, colour="black", geom="point", shape=15, size=3, show_guide=FALSE)

#median completion by unit plot, there seems to be significantly lower median completion rates for chapters 10-12

ggplot(df,aes(x=unit,y=completion))+geom_boxplot(fill="#4271AE",colour="#1F3552",outlier.shape = NA)+
scale_y_continuous(name = "Completion")+scale_x_discrete(name = "Unit")+ ggtitle("Completion by Unit")+theme(axis.text.x=element_text(angle=90, hjust=1))

#median completion by unit and studying type plot, presence of the teacher seems to have a significant positive impact on a completion rates

ggplot(df,aes(x=unit,y=completion))+geom_boxplot(fill="#4271AE",colour="#1F3552",outlier.shape = NA)+facet_grid(in_course_mod~.)+
scale_y_continuous(name = "Completion")+scale_x_discrete(name = "Unit")+ ggtitle("Completion by Unit and Studying Type")+theme(axis.text.x=element_text(angle=90, hjust=1))

#divergence rate by unit plot, students seems to be diverging very little from the suggested order of excercises

ggplot(df,aes(x=unit,y=inv_rate))+geom_boxplot(fill="#4271AE",colour="#1F3552",outlier.shape = NA)+scale_y_continuous(name = "Divergence rate", limits=c(0,0.1))+scale_x_discrete(name = "Unit")+ ggtitle("Divergence Rate from the Suggested Order of Tasks by Unit")+theme(axis.text.x=element_text(angle=90, hjust=1))

#total average score by course type, students with a teacher achieve slightly higher scores

ggplot(df,aes(x=in_course_mod,y=avg_score))+geom_boxplot(fill="#4271AE",colour="#1F3552",outlier.shape = NA)+
scale_y_continuous(name = "Average Score")+scale_x_discrete(name = "Course Type")+ ggtitle("Average Score by Course Type")

#total median completion by course type, there are significantly higher completion rates for students with a teacher

ggplot(df,aes(x=in_course_mod,y=completion))+geom_boxplot(fill="#4271AE",colour="#1F3552",outlier.shape = NA)+
scale_y_continuous(name = "Completion")+scale_x_discrete(name = "Course Type")+ ggtitle("Completion by Course Type")

#average score vs inv rate, there is no clear relationship between these two variables

ggplot(df,aes(x=inv_rate,y=avg_score))+geom_point(alpha=0.1)

#there is only slight negative correlation between inv rate and average score

df_cleaned=df[complete.cases(df)]
cor(df_cleaned$avg_score, df_cleaned$inv_rate)

#inv_rate is statisticaly significant, but the R2 is too small justify the model

lmInvRate=lm(avg_score~inv_rate,df_cleaned)
summary(lmInvRate)

#by extending the previous model with in_course higher but still very small R2 is achieved, dead end probably

lmTotal=lm(avg_score~inv_rate+in_course,df_cleaned)
summary(lmTotal)

#median completion by in_course check

df[,median(completion),by=in_course]

#mean average score by in_course

df[,mean(avg_score, na.rm=TRUE),by=in_course]

df[,.(mean=mean(avg_score, na.rm=TRUE), sd=sd(avg_score, na.rm=TRUE))]

#top10 poorest performing countries on average score

head(df[,.(mean_avg_score=round(mean(avg_score, na.rm=TRUE),2)),by=country][order(mean_avg_score)],10)

#top10 best performing countries

head(df[,.(mean_avg_score=round(mean(avg_score, na.rm=TRUE),2)),by=country][order(-mean_avg_score)],10)

#calculates and plots mean completion rates and average scores per country

countryData=df[,.(mean_avg_score=round(mean(avg_score, na.rm=TRUE),2), mean_completion=round(mean(completion),2)),by=country]
ggplot(countryData, aes(x=mean_avg_score,y=mean_completion, label=country))+geom_text(alpha=0.5)+
scale_y_continuous(name = "Mean Completion")+scale_x_continuous(name = "Mean Average Score for Student Population in Country")+ ggtitle("Mean Completion Rates and Average Scores per Country")+geom_vline(xintercept=countryData[,mean(mean_avg_score, na.rm=TRUE),],size=0.6,colour='darkred')+geom_hline(yintercept=countryData[,mean(mean_completion, na.rm=TRUE),],size=0.6,colour='darkred')

#calculates the mean completion rate for the population

df[,.(median_completion=median(completion), median_score=median(avg_score, na.rm=TRUE)),by=in_course]

#calculates the mean average score by unit

df[,mean(avg_score, na.rm=TRUE),by=unit]

#calculates the mean completion rate by unit

df[,mean(completion, na.rm=TRUE),by=unit]

#calculates the mean and median inv rate by unit

df[,.(mean=round(mean(inv_rate),3), median=median(inv_rate)),by=unit]

#plots the number of students that are active in a given unit

ggplot(as.data.frame(table(df$unit)), aes(x=Var1,y=Freq,label=Freq))+geom_point()+ geom_text(vjust = -0.5, nudge_x = 0.05, size=3.5)+scale_y_continuous(name = "Number of Students")+scale_x_discrete(name = "Unit")+ggtitle("Number of Students per Unit")+theme(axis.text.x=element_text(angle=90, hjust=1))

#calculates the number of students that have completed a given unit

table(df[completion==1, unit])

#plots the % of students that have completed a given unit

ggplot(as.data.frame(round(table(df[completion==1, unit])/table(df$unit),4)*100), aes(x=Var1,y=Freq,label=Freq))+geom_point()+ geom_text(vjust = -0.5, nudge_x = 0.05, size=3.5)+scale_y_continuous(name = "% of Students")+scale_x_discrete(name = "Unit")+ggtitle("% of Students That Reached 100% Completion Rate for the Unit")+theme(axis.text.x=element_text(angle=90, hjust=1))

#calculates the % of students that have completed a given unit

round(table(df[completion==1, unit])/table(df$unit),4)*100

#calculates and plots number of lost students vs previous unit

freq<-data.frame(table(df$unit))
names(freq)<-c('unit', 'student_count')
freq<-freq[1:12,]
drop_down_rate<-(tail(freq, -1) - head(freq, -1))/head(freq,-1)
drop_down_rate<-drop_down_rate[,2]
ggplot(as.data.frame(cbind(Drop_Rate=round(drop_down_rate,4)*100,Unit=c(1:11))), aes(x=as.factor(Unit), y=Drop_Rate, label=Drop_Rate))+geom_point()+geom_text(vjust = -0.5, nudge_x = 0.05, size=3.5)+scale_y_continuous(name = "% of Students")+scale_x_discrete(name = "After the Unit")+ggtitle("Change in the Number of Students After Previous Unit (%)")

#compares lost students between the course types

teach<-subset(df, in_course=='t')
no_teach<-subset(df, in_course=='f')

freq<-data.frame(table(teach$unit))
names(freq)<-c('unit', 'student_count')
freq<-freq[1:12,]
drop_down_rate<-(tail(freq, -1) - head(freq, -1))/head(freq,-1)
drop_down_rate<-drop_down_rate[,2]
plot(drop_down_rate)

freq<-data.frame(table(no_teach$unit))
names(freq)<-c('unit', 'student_count')
freq<-freq[1:12,]
drop_down_rate<-(tail(freq, -1) - head(freq, -1))/head(freq,-1)
drop_down_rate<-drop_down_rate[,2]
plot(drop_down_rate)

#checks how the average score works

ggplot(df,aes(x=completion,y=avg_score))+geom_point(alpha=0.3)

#investigates completion of chapter 5

unit5<-subset(df, unit=='5')
hist(unit5$completion)
