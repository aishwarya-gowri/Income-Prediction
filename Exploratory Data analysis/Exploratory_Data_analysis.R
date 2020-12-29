getwd()
setwd("D:/Classes/7th sem/BDA/Project")

user_data = read.delim("census.csv",header=TRUE,sep=',')
user_data
str(user_data)
library(ggplot2)
library(dplyr)
summary(user_data)

##Age distribution
user_data %>% count(age)->user_age_count
user_age_count
#plot(user_age_count,col='blue')
ggplot(data=user_age_count,aes(x=age,y=n))+geom_point(size=3,col="blue")+xlab("Age")+
ylab("Count")+scale_x_continuous(limits=c(0,120),breaks=seq(0,100,20))+
scale_y_continuous(limits=c(0,1300),breaks=seq(0,1300,by=100))+theme(axis.title=element_text(size=20),axis.text=element_text(size=18))
### Age vs Income
ggplot(data=user_data,aes(x=income,y=age))+geom_boxplot()+theme(axis.title=element_text(size=20),axis.text=element_text(size=18))


##Education label distribution
user_data %>% count(education.num)->education.num_count
education.num_count
ggplot(data=user_data,aes(x=education.num,fill=income))+geom_bar(position =position_stack(reverse=TRUE))+scale_x_continuous(limits=c(1,16),breaks=seq(1,16,by=1))+xlab("Number of years of Education")+ylab("Count")+theme(axis.title=element_text(size=20),axis.text=element_text(size=18))
###education.num vs income
ggplot(data=user_data,aes(x=income,y=education.num))+geom_boxplot()+theme(axis.title=element_text(size=20),axis.text=element_text(size=18))



##hours.per.week vs income
ggplot(data=user_data,aes(x=income,y=hours.per.week))+geom_boxplot()+theme(axis.title=element_text(size=20),axis.text=element_text(size=18))



##Working class distribution
ggplot(data=user_data,aes(x=workclass,fill=income))+geom_bar(position=position_stack(reverse=TRUE))+theme(axis.title=element_text(size=20),axis.text=element_text(size=18))+coord_flip()



##Marital Status Distribution
ggplot(data=user_data,aes(x=marital.status,fill=income))+geom_bar(position=position_stack(reverse=TRUE))+theme(axis.title=element_text(size=20),axis.text=element_text(size=18))+coord_flip()



##Relationship distribution
ggplot(data=user_data,aes(x=relationship,fill=income))+geom_bar(position=position_stack(reverse=TRUE))+theme(axis.title=element_text(size=20),axis.text=element_text(size=18))+coord_flip()



##Sex distribution
ggplot(data=user_data,aes(x=sex,fill=income))+geom_bar(position=position_stack(reverse=TRUE))+theme(axis.title=element_text(size=20),axis.text=element_text(size=18))


##Occupation grouped by income
ggplot(data=user_data,aes(x=occupation,fill=income))+geom_bar(position=position_stack(reverse=TRUE))+
  theme(axis.text.x=element_text(face="bold",size=18),axis.title=element_text(size=20))+coord_flip()


## income distribution by race
ggplot(data=user_data,aes(x=race,fill=income))+geom_bar(position=position_stack(reverse=TRUE))+theme(axis.title=element_text(size=20),axis.text=element_text(size=18))+coord_flip()

##native country
ggplot(data=user_data,aes(x=native.country,fill=income))+geom_bar(position=position_stack(reverse=TRUE))+theme(axis.title=element_text(size=20),axis.text=element_text(size=12))+coord_flip()


##histogram for hours.per.week attribute
ggplot(data=user_data,aes(x=hours.per.week))+geom_histogram(fill='mediumspringgreen',col='azure',bins=30)+
scale_x_continuous(limits=c(0,100),breaks=seq(0,100,by=10))+scale_y_continuous(limits=c(0,25000),breaks=seq(0,25000,by=2500))+theme(axis.title=element_text(size=20),axis.text=element_text(size=18))


##Density graph for hours.per.week attribute
d<-density(user_data$hours.per.week)
plot(d,col="RED")






















user_data$age
lapply(user_data$age,as.numeric)->df
lapply(user_data$education.num,as.numeric)->df2
data.frame(df,df2)->df3

class(df[0])
mode(df[3][0])
typeof(df)
class(user_data$age)
str(df)
str(user_data)

pairs(~df+df2,data=df3)
install.packages("car")
library(car)
ggplot2.scatterplot(~user_data$age+user_data$education.num,data=user_data)


