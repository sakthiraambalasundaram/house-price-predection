df<-read.csv("C:/project/Complete ML in R/1. Linear Regression/House_Price.csv")
str(df)
summary(df)

#extended data dictionary$
  
hist(df$crime_rate)
pairs(~price+crime_rate+n_hos_beds+rainfall,data = df)

barplot(table(df$airport))
barplot(table(df$waterbody))
barplot(table(df$bus_ter))



quantile(df$n_hot_rooms,0.99)
UV=3*quantile(df$n_hot_rooms,0.99)
summary(df$n_hot_rooms)


lv=0.3*quantile(df$rainfall,0.01)
df$rainfall(df$rainfall<lv)<-lv
summary(df$rainfall)

#missing value imputation;$

mean(df$n_hos_beds,na.rm = TRUE)
which(is.na(df$n_hos_beds))
df$n_hos_beds[is.na(df$n_hos_beds)]<-mean(df$n_hos_beds,na.rm = TRUE)
summary(df$n_hos_beds)
which(is.na(df$n_hos_beds))

#variabels transform 
pairs(~price+crime_rate,data = df)
plot(df$price,df$crime_rate)

df$crime_rate=log(1+df$crime_rate)

df$avg_dist=(df$dist1+df$dist2+df$dist3+df$dist4)/4
df2<-df[,-7:-10]
df=df2

rm(df2)

df<-df[,-14]


#dummy variable:

df<- dummy.data.frame(df)
df<-df[,-9]
df<-df[,-14]


#correlation matrix

cor(df)
round(cor(df),2)
df<-df[,-16]

#simple linear regerssion

simple_model<-lm(price~room_num,data = df)
summary(simple_model)

plot(df$room_num,df$price)
abline(simple_model)


#multiple linear regerssion
plot(df$room_num,df$price)
abline(simple_model)

multiple_model<- lm(price~room_num,data = df)
summary(multiple_model)

#test train split


set.seed(0)
split=sample.split(df,SplitRatio = 0.8)
training_set=subset(df,split==TRUE)
test_set=subset(df,split==FALSE)

lm_a=lm(price~.,data = training_set)
train_a=predict(lm_a,training_set)


mean((training_set$price_train_a)^2)
mean((test_set$price_test_a)^2)

#subset selection

lm_best=regsubsets(price~.,data = df,nvmax = 15)
summary(lm_best)
which.max(summary(lm_best)$adjr2)
coef(lm_best,8)

lm_forward=regsubsets(price~.,data = df,nvmax = 15,method = "forward")
summary(lm_forward)

#ridge regression & lasso:

x=model.matrix(price~.,data = df)
y=df$price

grid= 10^seq(10,-20,length= 100)
grid

lm_ridge=glmnet(x,y,alpha = 0,lambda = grid)
summary(lm_ridge)

cv_fit= cv.glmnet(x,y, alpha=0, lambda = grid)
plot(cv_fit)

opt_lambda= cv_fit$lambda.min
tss= sum((y-mean(y))^2)

y_a=predict(lm_ridge,s=opt_lambda,newx = x)
rss=sum((y_a-y)^2)
rsq= 1-rss/tss
lm_lesso=glmnet(x,y,alpha = 1,lambda = grid)
