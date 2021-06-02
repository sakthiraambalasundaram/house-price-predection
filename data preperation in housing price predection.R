df<-read.csv("C:/Users/Sakthi/Desktop/projects/r project/house pricing predection/House-Price.csv",header = TRUE)
str(df)

#Data Dictionary
#Edd 

summary(df)
boxplot(df$n_hot_rooms)
pairs(~df$Sold+df$rainfall)
barplot(table(df$airport))
barplot(table(df$bus_ter))

#Outlier Treatment

quantile(df$n_hot_rooms,0.99)
uv<-3*quantile(df$n_hot_rooms,0.99)
df$n_hot_rooms(df$n_hot_rooms>uv)<-uv
summary(df$n_hot_rooms)

lv<-0.3*quantile(df$rainfall,0.01)
df$rainfall(df$rainfall<lv)<-lv
summary(df$rainfall)

#missing value implementation:

mean(df$n_hos_beds,na.rm = TRUE)

#to idenyify blamk variables:

which(is.na(df$n_hos_beds))
df$n_hos_beds[is.na(df$n_hos_beds)]<-mean(df$n_hos_beds,na.rm = TRUE)
summary(is.na(df$n_hos_beds))

#variable transformation.

df$avg_dist=(df$dist1+df$dist2+df$dist3+df$dist4)/4
df2<-df[,-6:-9]
df=df2
rm(df2)
df<-df[,-13]

#Dummy variables:

df<-dummy.data.frame(df)
df<-df[,-8]
df<-df[,-13]

#training simple logistic model

glm.fit=glm(Sold~price,data = df,family = binomial)
summary(glm.fit)
#Multiple logistics:

glm.fit=glm(Sold~.,data = df,family = binomial)
summary(glm.fit)

#evaluating model performance:
#confusion matrix:
#predicting probability,assigning classes & marketing confusion:

glm.probs=predict(glm.fit,type = "response")
glm.probs[1:10]
glm.pred=rep("No",506)
glm.pred(glm.probs>0.5)="yes"
table(glm.pred,df$Sold)

#LDA:

lda.fit=lda(Sold~.,data = df)
lda.fit
lda.pred=predict(lda.fit,df)
lda.pred$posterior

lda.class=lda.pred$class
table(lda.class,df$Sold)

sum(lda.pred$posterior[,1]>0.8)


#test train split:
set.seed(0)
split=sample.split(df,SplitRatio = 0.8)
train_set=subset(df,split="True")
test_set=subset(df,split="False")
train.fit=glm(Sold~.,data = train_set,family = binomial)
test.probs=predict(train.fit,test_set,type = "response")

test.pred=rep('No',120)
test.pred(test.probs>0.5)='Yes'
table(test.pred,test_set$Sold)

trainx=train_set[,-16]
testx=test_set[,-16]

trainy=train_set$Sold
testy=test_set$Sold
k=3

trainx_s=scale(trainx)
testx_s=scale(testx)
set.seed(0)

knn.pred=knn(trainx_s,testx_s,trainy,k=k)
table(knn.pred,testy)

