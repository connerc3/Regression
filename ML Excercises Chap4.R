library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)

#matrix of all pairwise correlation
cor(Smarket[,-9])

attach(Smarket)
plot(Volume)


#LOGISTIC REGRESSION

#glm fits generalized linear models
glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,family = binomial)
summary(glm.fits)

#to just get coefficients
coef(glm.fits)
#summary for coef
summary(glm.fits)$coef
#just P-values
summary(glm.fits)$coef[,4]

#predict probability given predictors
glm.probs=predict(glm.fits,type = "response")
glm.probs[1:10]

contrasts(Direction) #to see how dummy bariable is coded

#convert predicted probabilites into class labels
glm.pred=rep("Down",1250) #vector of 1250 Down elements
glm.pred[glm.probs>.5]="Up" #up for elements with prob > .5

#to produce a confusion matrix
table(glm.pred,Direction)
(145+507)/1250 #Correct predictions
#compute fraction of when prediction was correct
mean(glm.pred==Direction)


#to create train and test data
train=(Year<2005)
Smarket.2005=Smarket[!train,]
Direction.2005=Direction[!train]

#create a model using subset of observations
glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,family = binomial,subset = train)
#obtain probabilites using test
glm.probs=predict(glm.fits,Smarket.2005,type = "response")

#compute predictions and compare to actual movement
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005) #test error rate

#removing variables
glm.fits=glm(Direction~Lag1+Lag2,family =binomial ,subset =train)
glm.probs =predict (glm.fits,Smarket.2005 , type="response")
glm.pred=rep ("Down" ,252)
glm.pred[glm.probs >.5]="Up"
table(glm.pred ,Direction.2005)
mean(glm.pred==Direction.2005)
106/(106+76)

#to predict for specific lag values
predict(glm.fits,newdata = data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-.8)),type = "response")





#LINEAR DISCRIMINANT ANALYSIS


#LDA model useS lda function in MASS library
library(MASS)
lda.fit=lda(Direction~Lag1+Lag2, data = Smarket, subset = train)
lda.fit
#linear discriminants obtained by -.642xlag1-.514xlag2
plot(lda.fit)

#predict has list of class, posterior, x=linear discriminants
lda.pred=predict(lda.fit,Smarket.2005)
lda.class=lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)

#50%  threshold to posterior probability can recreate prediction in class
sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)
#probability market will decrease
lda.pred$posterior[1:20,1]
lda.class[1:20]

#can easily choose %
sum(lda.pred$posterior[,1]>.9)




#QUADRATIC DISCRIMINANT ANALYSIS


#QDA model useS qda function in MASS library
#syntax identical to lda
qda.fit=qda(Direction~Lag1+Lag2, data = Smarket, subset = train)
qda.fit

qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005)




#K-NEAREST NEIGHBORS


#knn function in class library
#required 4 inputs
  #matrix containing predictors associated with the data
  #matrix containing predictors associated with the data for which we want to make predictions
  #vector containing the class labels for training observations
  #a value for K, the number of nearest neighbors

library(class)
#cbind (column bind) to bind variables
train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction=Direction[train]

#set seed to ensure reproducibility
set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2005)
(43+83)/252  #not good because only 50% predicted

knn.pred=knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)
#slight improvement but QLM had best results for this data




#APPLICATION TO CARAVAN INSURANCE DATA

dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5822

#scale can lead to issues in KNN
#standarize data to take care of this
standardized.X=scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,1])

test=1:1000
train.X=standardized.X[-test,]
test.X=standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")

table(knn.pred,test.Y)
9/(68+9)

knn.pred=knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y)
5/26

knn.pred=knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y)
4/15




glm.fits=glm(Purchase~.,data = Caravan, family = binomial,subset = -test)
glm.probs=predict(glm.fits,Caravan[test,],type = "response")
glm.pred=rep("No",1000)
glm.pred[glm.probs>.5]="yes"
table(glm.pred,test.Y)

glm.pred=rep("No",1000)
glm.pred[glm.probs>.25]="yes"
table(glm.pred,test.Y)
11/(22+11)
