install.packages("ISLR")
library(MASS)
library(ISLR)

fix(Boston)
names(Boston)

attach(Boston)
lm.fit=lm(medv~lstat)


#SIMPLE LINEAR REGRESSION


lm.fit
summary(lm.fit)
#coefficient
coef(lm.fit)
#confidence interval
confint(lm.fit)
#produce confidence intervals and prediction intervals for
  #the prediction of medv for a given value of lstat
predict(lm.fit,data.frame(lstat=c(5,10,15)),interval = "confidence")
predict(lm.fit,data.frame(lstat=c(5,10,15)),interval = "prediction")

plot(lstat,medv)
abline(lm.fit) # can be used to draw any line
#abline(a,b) intercept of a and slope of b

abline(lm.fit,lwd=3) #line width
abline(lm.fit,lwd=3,col="red") #color
plot(lstat,medv,col="red")
plot(lstat,medv,pch=20) #plot symbols
plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=1:20)

par(mfrow=c(2,2))
plot(lm.fit)

dev.off()

#residuals from a linear regression
plot(predict(lm.fit),residuals(lm.fit))
#studentized residuals
plot(predict(lm.fit),rstudent(lm.fit))

#to compute leverage statistics
plot(hatvalues(lm.fit))
#observation with the largest leverage statistic
which.max(hatvalues(lm.fit))



#MULTIPLE LINEAR REGRESSION


lm.fit=lm(medv~lstat+age)
summary(lm.fit)

#to use all variables
lm.fit=lm(medv~.,data = Boston)
summary((lm.fit))

#can access individual components of the summary by name
  #?summary.lm to see available
#R^2
summary(lm.fit)$r.sq
#RSE
summary(lm.fit)$sigma

#VIF is in car package
#install.packages("car")
library(car)
vif(lm.fit)

#to remove one variable
lm.fit1=lm(medv~.-age, data = Boston)
summary(lm.fit1)

#or remove using update function
lm.fit1=update(lm.fit, ~.-age)



#INTERACTION TERMS

summary(lm(medv~lstat*age))



#NON LINEAR TRANSFORMATIONS OF PREDICTORS

lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)

#to further quantify extent that quadratif is better than linear

lm.fit=lm(medv~lstat)
anova(lm.fit,lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)


#can use poly function for higher degrees
lm.fit5=lm(medv~poly(lstat,5))#5th degree
summary(lm.fit5)





#QUALITATIVE PREDICTORS
names(Carseats)

#multiple regression with some interaction terms
c.lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data = Carseats)
summary(c.lm.fit)


attach(Carseats)
#shows coding that r uses for dummy variables
contrasts(ShelveLoc)




#WRITING FUNCTIONS

LoadLibraries = function(){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded.")
}

LoadLibraries()
