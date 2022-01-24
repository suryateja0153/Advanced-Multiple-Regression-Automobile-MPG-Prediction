#Author: Suryateja Chalapati

#Importing required libraries
rm(list=ls())
library(rio)
library(moments)
library(dplyr)

#Setting the working directory and importing the dataset
setwd("C:/Users/surya/Downloads")

df = import("MPG Data.xlsx", sheet = "6304 Old Auto MPG")
colnames(df)=tolower(make.names(colnames(df)))
attach(df)

#Assigning factor variable & subsetting
df$cylinders = as.factor(df$cylinders)
str(df)

new_sample = subset(df, year <= 76)
new_sample$year = as.factor(new_sample$year)

str(new_sample)

#Setting seed and data sampling
set.seed(36991670)
df_sample = data.frame(new_sample[sample(1:nrow(new_sample), 80, replace = FALSE),])
attach(df_sample)

#Analysis_[1,2]
#Multiple Regression
lin_reg=lm(mpg~cubic.inches+horsepower+weight, data=df_sample)
summary(df_sample$mpg)
summary(lin_reg)

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(lin_reg)
par(mfrow=c(1,1))

#Analysis_3
confint(lin_reg)

par(mfrow=c(2,2))
plot(df_sample$mpg, df_sample$cubic.inches,
     pch=19,main="Multiple Regression",xlab="Cubic Inches",ylab="MPG")
plot(df_sample$mpg, df_sample$horsepower,
     pch=19,main="Multiple Regression",xlab="Horsepower",ylab="MPG")
plot(df_sample$mpg, df_sample$weight,
     pch=19,main="Multiple Regression",xlab="Weight",ylab="MPG")
par(mfrow=c(1,1))

#Analysis_4
#Linearity
plot(df_sample$cubic.inches, df_sample$mpg, pch=19)

plot(df_sample$horsepower, df_sample$mpg, pch=19)

plot(df_sample$weight, df_sample$mpg, pch=19)

plot(df_sample$mpg, lin_reg$fitted.values, pch=19, main="MPG Actual v. Fitted Values")
abline(0, 1, col="red", lwd=3)

#Independence
plot(lin_reg$fitted.values, rstandard(lin_reg), pch=19, main="The residuals and deviation")

#Normality
qqnorm(lin_reg$residuals, pch=19, main="MPG Normality Plot")
qqline(lin_reg$residuals, col="red", lwd=3)

#Equality of Variances
plot(lin_reg$fitted.values, lin_reg$residuals, pch=19, main="MPG Linear Residuals")
abline(0, 0, col="red", lwd=3)

#Analysis_5
#Identifying high leverage points.
lev=hat(model.matrix(lin_reg))
plot(lev, pch=19, ylim=c(0,1))
abline(3*mean(lev), 0, col="red", lwd=3)
plot(lin_reg, which=5)

lev_points = lev[lev > 3*mean(lev)]
loop = match(c(lev_points), lev)
for (i in loop){
  print(paste0("Make is ", df_sample$make[i], ", Model is ", df_sample$model[i], " & year is ", df_sample$year[i]))
}

#Getting rid of the high leverage points
no_lev=df_sample
new_lev=df_sample[lev>(3*mean(lev)),1]
no_lev=no_lev[-new_lev,]

lin_reg=lm(mpg~cubic.inches+horsepower+weight, data=df_sample)
summary(lin_reg)

lin_reg2=lm(mpg~cubic.inches+horsepower+weight, data=no_lev)
summary(lin_reg2)

#Analysis_6
Feat_Lev = lm(mpg ~ cubic.inches + horsepower + poly(weight,2), data = df_sample)
summary(Feat_Lev)

Feat_Lev = lm(mpg ~ cubic.inches + weight + poly(horsepower,2), data = df_sample)
summary(Feat_Lev)

Feat_Lev = lm(mpg ~ cubic.inches + poly(horsepower,2) + poly(weight,2), data = df_sample)
summary(Feat_Lev)

#Analysis_7
lin_reg3 = lm(mpg ~ cubic.inches + horsepower + weight + cylinders, data = df_sample)
summary(lin_reg3)
levels(df_sample$cylinders)
