require(MASS)
# install.packages("ISLR")
#require(ISLR)
require(car)

boston_df <- Boston
ggplot(data = boston_df,aes(lstat,medv))+
  geom_point()
lm.fit <- lm(medv~lstat,data = boston_df)
summary(lm.fit)
confint(lm.fit)
attach(boston_df)
plot(lstat,medv)
abline(lm.fit)
ggplot(data = boston_df,aes(lstat,medv))+
  geom_point()+
  geom_abline(slope = lm.fit$coefficients[2],intercept = lm.fit$coefficients[1],
              color="red",lty="dashed",lwd=1)
par(mfrow=c(2,2))
plot(lm.fit)
ggplot(data=NULL,aes(predict(lm.fit),residuals(lm.fit)))+
  geom_point()+
  geom_smooth()
plot(hatvalues(lm.fit))

ggplot(data=NULL,aes(seq(1,length(hatvalues(lm.fit))),hatvalues(lm.fit)))+
  geom_point()

#Multiple Linear Regression
lm.fit <- lm(medv~lstat + age, data = boston_df)
summary(lm.fit)

lm.fit <- lm(medv~. + age, data = boston_df)
summary(lm.fit)
vif(lm.fit)

lm.fit1 <- update(lm.fit,~.-age-indus)
summary(lm.fit1)
vif(lm.fit1)

#Interaction terms
lm.fit2 <- lm(medv~lstat*age,data = boston_df)
summary(lm.fit2)

#Non-linear Transformations of the predictors
lm.fit3 <- lm(medv~lstat+I(lstat^2))
summary(lm.fit3)
lm.fit4 <- lm(medv~lstat)
anova(lm.fit4,lm.fit3) # this function test the hypothesis where the null state that 
#the two models fit the data equally well and the alternative state that the full model is superior
par(mfrow=c(2,2),mar=c(1,1,1,1))
plot(lm.fit3)

lm.fit5 <- lm(medv~poly(lstat,5))
summary(lm.fit5)
summary(lm(medv~log(lstat)))

#Qualitative predictors
fix(Carseats)
colnames(Carseats)
attach(Carseats)
lm.fit6 <- lm(Sales~.+Income:Advertising+Price:Age,data = Carseats)
summary(lm.fit6)
contrasts(Urban) #this function contrasts helps to understand how R constructed the dummies
contrasts(ShelveLoc)


# Linear Model Selection and Regularization

# Best Subset Selection
fix(Hitters)
colnames(Hitters)
sum(is.na(Hitters$Salary))
dim(Hitters)
Hitters <- na.omit(Hitters)
sum(is.na(Hitters$Salary))
dim(Hitters)
require(leaps)
regfit.full <- regsubsets(Salary~.,Hitters)
summary(regfit.full)
