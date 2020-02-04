# Correlation
corMat <- cor(mtcars[,c(1,3,6)])
round(corMat,2)     

?mtcars
# Simple Linear Regression
m <- lm(mpg ~ wt, data = mtcars)

# Coefficient of Independent Variables
coef(m)

# Model Summary
summary(m)

# Evaluate Dependent Variables given an independent variable
p <- predict(m, data.frame(wt = 3))
p

# Plotting Regression Line
plot(mpg~wt,data=mtcars)
abline(m)

# Multiple Linear Regression
m <- lm(mpg ~ wt+hp, data = mtcars)

coef(m)

summary(m)

p<- predict(m, data.frame(wt = 3, hp = 200))

# Multiple Linear Regression with Interactions
m <- lm(mpg ~ wt*hp, data = mtcars)

coef(m)

summary(m)

p<- predict(m, data.frame(wt = 3, hp = 200))

# Logistic Regression
m <- glm(am ~ mpg+wt+hp, data = mtcars)

coef(m)

summary(m)
