bike = read.csv("C:\\Users\\User\\Desktop\\ST1131\\bike_day.csv")
attach(bike)
summary(cnt)
hist(cnt, col = 2)

#declaring categorical variable
bike$season = as.factor(bike$season)
workingday = as.factor(workingday)
bike$weathersit = as.factor(bike$weathersit)

#check assoc between categorical explanatory variable and response variable
opar <- par(mfrow=c(1,3))
boxplot(cnt ~ season, pch = 9, col = "red", main = "Boxplot Count against Season", xlab = "Season", ylab = "Count")
plot(cnt ~ weathersit, pch = 9, col = "blue", main = "Boxplot Count against Weather", xlab = "Weather", ylab = "Count")
plot(cnt ~ workingday, pch = 9, col = "yellow", main = "Boxplot Count against Working Day", xlab = "Working Day", ylab = "Count")
par(opar)

#check assoc between quantitative explanatory variable and response variable
cor(temp, cnt)
cor(hum, cnt)
cor(windspeed, cnt)
opar <- par(mfrow=c(1,3))
plot(temp, cnt, col = 2, main = "ScatterPlot of Count against Temperature", xlab = "Temperature", ylab = "Count")
plot(hum, cnt, col = 2, main = "ScatterPlot of Count against Humidity", xlab = "Humidity", ylab = "Count")
plot(windspeed, cnt, col = 2, main = "ScatterPlot of Count against Windspeed", xlab = "Windspeed", ylab = "Count")
par(opar)

################### Initial model M1 #######################
M1 = lm(cnt ~ season + weathersit + temp + (season * temp * weathersit), data = bike)
summary(M1)  
anova(M1)

### Residual plot
# Raw residuals: 
raw.res = M1$res

#standardized residuals:
SR = rstandard(M1)
opar <- par(mfrow=c(1,3))

#histogram of Standardized residuals with normal density curve overlaying:
hist(SR, prob = TRUE, col = 2, main = "Bike Rental")
x <- seq(-7, 6, length.out = 301)#
y <-dnorm(x, mean(SR), sd(SR))
lines(x, y, col = "darkblue")

#qq plot of Standardized residuals:
qqnorm(SR, ylab = "SR", xlab = "Z scores", main = "Normal Probability Plot")
qqline(SR, col = "red")

# Standardized residuals vs Predicted y^
plot(M1$fitted.values,SR, xlab="Predicted Response", ylab= "SR", main = "Bike Rental Data")
abline(h=0, col = "red")
par(opar)

### Outliers and Influential Points
which(SR>3 | SR<(-3) )

Cook = cooks.distance(M1)
which(Cook >1) 

################### Model M2 (Intermediate) #######################
M2 = lm(log(cnt) ~ season + temp + weathersit + (season * temp), data = bike)
summary(M2)  
anova(M2)

### Residual plot
# Raw residuals: 
raw.res2 = M2$res

#standardized residuals:
SR2 = rstandard(M2)

opar <- par(mfrow=c(1,3))

#histogram of Standardized residuals with normal density curve overlaying:
hist(SR2, prob = TRUE, col = 2, main = "Bike Rental")
x <- seq(-7, 6, length.out = 301)#
y <-dnorm(x, mean(SR2), sd(SR2))
lines(x, y, col = "darkblue")

#qq plot of Standardized residuals:
qqnorm(SR2, ylab = "SR2", xlab = "Z scores", main = "Normal Probability Plot")
qqline(SR2, col = "red")

# Standardized residuals vs Predicted y^
plot(M2$fitted.values,SR2, xlab="Predicted Response", ylab= "SR2", main = "Bike Rental Data")
abline(h=0, col = "red")
par(opar)

### Outliers and Influential Points
which(SR2 >3 | SR2<(-3) )

Cook = cooks.distance(M2)
which(Cook >1) 

###################### Model M3 (Final) ###########################
newbike = bike[-668, ]
M3 = lm(log(cnt) ~ season + temp + weathersit + (season * temp) + hum + I(hum^2), data = newbike)
summary(M3)
anova(M3)

### Residual plot
# Raw residuals: 
raw.res3 = M3$res

#standardized residuals:
SR3 = rstandard(M3)

opar <- par(mfrow=c(1,3))

#histogram of Standardized residuals with normal density curve overlaying:
hist(SR3, prob = TRUE, col = 2, main = "Bike Rental")
x <- seq(-7, 6, length.out = 301)#
y <-dnorm(x, mean(SR3), sd(SR3))
lines(x, y, col = "darkblue")

#qq plot of Standardized residuals:
qqnorm(SR3, ylab = "SR3", xlab = "Z scores", main = "Normal Probability Plot")
qqline(SR3, col = "red")

# Standardized residuals vs Predicted y^
plot(M3$fitted.values,SR3, xlab="Predicted Response", ylab= "SR3", main = "Bike Rental Data")
abline(h=0, col = "red")
par(opar)

### Outliers and Influential Points
which(SR3 >3 | SR3<(-3) )

Cook = cooks.distance(M3)
which(Cook >1) 




