
# Loading mtcars data set from a mtcars.csv file
mtcars <- read.csv(file='mtcars.csv', header=TRUE, sep=",")

# Converting appropriate variables to factors  
mtcars2 <- within(mtcars, {
   vs <- factor(vs)
   am <- factor(am)
   cyl  <- factor(cyl)
   gear <- factor(gear)
   carb <- factor(carb)
})


# Print the first six rows
print("head")
head(mtcars2, 6)

myvars <- c("mpg","wt","drat")
mtcars_subset <- mtcars2[myvars]

# Print the first six rows
print("head")
head(mtcars_subset, 6)

# Print the correlation matrix
print("cor")
corr_matrix <- cor(mtcars_subset, method = "pearson")
round(corr_matrix, 4)

# Create the multiple regression model and print summary statistics. Note that this model includes the interaction term. 
model1 <- lm(mpg ~ wt + drat + wt:drat, data=mtcars_subset)
summary(model1)

# Subsetting data to only include the variables that are needed
myvars <- c("mpg","wt","drat","am")
mtcars_subset <- mtcars2[myvars]

# Create the model
model2 <- lm(mpg ~ wt + drat + wt:drat + am, data=mtcars_subset)
summary(model2)

# predicted values
print("fitted")
fitted_values <- fitted.values(model2) 
fitted_values

# residuals
print("residuals")
residuals <- residuals(model2)
residuals

plot(fitted_values, residuals, 
     main = "Residuals against Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals",
     col="red", 
     pch = 19, frame = FALSE)

qqnorm(residuals, pch = 19, col="red", frame = FALSE)
qqline(residuals, col = "blue", lwd = 2)

# confidence intervals for model parameters
print("confint")
conf_90_int <- confint(model2, level=0.90) 
round(conf_90_int, 4)

newdata <- data.frame(wt=3.88, drat=3.05, am='1')

print("prediction interval")
prediction_pred_int <- predict(model2, newdata, interval="predict", level=0.90) 
round(prediction_pred_int, 4)

print("confidence interval")
prediction_conf_int <- predict(model2, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int, 4)

# Loading mtcars data set from a mtcars.csv file
mtcars <- read.csv(file='mtcars.csv', header=TRUE, sep=",")

# Converting appropriate variables to factors  
mtcars2 <- within(mtcars, {
   vs <- factor(vs)
   am <- factor(am)
   cyl  <- factor(cyl)
   gear <- factor(gear)
   carb <- factor(carb)
})


# Print the first six rows
head(mtcars2, 6)

# Subsetting data to only include the variables that are needed for model 1 mpg, drat, hp, qsec
myvars <- c("mpg","hp","qsec", "drat")
mtcars_subset <- mtcars2[myvars]

# Print the first six rows
print("head")
head(mtcars_subset)

# Print the correlation matrix
print("correlation matrix for mpg, hp, drat & qsec")
corr_matrix <- cor(mtcars_subset, method = "pearson")
round(corr_matrix, 4)

# Create the model for fuel economy (mpg) with horsepower (hp), quarter mile time (qsec), rear axle ratio (drat), and their interactions
model1 <- lm(mpg ~ hp + qsec + drat + hp:qsec + hp:drat, data=mtcars_subset)
summary(model1)

# predicted values
print("fitted")
fitted_values <- fitted.values(model1) 
fitted_values

# residuals
print("residuals")
residuals <- residuals(model1)
residuals

plot(fitted_values, residuals, 
     main = "Residuals against Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals",
     col="Purple", 
     pch = 19, frame = FALSE)

qqnorm(residuals, pch = 19, col="purple", frame = FALSE)
qqline(residuals, col = "dark green", lwd = 2)

# predictions with hypothetical values for model 1 variables
newdata <- data.frame(hp=175, qsec=14.2, drat=3.91)

print("prediction interval for model 1 hypothetical values")
prediction_pred_int <- predict(model1, newdata, interval="predict", level=0.95) 
round(prediction_pred_int, 4)

print("confidence interval for model 1 hypothetical values")
prediction_conf_int <- predict(model1, newdata, interval="confidence", level=0.95) 
round(prediction_conf_int, 4)

myvars <- c("mpg","hp","qsec","drat", "cyl")
mtcars_subset <- mtcars2[myvars]

# Print the first six rows
print("head")
head(mtcars_subset, 6)


# Fit the regression model
model2 <- lm(mpg ~ hp + qsec + hp:qsec + factor(cyl), data=mtcars_subset)

# Summarize the model
summary(model2)

# predicted values
print("fitted")
fitted_values <- fitted.values(model2) 
fitted_values

# residuals
print("residuals")
residuals <- residuals(model2)
residuals

# Plot residuals against fitted values for model 2 
plot(fitted_values, residuals,
     main = "Residuals against Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals", col = "Purple",
     pch = 19, frame = FALSE)

# Q-Q plot of residuals for model 2 
qqnorm(residuals, pch = 19, col = "purple", frame = FALSE) 
qqline(residuals, col = "dark green", lwd = 2)

# Predictions with hypothetical values for model 2 variables
newdata <- data.frame(hp = 175, qsec = 14.2, cyl = factor(6, levels = levels(mtcars2$cyl)))

# Prediction interval for model 2 hypothetical values
print("prediction interval")
prediction_pred_int <- predict(model2, newdata, interval = "predict", level = 0.95)
round(prediction_pred_int, 4)

# Confidence interval for model 2 hypothetical values
print("confidence interval")
prediction_conf_int <- predict(model2, newdata, interval = "confidence", level = 0.95)
round(prediction_conf_int, 4)
