# install the required library
install.packages("tidyverse")
library(tidyverse)

# Load the file by using file.choose(), then choose the new_processed_air_quality_data.csv file
data = read_csv(file.choose())
data # data loaded successfully
class(data) # returned 'data.frame'

# Part 1: Data Preprocessing
#======================================================================================
# Print the summary of the dataset: find the number of null values for each variable
summary(data)

# drop the last 2 columns since it has no data at all
updated_data<-data[c(1:15)]

# check the summary to ensure the last 2 columns are removed
summary(updated_data)

# Find rows with all -200 ("empty rows), then drop them
updated_data=updated_data[rowSums(updated_data[4:15])>-2600,] # returned 14

updated_data
summary(updated_data)

# replace all the remaining -200 with NA
updated_data<-replace(updated_data, updated_data==-200.00, NA)
summary(updated_data)

# interpolate: use na.locf() for last observation, carry forward
install.packages("zoo")
library(zoo)
updated_data=na.locf(updated_data)
summary(updated_data)

# Part 2: Outlier and Influential Observation Detection Part
#======================================================================================

# enusre the data works for the code later regardless of method employed
updated_data=data

# identify outliers, influential observations (standardized residual)
# With date and time
lm_prep=lm(`NO2(GT)` ~ Date+Time+`CO(GT)`+`PT08.S1(CO)`+`NMHC(GT)`+`C6H6(GT)`+`PT08.S2(NMHC)`+`NOx(GT)`+`PT08.S3(NOx)`+`PT08.S4(NO2)`+`PT08.S5(O3)`+T, data=updated_data)
lm_prep

# identify outliers using leverages
install.packages("ggfortify")
library (ggfortify)
autoplot(lm_prep, which=c(1,2,5:6), label.size=3)
autoplot(lm_prep, which=c(4), label.size=3)
autoplot(lm_prep, which=c(5,6), label.size=3)
autoplot(lm_prep, which=c(6), label.size=3)

# identify influential observations using standardized residual and residual plots
# display in paired format
par(mfrow=c(1,1))
plot(lm_prep, which=1) # display residual plot
plot(lm_prep, which=3) # display standardized residual (aka studentized residual) plot
par(mfrow=c(1,1)) # reset the plot display format

# identify both using cook's distance
cutoff <- 4/((nrow(updated_data)-length(lm_prep$coefficients))) # identify D values > 4/(n-k-1)
cutoff # 0.0004483802
plot(lm_prep, which=4, cook.levels=cutoff, main="Cook's Distance Plot") # display the Cook's distance plot

# Data cleaning
# minus one row (observation) since it's an outlier
updated_data=updated_data[-6130,]
summary(updated_data) # returned size with 9325


# Part 3: Residual analysis
#======================================================================================
# Influence Plot
if(require(car)){ # Use the car library
  influencePlot(lm_prep, id.method="identify", sub="Circle size is proportial to Cook's Distance")
  
}

if(require(car)){ # Use the car library
  crPlots(lm_prep) # Draw partial resdual plots.
}

# Check independence
durbinWatsonTest(lm_prep)

# Normality plots 
plot(lm_prep, which=2)

# Part 4: Residual analysis
#======================================================================================
# Assess colinearity using VIF:
vif(lm_prep)
vif(lm_prep)>5
summary(updated_data)

# Export data in (ETL)
library(readr)
write.csv(updated_data, file="ProcessedInput.csv")

# Part 5 and 6: Variable Selection
#======================================================================================
air_quality_data=read_csv(file.choose())

air_quality_model=lm(`NO2(GT)` ~ Date+Time+`CO(GT)`+`NMHC(GT)`+`NOx(GT)`+`PT08.S3(NOx)`+RH, data=air_quality_data)

# air_quality_model <- lm(NO2.GT. ~ Date + Time + CO.GT. + NMHC.GT. + NOx.GT. + PT08.S5.O3. + RH)
no_var_model <- lm(`NO2(GT)` ~ 1, data=air_quality_data)

#Forward variable selection
forward_model <- step(no_var_model, direction='forward', scope=formula(air_quality_model), trace=0)
forward_model$anova
summary(forward_model)

#Backward variable selection
backward_model <- step(air_quality_model, direction='backward', scope=formula(air_quality_model), trace=0)
backward_model$anova
summary(backward_model)

#Both-direction variable selection
both_model <- step(no_var_model, direction='both', scope=formula(air_quality_model), trace=0)
both_model$anova
summary(both_model)

# Part 7: Residual analysis for the final model 
#======================================================================================

#Make 4 plots at the same time: Residuals vs Fitted, Q-Q plot, Residuals vs Leverage, and Cook's dist vs Leverage
autoplot(backward_model, which = c(1:2,5:6), label.size = 3)

#Cook's Distance Plot
plot(backward_model,which=4)

#Influence Plot
influencePlot(backward_model)

#Partial Residual Plots
library(car)
crPlots(backward_model) 

# Part 8: Make Prediction based on linear regressions
#======================================================================================

# Linear regression

install.packages("caret")
library(caret)

summary(air_quality_data)

# Define Variables (naming issue- TBF)
x = as.matrix(air_quality_data[, c(which(colnames(air_quality_data)=='Date'|colnames(air_quality_data)=='CO(GT)'|colnames(air_quality_data)=='NOx(GT)'|colnames(air_quality_data)=='PT08.S5(O3)'|colnames(air_quality_data)=='RH' ))])
x
y <-air_quality_data$`NO2(GT)`
y

# Perform the test-train split (80% training, 20% testing)
splitIndex <- createDataPartition(air_quality_data$`NO2(GT)`, p = 0.8, list = FALSE)
train_data <- data[splitIndex, ]
test_data <- data[-splitIndex, ]

train_data

# Check the dimensions of the training and testing sets
dim(train_data) # return 7462, 16
dim(test_data) # return 1864, 16

# -----------------
# Fit a linear regression model on the training data
# model_train <- lm(y ~ x, data = train_data)

model_train <- lm(`NO2(GT)` ~ Date+`CO(GT)`+`NOx(GT)`+`PT08.S5(O3)`+RH, data = train_data)

model_train
# Make predictions on the test data
refined_predictions <- predict(model_train, newdata = test_data)

# Compute Mean Squared Error (MSE)
refined_mse <- mean((testData$`NO2(GT)` - refined_predictions)^2)
sqrt(refined_mse)


# -----------------

# Compare the regression perforamnce between the full model and refined model
lm_full=lm(`NO2(GT)` ~ Date+Time+`CO(GT)`+`PT08.S1(CO)`+`NMHC(GT)`+`C6H6(GT)`+`PT08.S2(NMHC)`+`NOx(GT)`+`PT08.S3(NOx)`+`PT08.S4(NO2)`+`PT08.S5(O3)`+T, data=train_data)
lm_full

lm_prep=lm(`NO2(GT)` ~ Date+Time+`CO(GT)`+`PT08.S1(CO)`+`NMHC(GT)`+`C6H6(GT)`+`PT08.S2(NMHC)`+`NOx(GT)`+`PT08.S3(NOx)`+`PT08.S4(NO2)`+`PT08.S5(O3)`+T, data=updated_data)
lm_prep

regular_predictions <- predict(lm_prep, newdata = test_data)

regular_mse <- mean((testData$`NO2(GT)` - regular_predictions)^2)
sqrt(regular_mse)

summary(backward_model)

# select the backward_model as the refined model
library(broom)
glance(model_train)
glance(lm_prep)

AIC(model_train)
AIC(lm_prep)

cat("The refined model is",((AIC(lm_prep)-AIC(model_train))/ AIC(lm_prep))*100,"% better than the original model")

# Ridge Regression
# Find the best lambda using cross-validation
library(glmnet)
cv_ridge <- cv.glmnet(x, y, alpha = 0, nfolds = 10)

best_lambda_ridge <- cv_ridge$lambda.min
best_lambda_ridge
ridge_model <- glmnet(x, y, alpha = 0, lambda = best_lambda_ridge)
ridge_model

plot(cv_ridge)
coef (cv_ridge,s="lambda.min")
print(coef(ridge_model)) # find the smallest coef

# Lasso Regression
# Find the best lambda using cross-validation
cv_lasso <- cv.glmnet(x, y, alpha = 1, nfolds = 10)

best_lambda_lasso <- cv_lasso$lambda.min
best_lambda_lasso
lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lambda_lasso)
lasso_model

plot(cv_lasso)
coef (cv_lasso,s="lambda.min")
print(coef(lasso_model)) # find the smallest coef
