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
updated_data=updated_data[rowSums(updated_data[3:15])>-2600,] # returned 14
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

# Part 5 and 6: Variable Selection
#======================================================================================

air_quality_data <- read.csv("~/Downloads/new_processed_air_quality_data.csv")
attach(air_quality_data)

air_quality_model <- lm(NO2.GT. ~ Date + Time + CO.GT. + NMHC.GT. + NOx.GT. + PT08.S5.O3. + RH)
no_var_model <- lm(NO2.GT. ~ 1)

#Forward variable selection
forward_model <- step(no_var_model, direction='forward', scope=formula(air_quality_model), trace=0)
forward_model$anova
summary(forward_model)

#Forward variable selection
backward_model <- step(air_quality_model, direction='backward', scope=formula(air_quality_model), trace=0)
backward_model$anova
summary(backward_model)

#Both-direction variable selection
both_model <- step(no_var_model, direction='both', scope=formula(air_quality_model), trace=0)
backward_model$anova
summary(backward_model)

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






