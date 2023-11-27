# install the required library
install.packages("tidyverse")
library(tidyverse)

# Load the file by using file.choose(), then choose the AirQualityUCI.csv file
data = read_csv(file.choose())
data # data loaded successfully
class(data) # returned 'data.frame'

# SKIP THE FOLLOWING PART IF YOU DECIDED TO LOAD "Processed_AirQualityICU.csv"
#======================================================================================
# PART STARTS

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

# PART ENDS
#======================================================================================
# Outlier and Influential Observation Detection Part: 

# enusre the data works for the code later regardless of method employed
updated_data=data

# *1000 on CO (PROTOTYPE)
# testing_data=updated_data
# summary(testing_data)
# testing_data$`CO(GT)`=testing_data$`CO(GT)`*1000

# identify outliers, influential observations (standardized residual)
# With date and time
lm_prep=lm(`NO2(GT)` ~ Date+Time+`CO(GT)`+`PT08.S1(CO)`+`NMHC(GT)`+`C6H6(GT)`+`PT08.S2(NMHC)`+`NOx(GT)`+`PT08.S3(NOx)`+`PT08.S4(NO2)`+`PT08.S5(O3)`+T+RH+AH, data=updated_data)
lm_prep

# Without date and time (PROTOTYPE - will be deleted)
# lm1=lm(`NO2(GT)` ~ `CO(GT)`+`PT08.S1(CO)`+`NMHC(GT)`+`C6H6(GT)`+`PT08.S2(NMHC)`+`NOx(GT)`+`PT08.S3(NOx)`+`PT08.S4(NO2)`+`PT08.S5(O3)`+T+RH+AH, data=updated_data)
# lm1

# identify outliers using leverages
leveragePlots(lm_prep)

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
updated_data=updated_data[-5849,]
summary(updated_data) # returned size with 9325


# Residual analysis:
#======================================================================================
# Influence Plot
influencePlot(lm_prep, id.method="identify", sub="Circle size is proportial to Cook's Distance")

# Normality plots 
plot(lm_prep, which=2)

# Assess colinearity using VIF:
vif(lm_prep)
vif(lm_prep)>5
summary(updated_data)

# Variable Selection
#======================================================================================







