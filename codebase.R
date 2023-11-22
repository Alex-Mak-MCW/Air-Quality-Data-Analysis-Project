install.packages("tidyverse")
library(tidyverse)

# Load the file by using file.choose(), then choose the AirQualityUCI.csv file
data = read_csv(file.choose())
data # data loaded successfully
class(data) # returned 'data.frame'

# Print the summary of the dataset
# For returning determine the number of null values for each variables
summary(data)

# drop the last 2 columns since it has no data at all
updated_data<-data[c(1:15)]

# check the summary to ensure the last 2 columns are removed
summary(updated_data)

# Find rows with all -200 ("empty rows), then drop them

updated_data=updated_data[rowSums(updated_data[3:15])>-2600,] # 14
summary(updated_data)


# replace all the remaining -200 with NA
updated_data<-replace(updated_data, updated_data==-200.00, NA)
summary(updated_data)

# Interpolate (TEST)
# install.packages("zoo")
# library(zoo)
# na.approx(updated_data)
# summary(updated_data)


# use na.locf() for last observation, carry forward
install.packages("zoo")
library(zoo)
updated_data=na.locf(updated_data)
summary(updated_data)

# identify outliers, influential observations (standardized residual)

# identify outliers using leverages


# identify influential observations using standardized residual
# identify both using cook's distance








