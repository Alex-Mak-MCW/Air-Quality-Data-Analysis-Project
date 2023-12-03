air_quality_data <- read.csv("~/Downloads/Processed_AirQualityUCI.csv")
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
