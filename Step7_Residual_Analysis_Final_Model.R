#Make 4 plots at the same time: Residuals vs Fitted, Q-Q plot, Residuals vs Leverage, and Cook's dist vs Leverage
autoplot(backward_model, which = c(1:2,5:6), label.size = 3)

#Cook's Distance Plot
plot(backward_model,which=4)

#Influence Plot
influencePlot(backward_model)

#Partial Residual Plots
library(car)
crPlots(backward_model)
