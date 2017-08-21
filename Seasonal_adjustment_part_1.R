# IDAO Website post 8/21/2017, Seasonal adjustment, Part I

# This code will examine use of sine/cosine and frequency domain approaches for time series data

# Clear memory and graphs
rm(list=ls()) 
graphics.off() 

# Load library
# install.packages("astsa")
library(astsa)

# Load data
fitbit <- read.csv("Mike_Fitbit_data.csv", as.is = TRUE, header = TRUE)
fitbit$Date <- as.Date(full_data$Date)

# Remove technical outliers where not wearing (either make = mean or zero)
# Create indicator based on time
fitbit$timetotal <- fitbit$Minutes.Sedentary + fitbit$Minutes.Lightly.Active + fitbit$Minutes.Fairly.Active + fitbit$Minutes.Very.Active + fitbit$Time.in.Bed
fitbit$perc.recorded <- fitbit$timetotal/(max(fitbit$timetotal, na.rm=TRUE))

#Remove fitbit recordings with less than 70% of time captured
fitbit <- subset(fitbit, fitbit$perc.recorded >= 0.7)

# Create variable for quality of sleep
fitbit$sleepquality <- fitbit$Minutes.Asleep/fitbit$Time.in.Bed

# Create variable for continuous time
fitbit$time <- seq(from = 1, to = length(fitbit$Date), by = 1)

attach(fitbit)
###################################################################################################

# Sine and cosine functions in fitbit linear regression with weekly period
model1 <- lm(Steps~time+sin((2*pi*time)/7)+cos((2*pi*time)/7))
summary(model1)

# Graph
graphName1 = "Cosine_curve_steps.jpeg"
jpeg(filename=graphName1, width = 800, height = 600, quality=90)
plot(time, Steps, main = "Steps over time", col = "black", type="l")
lines(time, model1$fitted.values, col = "red")
legend("topleft",
       "Fitted line", #Text
       col="red", #Line colors
       lty=1, #Line types
       lwd=2.0, #Line thickness
       bty= "n", #No border ("o" if border)
       cex=0.9, #Text size
       y.intersp=0.9
       )#Spacing between text/lines
dev.off()

# Sine and cosine functions in fitbit linear regression with weekly and biweekly periods
model2 <- lm(Steps~time+sin((2*pi*time)/7)+cos((2*pi*time)/7)+sin((2*pi*time)/14)+cos((2*pi*time)/14))
summary(model2)

# Graph
graphName2 = "Cosine_curves_2periods_steps.jpeg"
jpeg(filename=graphName2, width = 800, height = 600, quality=90)
plot(time, Steps, main = "Steps over time", col = "black", type="l")
lines(time, model1$fitted.values, col = "red")
lines(time, model2$fitted.values, col = "green")
legend("topleft",
       c("Weekly alone, Fitted line", "Weekly and biweekly"), #Text
       col=c("red", "green"), #Line colors
       lty=c(1,1), #Line types
       lwd=c(2.0, 2.0), #Line thickness
       bty= "n", #No border ("o" if border)
       cex=0.9, #Text size
       y.intersp=0.9
)#Spacing between text/lines
dev.off()

## Time Series Approaches
graphName3 = "Periodogram_steps.jpeg"
jpeg(filename=graphName3, width = 800, height = 600, quality=90)
#k = kernel("daniell", 2)
steps.per = spec.pgram(Steps, taper = 0, log = "no")
# Lines for top 6 frequencies
graphdata <- cbind(steps.per$freq, steps.per$spec)
sort1 <- sort(graphdata[,2], decreasing = TRUE)
graphdata <- subset(graphdata, graphdata[,2] >= sort1[6])
abline(v=graphdata[1,1], col = "blue")
abline(v=graphdata[2,1], col = "red")
abline(v=graphdata[3,1], col = "green")
abline(v=graphdata[4,1], col = "orange")
abline(v=graphdata[5,1], col = "purple")
abline(v=graphdata[6,1], col = "yellow")
dev.off()

# Filtering
graphName4 = "Periodogram_smooth_steps.jpeg"
jpeg(filename=graphName4, width = 800, height = 600, quality=90)
k = kernel("daniell", 2)
steps.per1 = spec.pgram(Steps, k, taper = 0, log = "no")
abline(v=graphdata[1,1], col = "blue")
abline(v=graphdata[2,1], col = "red")
abline(v=graphdata[3,1], col = "green")
abline(v=graphdata[4,1], col = "orange")
abline(v=graphdata[5,1], col = "purple")
abline(v=graphdata[6,1], col = "yellow")
dev.off()

# Filtering
graphName5 = "Periodogram_smooth2_steps.jpeg"
jpeg(filename=graphName5, width = 800, height = 600, quality=90)
k = kernel("daniell", c(2,2))
steps.per2 = spec.pgram(Steps, k, taper = 0.2, log = "no")
abline(v=graphdata[1,1], col = "blue")
abline(v=graphdata[2,1], col = "red")
abline(v=graphdata[3,1], col = "green")
abline(v=graphdata[4,1], col = "orange")
abline(v=graphdata[5,1], col = "purple")
abline(v=graphdata[6,1], col = "yellow")
dev.off()

graphName6 = "Periodogram_smooth3_steps.jpeg"
jpeg(filename=graphName6, width = 800, height = 600, quality=90)
k = kernel("daniell", c(7,7))
steps.per2 = spec.pgram(Steps, k, taper = 0, log = "no")
abline(v=graphdata[1,1], col = "blue")
abline(v=graphdata[2,1], col = "red")
abline(v=graphdata[3,1], col = "green")
abline(v=graphdata[4,1], col = "orange")
abline(v=graphdata[5,1], col = "purple")
abline(v=graphdata[6,1], col = "yellow")
dev.off()

## Regression with periods
# Sine and cosine functions in fitbit linear regression with weekly and biweekly periods
model3 <- lm(Steps~time+sin((2*pi*time)/15.4)+cos((2*pi*time)/15.4)+sin((2*pi*time)/6.9)+cos((2*pi*time)/6.9)+sin((2*pi*time)/5.8)+cos((2*pi*time)/5.8)+sin((2*pi*time)/4.5)+cos((2*pi*time)/4.5)+sin((2*pi*time)/3.5)+cos((2*pi*time)/3.5)+sin((2*pi*time)/2.3)+cos((2*pi*time)/2.3))
summary(model3)
model4 <- lm(Steps~time+sin((2*pi*time)/5.8)+cos((2*pi*time)/5.8)+sin((2*pi*time)/2.3)+cos((2*pi*time)/2.3))
summary(model4)

# Graph
graphName7 = "Cosine_curves_3periods_steps.jpeg"
jpeg(filename=graphName7, width = 800, height = 600, quality=90)
plot(time, Steps, main = "Steps over time", col = "black", type="l")
lines(time, model3$fitted.values, col = "red")
lines(time, model4$fitted.values, col = "green")
legend("topleft",
       c("6 frequencies, Adjusted R-square = 0.221", "2 frequencies, Adjusted R-square = 0.092"), #Text
       col=c("red", "green"), #Line colors
       lty=c(1,1), #Line types
       lwd=c(2.0, 2.0), #Line thickness
       bty= "n", #No border ("o" if border)
       cex=0.9, #Text size
       y.intersp=0.9
)#Spacing between text/lines
dev.off()

# Stats for confidence intervals
steps.per$spec[18] # Freq 18/108 (for freq = 0.171 * 108)
steps.per$spec[47] # Freq 47/108 (for freq = 0.435 * 108)

U = qchisq(0.025, 2)
L = qchisq(0.975, 2)
low1 <- 2*steps.per$spec[18]/L # Lower for 0.171
high1 <- 2*steps.per$spec[18]/U # Upper for 0.171
low2 <- 2*steps.per$spec[47]/L # Lower for 0.435
high2 <- 2*steps.per$spec[47]/U # Upper for 0.435

graphName7 = "Periodogram_CI_steps.jpeg"
jpeg(filename=graphName7, width = 800, height = 600, quality=90)
k = kernel("daniell", c(7,7))
steps.per4 = spec.pgram(Steps, k, taper = 0, log = "yes", ylim = c(1e6, 3e11))
abline(h= low1, lty="dashed", col="red"); abline(h = high1, lty="dashed", col="red")
abline(h= low2, lty="dashed", col="blue"); abline(h = high2, lty="dashed", col="blue")
abline(v = 0.171, col = "red"); abline(v=0.435, col = "blue")
dev.off()



