# IDAO Website post 8/26/2017, Seasonal adjustment, Part II

# This code will examine some interative time domain approaches to seasonal adjustment, and put it all together into a single approach

# Clear memory and graphs
rm(list=ls()) 
graphics.off() 

# Load library
# install.packages("astsa")
library(astsa)
library(forecast)

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

# Use of ACF/PACF to examine seasonal effects
graphName1 = "acf_steps.jpeg"
jpeg(filename=graphName1, width = 800, height = 600, quality=90)
acf2(Steps, max.lag = 100)
dev.off()

# Check for trend
modelCheck <- lm(Steps~time)
summary(modelCheck)

# AUTO.ARIMA to create non-seasonal model
model1 <- auto.arima(Steps, max.p = 3, max.q = 3, max.d = 2, ic = "aicc", trace = TRUE, stepwise = TRUE)
summary(model1)
graphName2 = "model1_steps.jpeg"
jpeg(filename=graphName2, width = 800, height = 600, quality=90)
plot(forecast(model1, h = 30, fan = TRUE))
dev.off()


# AUTO.ARIMA loop to select best seasonal model
bestModelAICC <- 500000
bestModel <- NULL
bestSeasonPeriod <- NULL

for(i in 1:30) {
  model2 <- auto.arima(ts(Steps, freq = i), max.p = 3, max.d = 2, max.q = 3, ic = "aicc", trace = TRUE)
  if(model2$aicc < bestModelAICC) {
    bestModelAICC = model2$aicc
    bestModel = model2
    bestSeasonPeriod = i
  }
}

print(paste("Best seasonal period is", bestSeasonPeriod))
summary(bestModel)
graphName3 = "model2Seasonal_steps.jpeg"
jpeg(filename=graphName3, width = 800, height = 600, quality=90)
plot(forecast(bestModel, h = 30, fan = TRUE))
dev.off()

######### Combination methods########
# Run periodogram to select best period
graphName8 = "periodogram5_steps.jpeg"
jpeg(filename=graphName8, width = 800, height = 600, quality=90)
steps.per = spec.pgram(Steps, taper = 0, log = "no")
freqSpec <- cbind(steps.per$freq, steps.per$spec)
sortedSpec <- sort(freqSpec[,2], decreasing = TRUE)
topFreq <- subset(freqSpec, freqSpec[,2] >= sortedSpec[5])
abline(v=topFreq[1,1], col = "blue")
abline(v=topFreq[2,1], col = "red")
abline(v=topFreq[3,1], col = "green")
abline(v=topFreq[4,1], col = "orange")
abline(v=topFreq[5,1], col = "purple")
dev.off()

# Run automated auto.arima to select from these
bestModel1AICC <- 500000
bestModel1 <- NULL
bestSeason1Period <- NULL

for(i in topFreq[,1]) {
  model3 <- auto.arima(ts(Steps, freq = round((1/i), 0)), max.p = 3, max.d = 2, max.q = 3, ic = "aicc", trace = TRUE)
  if(model3$aicc < bestModel1AICC) {
    bestModel1AICC = model3$aicc
    bestModel1 = model3
    bestSeason1Period = (1/i)
  }
}

print(paste("Best seasonal period is", bestSeason1Period))
summary(bestModel1)
graphName4 = "seasonalARIMA_steps.jpeg"
jpeg(filename=graphName4, width = 800, height = 600, quality=90)
plot(forecast(bestModel1, h = 30, fan = TRUE))
dev.off()

# Run linear regression with cosine functions to select best model and graph it
bestModel2pVal <- 0.5
bestModel2 <- NULL
bestSeason2Period <- NULL

for(i in topFreq[,1]) {
  model4 <- lm(Steps~time+sin((2*pi*time)/(1/i))+cos((2*pi*time)/(1/i)))
  f <- summary(model4)$fstatistic
  p <- pf(f[1], f[2], f[3], lower.tail = F)
  attributes(p) <- NULL
  model4pVal <- p
  if(model4pVal < bestModel2pVal) {
    bestModel2pVal = model4pVal
    bestModel2 = model4
    bestSeason2Period = (1/i)
  }
}

print(paste("Best seasonal period is", bestSeason2Period))
(sumMod <- summary(bestModel2))

# Examine AICc from the linear model based on mean squared error
n = length(bestModel2$fitted.values)
k = length(bestModel2$fitted.values) - (bestModel2$df.residual + 1)
aicLinearModel <- n * log(anova(bestModel2)$`Mean Sq`[k+1]) + 2*k
aiccLinearModel <- aicLinearModel + ((2*k)*(k+1))/(n - k - 1)

nextTime <- data.frame(time = c(1:30))
predict(bestModel2)
foreBestModel2 <- forecast(bestModel2, newdata = nextTime, h = 30)
forecastData <- data.frame(cbind(seq(from = max(time) + 1, to = max(time) + 30, by = 1), foreBestModel2$mean, foreBestModel2$lower[,2], foreBestModel2$upper[,2]))
colnames(forecastData) <- c("Time", "Steps", "Upper", "Lower")
graphName5 = "linearModel_steps.jpeg"
jpeg(filename=graphName5, width = 800, height = 600, quality=90)
plot(time, Steps, type = "l", col = "black", xlim = c(0, 243), main = paste("Forecast Steps for Seasonal Period of", round(bestSeason2Period, 1), "days"))
lines(bestModel2$fitted.values, col = "red")
lines(forecastData$Time, forecastData$Steps, col = "blue")
lines(forecastData$Time, forecastData$Lower, col = "blue", lty = "dashed")
lines(forecastData$Time, forecastData$Upper, col = "blue", lty = "dashed")
legend("topleft",
       c("Fitted line", "Forecast estimate", "95% Confidence intervals"), #Text
       col= c("red", "blue", "blue"), #Line colors
       lty=c("solid","solid", "dashed"), #Line types
       lwd=c(2.0, 2.0, 2.0), #Line thickness
       bty= "n", #No border ("o" if border)
       cex=0.9, #Text size
       y.intersp=0.9
)#Spacing between text/lines
dev.off()

# Check for autocorrelation
graphName6 = "acfModel_steps.jpeg"
jpeg(filename=graphName6, width = 800, height = 600, quality=90)
acf2(bestModel2$residuals)
dev.off


# Create lagged dataset and run model
laggedSteps <- lag(ts(Steps), 14)
data <- ts.intersect(ts(Steps), laggedSteps, ts(time))
colnames(data) <- c("Steps", "LagSteps", "time")

model5 <- lm(Steps~time+LagSteps+sin((2*pi*time)/bestSeason2Period)+cos((2*pi*time)/bestSeason2Period), data = data)
summary(model5)

# Examine AICc from the linear model based on mean squared error
n = length(model5$fitted.values)
k = length(model5$fitted.values) - (model5$df.residual + 1)
(aicLinearModel1 <- n * log(anova(model5)$`Mean Sq`[k+1]) + 2*k)
(aiccLinearModel1 <- aicLinearModel1 + ((2*k)*(k+1))/(n - k - 1))

graphName6 = "linearModelLagACF_steps.jpeg"
jpeg(filename=graphName6, width = 800, height = 600, quality=90)
acf2(model5$residuals)
dev.off()

data.df <- as.data.frame(data)
data.df$Fit <- model5$fitted.values


graphName7 = "linearBestModel_steps.jpeg"
jpeg(filename=graphName7, width = 800, height = 600, quality=90)
plot(time, Steps, main = "Best Model for Steps", type = "l", xlim=c(0, 190))
lines(model5$fitted.values, col = "red")
dev.off()

       








