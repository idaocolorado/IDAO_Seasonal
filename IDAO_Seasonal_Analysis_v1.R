## Seasonal analysis 1: Introduction to seasonal analysis


# First load the requisite packages
rm(list=ls()) # Clear memory
graphics.off() # Clears graphics
library(forecast) # Needed to run forecast and auto.arima functions
library(astsa) # To run acf

############################################################
# Load data from your fitbit dashboard
fitbit_activity <- read.csv("Activity.csv", as.is = TRUE, header = TRUE)
fitbit_sleep <- read.csv("Sleep.csv", as.is = TRUE, header = TRUE)
fitbit <- merge(fitbit_sleep, fitbit_activity, by="Date")

# Or if you're using the file on Github
fitbit <- read.csv("Mike_Fitbit_data.csv", as.is = TRUE, header = TRUE)
fitbit$Date <- as.Date(fitbit$Date)

# Remove technical outliers where not wearing (either make = mean or zero)
# Create indicator based on time
fitbit$timetotal <- fitbit$Minutes.Sedentary + fitbit$Minutes.Lightly.Active + fitbit$Minutes.Fairly.Active + fitbit$Minutes.Very.Active + fitbit$Time.in.Bed
fitbit$perc.recorded <- fitbit$timetotal/(max(fitbit$timetotal, na.rm=TRUE))

#Remove fitbit recordings with less than 70% of time captured
fitbit <- subset(fitbit, fitbit$perc.recorded >= 0.7)

# Create variable for quality of sleep
fitbit$sleepquality <- fitbit$Minutes.Asleep/fitbit$Time.in.Bed
attach(fitbit)

#############################################################################
# Frequency plot from prior post

fileName1 = "/freqencyPlot.jpeg"
jpeg(filename=fileName1, width = 800, height = 600, quality=90)
x = diff(Minutes.Asleep)
FF = abs(fft(x)/sqrt(length(x)))^2  # Need to square and normalize raw fft output
P = (4/(length(FF)))*FF[1:(((length(FF))/2)+1)] # Periodogram values (can scale by multiplying times 4/n)
f = (0:(length(FF)/2))/length(FF) # Harmonic frequencies (from 0 to 0.5)
P.filter = filter(P, c(rep(0.2, 5)), side=2) # Linear filter for plot
#P.filter = lowess(f, P, f=1/15)
plot(f, P, type="l", main="Spectral Plot", xlab="Frequency", ylab="Periodogram")
lines(f, P.filter$y, col="red")
# Quantitatively identify period/cycle
Spect <- as.data.frame(cbind(f, P))
Spect <- Spect[order(Spect$P, decreasing=TRUE)]  # Sort Spectrum from highest to lowest
predcycle <- Spect[Spect$P==max(Spect$P),] # Find frequency where scaled Periodogram is maximal
(Period = 1/predcycle$f) # Identify cycle/period
abline(v=predcycle$f, col="blue")
abline(v=1/7, col="green")
abline(v=1/14, col="orange")
abline(v=1/30, col="purple")
legend(0, max(P), #Location
       c(paste("Dominant frequency (Period=", round(Period, 2), ")"), "Weekly", "Bi-weekly", "Monthly"), #Text
       col=c("blue", "green", "orange", "purple"), #Line colors
       lty=c(1,1,1,1), #Line types
       lwd=c(2.0, 2.0, 2.0, 2.0), #Line thickness
       bty="o", #No border ("o" if border)
       cex=0.9, #Text size
       y.intersp=0.9 #Spacing between text/lines
) 
dev.off()

#########################################################################################
# Average values by day of the week

fileName2 = "./dailySteps.jpg"
jpeg(filename=fileName2, width = 800, height = 600, quality=90)
stepsm = matrix(Steps, ncol=7,byrow=TRUE)

#Stats to assess anova
colnames(stepsm) <- c(rep(1:7,1))
stepsm <- as.data.frame(stepsm)
stepsmreshape <- reshape(stepsm, varying = c(rep(1:7,1)), 
                         v.names = "Steps", 
                         timevar = "Day", 
                         times = c(rep(1:7, 1)), 
                         direction = "long")
anovaSteps <- anova(lm(stepsmreshape$Steps ~ stepsmreshape$Day))
pValueAnova <- round(anovaSteps$`Pr(>F)`[1], 4)

# Plot mean
col.means = apply(stepsm,2,mean)
plot(col.means,type="b", 
     main="Daily Means Plot for Steps", 
     xlab="Day of Week", 
     ylab="Mean Steps/day")
legend("topleft", #Location
       c(paste("P value for difference across days =", pValueAnova)), #Text
       bty= "n", #No border ("o" if border)
       cex=1.5 #Text size
)
dev.off()


## Repeat for sleep
fileName3 = "./dailySleep.jpg"
jpeg(filename=fileName3, width = 800, height = 600, quality=90)
sleepm = matrix(Minutes.Asleep, ncol=7,byrow=TRUE)

#Stats to assess anova
colnames(sleepm) <- c(rep(1:7, 1))
sleepm <- as.data.frame(sleepm)
sleepmreshape <- reshape(sleepm, varying = c(rep(1:7, 1)), 
                         v.names = "MinutesAsleep", 
                         timevar = "Day", 
                         times = c(rep(1:7, 1)), 
                         direction = "long")
anovaSleep <- anova(lm(sleepmreshape$MinutesAsleep ~ sleepmreshape$Day))
pValueAnova2 <- round(anovaSleep$`Pr(>F)`[1], 4)

# Plot mean
col.means.sleep = apply(sleepm,2,mean)
plot(col.means.sleep,type="b", 
     main="Daily Means Plot for Minutes Asleep", 
     xlab="Day of Week", 
     ylab="Mean Minutes Asleep/day")
legend("top", #Location
       c(paste("P value for difference across days =", pValueAnova2)), #Text
       bty= "n", #No border ("o" if border)
       cex=1.5 #Text size
)
dev.off()

# Average values by day of two week cycle

fileName4 = "./monthlySteps.jpg"
jpeg(filename=fileName4, width = 800, height = 600, quality=90)
stepsmonth = matrix(Steps, ncol=14, byrow=TRUE)

#Stats to assess anova
colnames(stepsmonth) <- c(rep(1:14,1))
stepsm <- as.data.frame(stepsmonth)
stepsmonthreshape <- reshape(stepsmonth, varying = c(rep(1:14,1)), 
                         v.names = "Steps", 
                         timevar = "Day", 
                         times = c(rep(1:14, 1)), 
                         direction = "long")
anovaSteps2 <- anova(lm(stepsmonthreshape$Steps ~ stepsmonthreshape$Day))
pValueAnova3 <- round(anovaSteps$`Pr(>F)`[1], 4)

# Plot mean
col.means3 = apply(stepsmonth,2,mean)
plot(col.means3,type="b", 
     main="Biweekly Means Plot for Steps", 
     xlab="Day of Two Week cycle", 
     ylab="Mean Steps/day")
legend("topleft", #Location
       c(paste("P value for difference across days =", pValueAnova3)), #Text
       bty= "n", #No border ("o" if border)
       cex=1.5 #Text size
)
dev.off()

## Repeat for sleep
fileName5 = "./biweeklySleep.jpg"
jpeg(filename=fileName5, width = 800, height = 600, quality=90)
sleepmonth = matrix(Minutes.Asleep, ncol=14,byrow=TRUE)

#Stats to assess anova
colnames(sleepmonth) <- c(rep(1:14, 1))
sleepmonth <- as.data.frame(sleepmonth)
sleepmonthreshape <- reshape(sleepmonth, varying = c(rep(1:14, 1)), 
                         v.names = "MinutesAsleep", 
                         timevar = "Day", 
                         times = c(rep(1:14, 1)), 
                         direction = "long")
anovaSleep2 <- anova(lm(sleepmonthreshape$MinutesAsleep ~ sleepmonthreshape$Day))
pValueAnova4 <- round(anovaSleep2$`Pr(>F)`[1], 4)

# Plot mean
col.means.sleep2 = apply(sleepmonth,2,mean)
plot(col.means.sleep2,type="b", 
     main="Biweekly Means Plot for Minutes Asleep", 
     xlab="Day of Two week cycle", 
     ylab="Mean Minutes Asleep/day")
legend("top", #Location
       c(paste("P value for difference across days =", pValueAnova4)), #Text
       bty= "n", #No border ("o" if border)
       cex=1.5 #Text size
)
dev.off()

