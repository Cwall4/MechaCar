#install.packages('pastecs')

library(pastecs) # A library that contains more detailed summary statistics than the summary() function.

# MPG Regression

data_mpg <- read.csv(file = 'MechaCar_mpg.csv', check.names = F, stringsAsFactors = F)

summary(data_mpg)

names(data_mpg) <- c('VLength', 'VWeight', 'SAngle', 'GClearance', 'AWD', 'MPG')

reg <- lm(MPG ~ VLength + VWeight + SAngle + GClearance + AWD, data = data_mpg)

summary(reg)

#aov(reg)

#plot(reg$fitted.values, reg$residuals)

shapiro.test(reg$residuals)

plot(density(reg$residuals))

# Suspension Coil

data_susp <- read.csv(file = 'Suspension_Coil.csv', check.names = F, stringsAsFactors = F)

# I use the function from pastecs, filtering for the desired summary stats.
stat.desc(data_susp$PSI)[c('median', 'mean', 'var', 'std.dev')]

# t-Test
t.test(data_susp$PSI, mu = 1500)

# Design Your Own Study