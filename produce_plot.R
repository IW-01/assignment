# load in Boston housing prices dataset downloaded from
# http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data
# and reformatted in Excel to a tab delimited txt file with no header row
# (dataset description in file housing.names)

x <- read.table('housing.txt', sep = '\t')

# inspect the structure of the dataset
str(x)

# fit linear model for house price variable (V14)
# using all remaining variables in the dataset
# and view summary statistics
fit <- lm(V14 ~ ., data = x)
summary(fit)

# R2 value is 0.7406

# update model to remove V7 which has lowest abs t value and view summary
fit2 <- update(fit, .~. -V7)
summary(fit2)

# update model to remove V3 with next lowest abs t value and view summary
fit3 <- update(fit2, .~. -V3)
summary(fit3)

# R2 values unchanged, slight changes in adjusted R2 and residuals

# output plots for fit3 to png file
png("Fit3_all.png", width=6, height=6, units='in', res=300)
layout(matrix(1:4, ncol = 2))
plot(fit3)
layout(1)
dev.off()

# Bonus Questions
# 1. polynomial fit for V6 (number of rooms)
polyfit <- lm(V14 ~ V6, data = x)
summary(polyfit)
polyfit2 <- update(polyfit, .~. + I(V6^2))
summary(polyfit2)
polyfit3 <- update(polyfit2, .~. + I(V6^3))
summary(polyfit3)
polyfit6 <- update(polyfit3, .~. + I(V6^4) + I(V6^5) + I(V6^6))
summary(polyfit6)
# R2 increases with increasing polynomial terms

# 2. Ridge Regression
library(MASS)
ridgefit <- lm.ridge(V14 ~ ., data = x)
summary(ridgefit)
ridgefit
fit <- lm(V14 ~ ., data = x)
summary(fit)
#  in this case the coefficients are similar for ridge and normal regression