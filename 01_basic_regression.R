# install and load packages
library(tidyverse)
library(car)
library(skimr)
library(beeswarm)
library(scatterplot3d)
library(parameters)
library(performance)

# read data ---------------------------------------------------------------

# The hw dataset contains height and weight data of
# the !Kung San, an indigenous people living in the
# Kalahari desert, collected by Nancy Howell from the
# 1960s onwards. The use of the data here is inspired by
# Richard McElreath's "Statistical Rethinking"; the data
# exploration part is inspired by Bodo Winter's "Statistics
# for Linguists".

# Data from https://tspace.library.utoronto.ca/handle/1807/10395;
# version used here from McElreath (2020): https://github.com/rmcelreath/rethinking/blob/master/data/Howell1.csv
hw <- read_csv("data/howell.csv")
# hw <- read_delim("https://raw.githubusercontent.com/rmcelreath/rethinking/master/data/Howell1.csv", 
#                 delim = ";")

# backup
hw_backup <- hw

# only adults
hw <- subset(hw, age >= 18)

# sort by "height"
hw <- hw %>% arrange(height)

# data structure
skim(hw)
str(hw)

# visualization: scatterplot
with(hw, plot(weight, height))

# visualization: histogram
hist(hw$height)
hist(hw$height, probability = T)
lines(density(hw$height), col = "red", lwd = 2)

# back to scatterplot
with(hw, plot(weight, height))

# fit a model
m01 <- lm(height ~ weight, data = hw)

# add regression line to the plot
abline(m01, col = "red")

# show residuals
sapply(1:nrow(hw), 
       function(i)
         lines(x = rep(hw$weight[i], 2),
               y = c(hw$height[i], fitted(m01)[i]), 
               col = "blue")
)

# summarize the model
summary(m01)


# plot the model
# reminder: basic formula is y = a + b * x,
# with a being the intercept, and b the slope
coef(m01) # intercept + slope
a <- coef(m01)[1]
b <- coef(m01)[2] 

# Regression line through scatterplot from original data...
# x and y limits are adjusted so that we see the
# intercept (x = 0)
with(hw, plot(weight, height, col = "lightgrey", xlim = c(-10, 65),
              ylim = c(110, 180)))
abline(a = a, b = b)
abline(m01)
points(x = 0, y = a, pch = 15, cex = 3, col = "orange") # intercept
abline(v = 0, col = "orange", lty = 2)

# predicted + observed distribution
predict(m01) %>% hist(probability = T, main = "predicted value",
                      ylim = c(0, 0.065))
predict(m01) %>% density %>% lines(col = "darkblue", lwd = 2)
hw$height %>% density %>% lines(col= "darkred", lwd = 2, lty = 2)


#########################
### explore residuals ###
#########################
summary(m01)

# Smallest and largest residual:
resid(m01) %>% min
resid(m01) %>% max

# Quantiles, sometimes also called percentiles,
# are points in the data below which a certain
# proportion of our data fall. For example,
# none of our data is smaller than -19.7,
# 25% are below 2.88, 50% are below 0.02, etc.
resid(m01) %>% quantile() 

resid(m01) %>% sort %>% plot(type = "n")
for(i in 1:length(resid(m01))) {
  if(i %in% round(seq(1, length(resid(m01)), length.out = 5))) {
    col = "red"
  } else {
    col = rgb(0,1,1,.2)
  }
  text(x = i, y = sort(resid(m01))[i], label = i, col = col)
}

sapply(1:length(quantile(resid(m01))),
       function(i) abline(h = quantile(resid(m01)), col = "grey75", lty = 3)[i])
hist(resid(m01), breaks = quantile(resid(m01)))

# Note that
resid(m01) %>% quantile() 

# yields essentially the same result as the
# residuals line on
summary(m01)


#############################
## calculate R^2 manually ###
#############################

# R^2 = model sum of squares / total sum of squares

# model sum of squares (SSM): 
# difference between mean of Y and the
# regression line

# in visualized form:
with(hw, plot(weight, height, col = "grey"))
abline(m01, col = "grey20")
abline(h = mean(hw$height), lty = 2) # mean

sapply(1:nrow(hw), function(i)
  lines(x = rep(hw$weight[i], 2),
        c(mean(hw$height), fitted(m01)[i]), col = "purple")
)

title("Model sum of squares\nDifference between mean of Y and regression line")


# model sum of squares
ssm <- sum((fitted(m01) - mean(hw$height))^2)


# total sum of squares (SST): Difference between
# observed data and the mean of Y

# in visualized form:
with(hw, plot(weight, height, col = "grey"))
abline(m01, col = "grey20")
abline(h = mean(hw$height), lty = 2) # Mittelwert

sapply(1:nrow(hw), function(i)
  lines(x = rep(hw$weight[i], 2),
        c(mean(hw$height), hw$height[i]), col = "darkorange")
)

title("Total sum of squares\nDifference between mean of Y and observed data")

# get total sum of squares
sst <- sum((hw$height - mean(hw$height))^2)


# R^2
ssm / sst

# compare this to what we get from the 
# summary of our model:
summary(m01)$r.squared

(ssm / sst) == summary(m01)$r.squared


# On a side note: SST is the sum of squared errors (ssm)
# of the null model:

m0 <- lm(height ~ 1, data = hw) # only intercept
summary(m0)

# plot null model
with(hw, plot(weight, height, col = "grey"))
abline(m0, col = "orange", lwd = 2) # regression line = mean

# also, because we just have one predictor, the R^2
# value represents the square of the simple correlation
# between height and weight:
cor.test(hw$height, hw$weight, method = "pearson")$statistic
summary(m01)$r.squared %>% sqrt # square root of R^2
cor.test(hw$height, hw$weight, method = "pearson")

# and the F-value is the t-value from cor.test
# squared:
cor.test(hw$height, hw$weight, method = "pearson")$statistic^2
summary(m01)$fstatistic

#################################
## calculate F-value manually ##
################################

# F = mean model sum of squares / mean residual sum of squares
# residual sum of squares: Difference between
# observed data and regression line

# as above:
with(hw, plot(weight, height, col = "grey"))
abline(h = mean(hw$height), col = "blue", lty = 2)
sapply(1:nrow(hw), 
       function(i)
         lines(x = rep(hw$weight[i], 2),
               y = c(hw$height[i], fitted(m01)[i]), 
               col = "blue")
)
title("Residual sum of squares\nDifference between observed data and regression line")

# one randomly selected point to visualize
# explained and unexplained variance:

with(hw, points(weight[351], height[351], col = "red", pch = 20))
abline(m01, col = "orange", lwd = 2)
lines(x = rep(hw$weight[351], 2),
      y = c(hw$height[351], mean(hw$height)), col = "red")


# residual sum of squares
ssr <- sum((hw$height - fitted(m01))^2)


# to get the *mean* model sum of squares and the
# *mean* residual sum of squares, we have to divide
# by the degrees of freedom.

# For SSM, the degrees of freedom are simply the
# number of variables in the model (here: 1)
# For SSR, the degrees of freedom are the number of
# observations minus the number of estimated parameters
# (here: intercept and weight, i.e. two).
# See also 
summary(lm(m01)) # bottom line: "1 and 350 DF"

# the number of observations and the number of
# parameters can also be gauged from:
nobs(m01)
n_parameters(m01) # from parameters package

# mean model sum of squares
msm <- ssm / 1

# mean residual sum of squares
msr <- ssr / 350

# F = msm / msr
msm / msr

# compare:
summary(lm(m01))$fstatistic


##########################
### relationship F/R^2 ###
##########################

# F = msm / msr
f <- msm / msr

# R^2 = ssm / sst
r2 <- ssm / sst

# R^2 = 1 - (1 + F * ((p-1)/(n-p)))^-1
1- (1 + (f *((2 - 1) / (352 - 2))))^-1

# compare: 
r2


#########
## AIC ##
#########

# AIC = -2 log-likelihood + 2k, or 2k-2log-likelihood
(-2 * logLik(m01)) +( 2*n_parameters(m01))
-2*as.numeric(logLik(m01)) + (2*2)
AIC(m01, k = 2)  

a + b * 150




##########################
##Checking assumptions ##
##########################

# a) Normally distributed residuals
qqnorm(resid(m01)) 
qqline(resid(m01))
hist(resid(m01))

# b) Homoskedasticity of residual variance
plot(resid(m01), fitted(m01))
check_heteroskedasticity(m01) # from package "parameters"

# c) no collinearity between predictors
# only one predictor here, hence irrelevant

# d) no overly influential datapoints.
# Function dfbeta() gives DFBeta values, i.e.
# values with which the coefficients
# have to be adjusted if a particular data point 
# is excluded. 
(coef(m01)[2] + dfbeta(m01)[,2]) %>% plot
abline(h = coef(m01)[2], col = "red", lty = 2)

# Rule of thumb: +/- half absolute value
# of the coefficient is reason for concern
coef(m01)[2] + coef(m01)[2] / 2 # clearly above the highest point in our data
coef(m01)[2] - coef(m01)[2] / 2 # clearly below the lowest point in our data


# also, car::influencePlot offers some diagnostics.
# It identifies points that have unusually small or 
# large values on an explanatory variable. 
# They *can* have a significant effect on the regression slopes.
influencePlot(m01)
plot(hw$height, hw$weight, col = "darkgrey")
points(hw[c(352, 350, 309, 6),]$height, hw[c(352, 350, 309, 6),]$weight, col="red", pch = 20)


# e) Independence:
# this assumption is fulfilled here because every
# data point represents one person.




################################
## challenge: plug in values ##
###############################

# intercept and slope:
b0 <- m01$coefficients[1] # the intercept, same as coef(m)[1]
b1 <- m01$coefficients[2] # the slope, same as coef(m)[2]

# visualize:
plot(hw$weight, hw$height, ylim = c(0,255), xlim = c(0,160),
     col = "darkgrey", xlab="weight", ylab="height")
abline(v = 0, col = "blue", lty = 3)
abline(a = b0, b = b1, col = "darkorange", lwd = 2)

# plug in values:
points(x = 150, y = 249.63, pch = 15, col = "red")
b0 + (b1 * 150)


###########################
#  Linear transformation: #
#     center data         #
###########################

# again: regression line through scatterplot of original data...
# set x and y limits so that we see the intercept (at x = 0)

with(hw, plot(weight, height, col = "lightgrey", xlim = c(-10, 65),
              ylim = c(110, 180)))
abline(a = a, b = b)
points(x = 0, y = a, pch = 15, cex = 3, col = "orange") # intercept
abline(v = 0, col = "orange", lty = 2)
text(0, 130, expression(bold("weight = 0 ?!?")), col = "orange")



# Center both variables:
hw$weight_c <- hw$weight - mean(hw$weight)
hw$height_c <- hw$height - mean(hw$height)

# compare:
plot(hw$weight, hw$height)
plot(hw$weight_c, hw$height_c)

# new model:
m02 <- lm(height_c ~ weight_c, data = hw)
summary(m02)
summary(m01) # for comparison

# plot:
with(hw, plot(weight_c, height_c, col = "lightgrey"))
abline(m02, col = "orange", lwd = 2)
points(x = 0, y = coef(m02)[1], pch = 15, cex = 3, col = "orange")
abline(v = 0, lty = 2, col = "orange")
text(2, mean(hw$weight_c), 
     paste0("mean weight: ", round(mean(hw$weight), 2), " kg"),
     col = "orange", adj = 0)

# what does the beta coefficient tell us?
coef(m02)
b <- coef(m02)[2]

# if "weight" increases by one unit,
# then "height" increases ~0.9.
# compare
a <- coef(m02)[1] # intercept


# Model: a + b x
# Let x be 1:
(a + b * 1)

# Let x be 2:
x = 2
a + b * x
points(x = x, y = (a + b * x), pch = 20, col = "red", cex = 2)

# Let x be 3:
x = 3
a + b * x
points(x = x, y = (a + b * x), pch = 20, col = "red", cex = 2)

# etc.!


# which increase in height is to be expected when
# weight increases by one unit?
a          # intercept: predicted value when
# weight is at the mean (~ 45kg), 
# compare mean(hw$weight)
a+b        # predicted value when weight is
# increased by one unit (a+b * 1)


###########################
#  Linear transformation: #
#      standardizing      #
###########################

# by standardizing, or "z-scoring", we divide
# the centered variable by the standard deviation
# of the sample.

hw$weight_std <- hw$weight_c / sd(hw$weight_c)
hw$height_std <- hw$height_c / sd(hw$height_c)

# compare:

plot(hw$height_c, hw$weight_c)
plot(hw$height_std, hw$weight_std)


# new model:
m03 <- lm(height_std ~ weight_std, data = hw)
summary(m03)
summary(m02) # for comparison


# plot:
with(hw, plot(weight_std, height_std, col = "lightgrey"))
abline(m03, col = "orange", lwd = 2)
points(x = 0, y = coef(m03)[1], pch = 15, cex = 3, col = "orange")
abline(v = 0, lty = 2, col = "orange")
text(0.2, mean(hw$weight_std), 
     paste0("mean weight: ", round(mean(hw$weight), 5), " kg"),
     col = "orange", adj = 0)
abline(v = 2, col = "blue", lty = 2)
text(x = 1.3, y = 2, "1 sd\naway\nfrom\nmean", 
     col = "blue", cex=.8)
abline(v = 1, col = "darkgreen", lty = 2)
text(x = 2.3, y = 2, "2 sd\naway\nfrom\nmean", 
     col = "darkgreen", cex=.8)

# get intercept and slope of current model:
a <- unname(coef(m03)[1])
b <- unname(coef(m03)[2])

# explore data:
a + b # predicted value when weight is increased
# by one unit

points(x = 1, y = a+b, col="red", pch = 15)

a + b*2 # predicted value when weight is increased
# by two units
points(x = 2, y = a+b*2, col="red", pch = 15)



################################
##  Nonlinear transformation: ##
##  Logarithmization          ##
################################

hw$log_height <- log(hw$height)
hw$log_weight <- log(hw$weight)

plot(hw$weight, hw$height)
plot(hw$log_weight, hw$log_height)

# new model
m04 <- lm(log_height ~ log_weight, data = hw)
summary(m04)



#########################################
## Models with more than one predictor ##
#########################################

# New model: height as a function of age and gender.

# This time, we only take kids into account:
hw <- hw_backup
hw <- subset(hw, age < 18)
str(hw)

# add "gender" category
hw$gender <- ifelse(hw$male==0, "female", "male")

# plot
with(hw, plot(age, height))

# model
mm1 <- lm(height ~ age + gender, data = hw)

# Summary
summary(mm1)

# plot (1)
with(hw, plot(age, height, col = "darkgrey"))
abline(mm1) # what does the warning tell us?
# --> of course there are more than two coefficients...
# i.e. more than one line was fit.

# individual coefficients
coef(mm1)
a        <- coef(mm1)[1]
b_age    <- coef(mm1)[2]
b_gender <- coef(mm1)[3] 

# predict the height of a 15-year old girl:
coef(mm1)

a + b*15 + 0


# 3d plot with regression plane
hw$gender2 <- ifelse(hw$gender=="male", 0, 1)
s3d <- scatterplot3d(x = as.numeric(as.factor(hw$gender))-1,
                     y = hw$age, 
                     z = hw$height, type = "h", highlight.3d = T,
                     xlab = "gender", ylab = "height", zlab = "age",
                     angle = 35)
s3d$plane3d(mm1, draw_polygon = T, draw_lines = T) 


# rotate:
for(i in 1:36) {
  s3d <- scatterplot3d(x = as.numeric(as.factor(hw$gender))-1,
                       y = hw$age, 
                       z = hw$height, type = "h", highlight.3d = T,
                       xlab = "gender", ylab = "height", zlab = "age",
                       angle = i*10)
  s3d$plane3d(mm1, draw_polygon = T, draw_lines = T)
  Sys.sleep(.5)
}


