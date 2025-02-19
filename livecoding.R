library(effects)

# read data
d <- read.csv("https://t1p.de/howell_raw")

# inspect data
plot(d$height, d$weight)

# exclude children
d <- subset(d, age >= 18)

# linear model
m <- lm(height ~ weight, data = d)
summary(m)

# intercept and slope
intercept <- coef(m)[1]
slope     <- coef(m)[2]

# plot
plot(d$weight, d$height, col = "grey")
abline(a = intercept, b = slope)

# make predictions by plugging in
# values into the basic regression formula
intercept + slope * 200


hist(resid(m))
qqnorm(resid(m))


# dative alternation example
library(languageR)
data("dative")

m_dative <- glm(RealizationOfRecipient ~ AnimacyOfRec,
    data = dative,
    family = binomial)

summary(m_dative)

plot(allEffects(m_dative))


# corpus example ----------------------------------------------------------

maed <- read.csv("https://t1p.de/example_maedchen")
maed <- maed[-which(is.na(maed$dist)),]

# outcome variable is factor
maed$anaph <- factor(maed$anaph)

# plot
plot(maed$anaph, maed$dist)

# center data
maed$dist_c <- maed$dist - mean(maed$dist)

# model:
m_maed <- glm(anaph ~ dist_c, 
              data = maed, 
              family = binomial)
summary(m_maed)
plot(allEffects(m_maed))

# null model:
m_null <- glm(anaph ~ 1, 
              data = maed, 
              family = binomial)
anova(m_maed, m_null)
