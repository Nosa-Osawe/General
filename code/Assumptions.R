###
### Statistical Analysis and Reporting in R
### Jacob O. Wobbrock, Ph.D.
### The Information School
### University of Washington
### March 12, 2019
### Updated: 08/02/2023
###

###
### Assumptions.R
### (Tests of Assumptions: Normality, Homoscedasticity, Sphericity)
###


###
### Tests of ANOVA Assumptions
###

## Shapiro-Wilk conditional normality test
# (on the response within each condition)
# assume df has two factors (X1,X2) each w/two levels (a,b) and continuous response Y
df <- read.csv("data/Assumptions/2F2LBs_normal.csv")
shapiro.test(df[df$X1 == "a" & df$X2 == "a",]$Y) # condition a,a
shapiro.test(df[df$X1 == "a" & df$X2 == "b",]$Y) # condition a,b
shapiro.test(df[df$X1 == "b" & df$X2 == "a",]$Y) # condition b,a
shapiro.test(df[df$X1 == "b" & df$X2 == "b",]$Y) # condition b,b

## Anderson-Darling conditional normality test
# (on the response within each condition)
# assume df has two factors (X1,X2) each w/two levels (a,b) and continuous response Y
library(nortest)
df <- read.csv("data/Assumptions/2F2LBs_normal.csv")
ad.test(df[df$X1 == "a" & df$X2 == "a",]$Y) # condition a,a
ad.test(df[df$X1 == "a" & df$X2 == "b",]$Y) # condition a,b
ad.test(df[df$X1 == "b" & df$X2 == "a",]$Y) # condition b,a
ad.test(df[df$X1 == "b" & df$X2 == "b",]$Y) # condition b,b


## Shapiro-Wilk and Anderson-Darling normality tests
# (on between-Ss residuals)
# assume df has two between-Ss factors (X1,X2) each w/two levels (a,b) and continuous response Y
library(nortest)
df <- read.csv("data/Assumptions/2F2LBs_normal.csv")
m = aov(Y ~ X1*X2, data=df) # make anova model
r = residuals(m) # extract model residuals
sum(r); mean(r) # both should be ~0
par(mfrow=c(3,1))
  plot(r[1:length(r)], main="Residual plot"); abline(h=0) # should look random
  hist(r, main="Histogram of residuals") # should look normal
  qqnorm(r); qqline(r) # Q-Q plot
par(mfrow=c(1,1))
shapiro.test(r) # Shapiro-Wilk test
ad.test(r) # Anderson-Darling test

## Shapiro-Wilk and Anderson-Darling normality tests
# (on within-Ss residuals)
# assume df has two within-Ss factors (X1,X2) each w/two levels (a,b) and continuous response Y
library(afex)
library(nortest)
df <- read.csv("data/Assumptions/2F2LWs_normal.csv")
df$S = factor(df$S)
df$X1 = factor(df$X1)
df$X2 = factor(df$X2)
m = aov_ez(dv="Y", within=c("X1","X2"), id="S", type=3, data=df) # make rm-anova model
r = residuals(m$lm)
sum(r); mean(r) # both should be ~0
par(mfrow=c(3,1))
  plot(r[1:length(r)], main="Residual plot"); abline(h=0) # should look random
  hist(r, main="Histogram of residuals") # should look normal
  qqnorm(r); qqline(r) # Q-Q plot
par(mfrow=c(1,1))
shapiro.test(r) # Shapiro-Wilk test
ad.test(r) # Anderson-Darling test

## Shapiro-Wilk and Anderson-Darling normality tests
# (on residuals from linear mixed models)
library(lme4)
library(lmerTest)
library(nortest)
df <- read.csv("data/Assumptions/2F2LWs_normal.csv")
df$S = factor(df$S)
df$X1 = factor(df$X1)
df$X2 = factor(df$X2)
m = lmer(Y ~ X1*X2 + (1|S), data=df) # make linear mixed model
r = residuals(m)
sum(r); mean(r) # both should be ~0
par(mfrow=c(3,1))
  plot(r[1:length(r)], main="Residual plot"); abline(h=0) # should look random
  hist(r, main="Histogram of residuals") # should look normal
  qqnorm(r); qqline(r) # Q-Q plot
par(mfrow=c(1,1))
shapiro.test(r) # Shapiro-Wilk test
ad.test(r) # Anderson-Darling test


## Levene's test for homoscedasticity 
# assume df has two factors (X1,X2) each w/two levels (a,b) and continuous response Y
library(car) # for leveneTest, Anova
df <- read.csv("data/Assumptions/2F2LBs_normal.csv")
leveneTest(Y ~ X1*X2, data=df, center=mean)

## Brown-Forsythe test for homoscedasticity
# assume df has two factors (X1,X2) each w/two levels (a,b) and continuous response Y
library(car) # for leveneTest, Anova
df <- read.csv("data/Assumptions/2F2LBs_normal.csv")
leveneTest(Y ~ X1*X2, data=df, center=median)

## Bartlett's test for homoscedasticity
# assume df has two factors (X1,X2) each w/two levels (a,b) and continuous response Y
library(car) # for Anova
df <- read.csv("data/Assumptions/2F2LBs_normal.csv")
bartlett.test(Y ~ interaction(X1,X2), data=df)

## Fligner-Killeen test for homoscedasticity
# assume df has two factors (X1,X2) each w/two levels (a,b) and continuous response Y
library(car) # for Anova
df <- read.csv("data/Assumptions/2F2LBs_normal.csv")
fligner.test(Y ~ interaction(X1,X2), data=df)

# with any of the above tests, if a violation occurs and only a t-test is needed, use a Welch t-test
t.test(Y ~ X1, data=df, var.equal=FALSE) # Welch t-test
# if a violation occurs and an ANOVA is needed, use a White-adjusted ANOVA
m = aov(Y ~ X1*X2, data=df)
Anova(m, type=3, white.adjust=TRUE)



## Mauchly's test of sphericity
# assume df has subjects (S), one between-Ss factor (X1), and one within-Ss factor (X2)
library(ez) # for ezANOVA
df <- read.csv("data/Assumptions/2F23LMs_mauchly.csv")
df$S = factor(df$S) # Subject id is nominal
df$X1 = factor(df$X1)
df$X2 = factor(df$X2)
m = ezANOVA(dv=Y, between=c(X1), within=c(X2), wid=S, type=3, data=df) # use c() for >1 factors
m$Mauchly # p<.05 indicates a sphericity violation for within-Ss effects


