###
### Statistical Analysis and Reporting in R
### Jacob O. Wobbrock, Ph.D.
### The Information School
### University of Washington
### November 14, 2018
### Updated: 08/02/2023
###

###
### Proportions.R
### (Tests of Proportion and Association)
###


## 
## One Sample
##

## Binomial test
# df is a long-format data table w/columns for subject (S) and 2-category outcome (Y)
binomial_df <- read.csv("C:\\Users\\HP\\Documents\\General\\data\\tutorial_data\\Proportions\\0F0LBs_binomial.csv")
binomial_df$S = factor(binomial_df$S) # Subject id is nominal (unused)
binomial_df$Y = factor(binomial_df$Y) # Y is an outcome of 2 categories
xt = xtabs( ~ Y, data=binomial_df) # make counts
binom.test(xt, p=0.5, alternative="two.sided")


## Multinomial test
# df is a long-format data table w/columns for subject (S) and N-category outcome (Y)
install.packages("XNomial")
install.packages("RVAideMemoire")

library(XNomial) # for xmulti
df_multinom <- read.csv("C:\\Users\\HP\\Documents\\General\\data\\tutorial_data\\Proportions\\0F0LBs_multinomial.csv")
df_multinom$S = factor(df_multinom$S) # Subject id is nominal (unused)
df_multinom$Y = factor(df_multinom$Y) # Y is an outcome of ≥2 categories
xt = xtabs( ~ Y, data=df_multinom) # make counts
xmulti(xt, rep(1/length(xt), length(xt)), statName="Prob")

# the following gives the same result
library(RVAideMemoire) # for multinomial.test
multinomial.test(df$Y)

## Multinomial post hoc test
# xt is a table of counts for each category of Y
library(RVAideMemoire) # for multinomial.multcomp
multinomial.multcomp(xt, p.method="holm") # xt shows levels


## One-Sample Chi-Squared test
# df is a long-format data table w/columns for subject (S) and N-category outcome (Y)
df <- read.csv("data/Proportions/0F0LBs_multinomial.csv")
df$S = factor(df$S) # Subject id is nominal (unused)
df$Y = factor(df$Y) # Y is an outcome of ≥2 categories
xt = xtabs( ~ Y, data=df) # make counts
chisq.test(xt)

## Chi-Squared post hoc test
# xt is a table of counts for each category of Y
library(RVAideMemoire) # for chisq.multcomp
chisq.multcomp(xt, p.method="holm") # xt shows levels
# for the Chi-Squared values, use qchisq(1-p, df=1), where p is the pairwise p-value.


## One-sample post hoc tests
# A different kind of post hoc test for one sample. For Y's response categories (x,y,z), 
# test each proportion against chance.
x = binom.test(sum(df$Y == "x"), nrow(df), p=1/3) # proportion of "x" rows
y = binom.test(sum(df$Y == "y"), nrow(df), p=1/3) # proportion of "y" rows
z = binom.test(sum(df$Y == "z"), nrow(df), p=1/3) # proportion of "z" rows
p.adjust(c(x$p.value, y$p.value, z$p.value), method="holm")



##
## Two Samples
##

## Fisher's exact test
# df is a long-format data table w/subject (S), categorical factor (X), and categorical outcome (Y)
df <- read.csv("data/Proportions/1F2LBs_xnomial.csv")
df$S = factor(df$S) # Subject id is nominal (unused)
df$X = factor(df$X) # X is a factor of m ≥ 2 levels
df$Y = factor(df$Y) # Y is an outcome of n ≥ 2 categories
xt = xtabs( ~ X + Y, data=df) # make m × n crosstabs
fisher.test(xt)

## Fisher's post hoc test
# xt is an m × n crosstabs with categories X and Y
library(RVAideMemoire) # for fisher.multcomp
fisher.multcomp(xt, p.method="holm")


## G-test
# df is a long-format data table w/subject (S), categorical factor (X), and categorical outcome (Y)
library(RVAideMemoire) # for G.test
df <- read.csv("data/Proportions/1F2LBs_xnomial.csv")
df$S = factor(df$S) # Subject id is nominal (unused)
df$X = factor(df$X) # X is a factor of m ≥ 2 levels
df$Y = factor(df$Y) # Y is an outcome of n ≥ 2 categories
xt = xtabs( ~ X + Y, data=df) # make m × n crosstabs
G.test(xt)

## G-test post hoc test
# xt is an m × n crosstabs with categories X and Y
library(RVAideMemoire) # for G.multcomp
G.multcomp(xt, p.method="holm") # xt shows levels


## Two-sample Chi-Squared test
# df is a long-format data table w/subject (S), categorical factor (X), and categorical outcome (Y)
df <- read.csv("data/Proportions/1F2LBs_xnomial.csv")
df$S = factor(df$S) # Subject id is nominal (unused)
df$X = factor(df$X) # X is a factor of m ≥ 2 levels
df$Y = factor(df$Y) # Y is an outcome of n ≥ 2 categories
xt = xtabs( ~ X + Y, data=df) # make m × n crosstabs
chisq.test(xt)

## Chi-Squared post hoc test
# xt is an m × n crosstabs with categories X and Y
library(RVAideMemoire) # for chisq.multcomp
chisq.multcomp(xt, p.method="holm") # xt shows levels
# for the Chi-Squared values, use qchisq(1-p, df=1), where p is the pairwise p-value.


## Two-sample post hoc tests
# A different kind of post hoc test for two samples. For X's categories (a,b) and Y's 
# response categories (x,y,z), test each proportion of Y within each level of X against 
# chance.
# df is a long-format data table w/subject (S), categorical factor (X) and outcome (Y).
ax = binom.test(sum(df[df$X == "a",]$Y == "x"), nrow(df[df$X == "a",]), p=1/3)
ay = binom.test(sum(df[df$X == "a",]$Y == "y"), nrow(df[df$X == "a",]), p=1/3)
az = binom.test(sum(df[df$X == "a",]$Y == "z"), nrow(df[df$X == "a",]), p=1/3)
p.adjust(c(ax$p.value, ay$p.value, az$p.value), method="holm")

bx = binom.test(sum(df[df$X == "b",]$Y == "x"), nrow(df[df$X == "b",]), p=1/3)
by = binom.test(sum(df[df$X == "b",]$Y == "y"), nrow(df[df$X == "b",]), p=1/3)
bz = binom.test(sum(df[df$X == "b",]$Y == "z"), nrow(df[df$X == "b",]), p=1/3)
p.adjust(c(bx$p.value, by$p.value, bz$p.value), method="holm")


##
## Within-subjects samples
##

## Symmetry Test
# df is a long-format data table w/repeated subject (S), categorical factor (X), and categorical outcome (Y)
library(coin) # for cochran.qtest
df <- read.csv("data/Proportions/1F2LWs_xnomial.csv")
df$S = factor(df$S) # Subject id is nominal
df$X = factor(df$X) # X is a factor of m ≥ 2 levels
df$Y = factor(df$Y) # Y is an outcome of n ≥ 2 categories
symmetry_test(Y ~ X | S, data=df)

## Example: Each season, a respondent is asked what ice cream flavor they prefer: vanilla, chocolate, or strawberry.
# df is a long-format data table w/repeated subject (S), categorical factor (Season), and categorical outcome (Pref)
df <- read.csv("data/Proportions/seasonal_ice_cream_prefs.csv")
df$S = factor(df$S)
df$Season = factor(df$Season)
df$Pref = factor(df$Pref)
symmetry_test(Pref ~ Season | S, data=df)

# post hoc pairwise comparisons
df2 <- df[df$Season == "fall" | df$Season == "spring",]
df2$Season = factor(df2$Season)
fa_sp = pvalue(symmetry_test(Pref ~ Season | S, data=df2))

df2 <- df[df$Season == "fall" | df$Season == "summer",]
df2$Season = factor(df2$Season)
fa_su = pvalue(symmetry_test(Pref ~ Season | S, data=df2))

df2 <- df[df$Season == "fall" | df$Season == "winter",]
df2$Season = factor(df2$Season)
fa_wi = pvalue(symmetry_test(Pref ~ Season | S, data=df2))

df2 <- df[df$Season == "spring" | df$Season == "summer",]
df2$Season = factor(df2$Season)
sp_su = pvalue(symmetry_test(Pref ~ Season | S, data=df2))

df2 <- df[df$Season == "spring" | df$Season == "winter",]
df2$Season = factor(df2$Season)
sp_wi = pvalue(symmetry_test(Pref ~ Season | S, data=df2))

df2 <- df[df$Season == "summer" | df$Season == "winter",]
df2$Season = factor(df2$Season)
su_wi = pvalue(symmetry_test(Pref ~ Season | S, data=df2))

p.adjust(c(
  fa_sp,
  fa_su,
  fa_wi,
  sp_su,
  sp_wi,
  su_wi), method="holm")

