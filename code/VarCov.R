###
### Statistical Analysis and Reporting in R
### Jacob O. Wobbrock, Ph.D.
### The Information School
### University of Washington
### October 23, 2021
### Updated: 08/02/2023
###

###
### VarCov.R
### (Variance-Covariance Structures)
###

### R code for common covariance structures. 
### See ?nlme::corClasses, ?nlme::varClasses.
### See also https://rpubs.com/samuelkn/CovarianceStructuresInR
### See also https://www.ibm.com/docs/en/spss-statistics/26.0.0?topic=mixed-covariance-structure-list-command
### See also https://www.ibm.com/docs/en/spss-statistics/26.0.0?topic=statistics-covariance-structures

library(nlme) # lme
library(lme4) # lmer
library(lmerTest)
library(MuMIn) # AICc
library(car) # Anova
library(emmeans) # emmeans

## Dummy data has (S)ubjects, one repeated factor X with three levels (a,b,c), and continuous response Y.
df <- read.csv("data/Parametric/1F3LWs.csv")
df$S = factor(df$S) # subjects
df$X = factor(df$X) # factor
contrasts(df$X) <- "contr.sum"
View(df)

## LMER model for comparison:
m0 = lmer(Y ~ X + (1|S), data=df) # random intercept for each subject

# NOTES ON LME:
#   In nlme::lme, the "correlation" parameter specifies off-diagonal covariances, while the "weights" parameter
#   specifies the on-diagonal variances. If nlme::lme fails to converge, try adding the "control" parameter to
#   the lme call: control=list(maxIter=100, msMaxIter=100, niterEM=100, msMaxEval=100, opt="optim"). Usually works!

##
## VARIANCE-COVARIANCE STRUCTURES
## ...even more available at ?nlme::corClasses
##

## ID. Scaled identity. This is a scaled identity matrix.
## All variances are the same, and all covariances are zero. DEFAULT for nlme::lme.
m = lme(Y ~ X, random=~1|S, data=df, na.action=na.omit, weights=varIdent(form=~X)) # weights param optional

## DIAG. Diagonal. This is a diagonal structure with heterogenous variance. Also called "Independence."
## All variances are different, and all covariances are zero.
m = lme(Y ~ X, random=~1|S, data=df, na.action=na.omit, weights=varIdent(form=~1|X))

## CS. Compound symmetry. This structure has constant variance and constant covariance. Also called "Exchangeable."
## All variances are equal, and separately, all covariances are equal.
m = lme(Y ~ X, random=~1|S, data=df, na.action=na.omit, correlation=corCompSymm(form=~1|S))

## CSH. Heterogenous compound symmetry. This structure has non-constant variance and constant correlation.
## Like CS but the on-diagonal variances can differ from each other.
m = lme(Y ~ X, random=~1|S, data=df, na.action=na.omit, correlation=corCompSymm(form=~1|S), weights=varIdent(form=~1|X))

## AR1. First-order autoregressive.
## All variances are equal, and correlation among repeats decreases the further apart they are.
m = lme(Y ~ X, random=~1|S, data=df, na.action=na.omit, correlation=corAR1(form=~1|S))

## ARH1. Heterogenous first-order autoregressive.
## Like AR1 but the on-diagonal variances can differ from each other.
m = lme(Y ~ X, random=~1|S, data=df, na.action=na.omit, correlation=corAR1(form=~1|S), weights=varIdent(form=~1|X))

## ARMA11. Autoregressive moving average (1,1).
## Like AR1 but with a moving average. The p parameter determines the autoregressive order. The q parameter determines the moving average.
m = lme(Y ~ X, random=~1|S, data=df, na.action=na.omit, correlation=corARMA(form=~1|S, p=1, q=1)) # (p,q)=(1,0) is AR1

## TP. Toeplitz.
## A more general version of AR1. The correlation between adjacent elements is homogenous across pairs of adjacent elements. 
## The correlation between elements separated by a third is again homogenous, and so on.
m = lme(Y ~ X, random=~1|S, data=df, na.action=na.omit, correlation=corARMA(form=~1|S, p=2, q=0))

## TPH. Heterogenous Toeplitz.
## Like TP but the on-diagonal variances can differ from each other.
m = lme(Y ~ X, random=~1|S, data=df, na.action=na.omit, correlation=corARMA(form=~1|S, p=2, q=0), weights=varIdent(form=~1|X))

## UN. Unstructured. This is a completely general covariance matrix.
## No pattern, all variances and covariances are separately fit. Most computationally intensive.
m = lme(Y ~ X, random=~1|S, data=df, na.action=na.omit, correlation=corSymm(form=~1|S), weights=varIdent(form=~1|X))


## Once a model is built, run summary to inspect it:
summary(m)

VarCorr(m)
getVarCov(m, type="random.effects") # lme models only
getVarCov(m, type="conditional") # lme models only
getVarCov(m, type="marginal") # lme models only

# information criterion -- lower is better
-2*logLik(m)
AIC(m)
AICc(m)
BIC(m)

# significance tests
anova(m0, type="I")
anova(m0, type="III")
Anova(m0, type=3, test.statistic="F") # only lmer produces "F", lme ignores
Anova(m0, type=3, test.statistic="Chisq")

anova(m, type="sequential") # Type I SSq
anova(m, type="marginal")   # Type III SSq
Anova(m, type=3, test.statistic="Chisq")


## Post hoc pairwise comparisons
## See https://cran.r-project.org/web/packages/emmeans/vignettes/models.html
# valid adjust strings:  "tukey", "scheffe", "sidak", "dunnettx", "mvt", 
#                        "holm", "hochberg", "hommel", "bonferroni", 
#                        "BH", "BY", "fdr", "none"
# valid LMER df mode strings: “satterthwaite”, “kenward-roger”, “asymptotic”
summary(emmeans(m0, pairwise ~ X, adjust="holm", mode="kenward-roger")) # lmer model

# valid LME df mode strings: “containment”, “satterthwaite”, “appx-satterthwaite”, 
#                            “auto”, “boot-satterthwaite”, “asymptotic”
summary(emmeans(m, pairwise ~ X, adjust="holm", mode="containment")) # lme model

