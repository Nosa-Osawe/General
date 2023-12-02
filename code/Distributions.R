###
### Statistical Analysis and Reporting in R
### Jacob O. Wobbrock, Ph.D.
### The Information School
### University of Washington
### August 2, 2023
###

###
### Distributions.R
### normal, lognormal, Poisson, negative binomial, exponential, Gamma
###
library(ggplot2)
library(ggthemes)
library(scales)


###
### Distribution Tests
###


##
## Normal distribution
##
## assume df has one factor (X) w/two levels (a,b) and continuous response Y
library(MASS) # for fitdistr
df <- read.csv("data/Distributions/1F2LBs_normal.csv")
fa = fitdistr(df[df$X == "a",]$Y, "normal")$estimate # create fit for X.a
ks.test(df[df$X == "a",]$Y, "pnorm", mean=fa[1], sd=fa[2])
fb = fitdistr(df[df$X == "b",]$Y, "normal")$estimate # create fit for X.b
ks.test(df[df$X == "b",]$Y, "pnorm", mean=fb[1], sd=fb[2])

# base plot histograms
par(mfrow=c(2,1))
  hist(df[df$X == "a",]$Y, 
       main="Histogram of Y for 'a'", 
       xlab="Y",
       col="skyblue", 
       xlim=c(4, 18), 
       ylim=c(0, 0.3), 
       breaks=seq(4, 18, 1),
       axes=FALSE,
       freq=FALSE
  )
  axis(side=1, at=seq(4, 18, 2), labels=seq(4, 18, 2))
  axis(side=2, at=seq(0, 0.3, 0.1), labels=seq(0, 0.3, 0.1))
  curve(dnorm(x, mean=fa[1], sd=fa[2]), col="blue", lty=1, lwd=3, add=TRUE)
  hist(df[df$X == "b",]$Y, 
       main="Histogram of Y for 'b'", 
       xlab="Y",
       col="plum2", 
       xlim=c(4, 18), 
       ylim=c(0, 0.3), 
       breaks=seq(4, 18, 1),
       axes=FALSE,
       freq=FALSE
  )
  axis(side=1, at=seq(4, 18, 2), labels=seq(4, 18, 2))
  axis(side=2, at=seq(0, 0.3, 0.1), labels=seq(0, 0.3, 0.1))
  curve(dnorm(x, mean=fb[1], sd=fb[2]), col="purple", lty=1, lwd=3, add=TRUE)
par(mfrow=c(1,1))

# ggplot histograms
# http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
ggplot(data=df, aes(x=Y, col=X, fill=X)) + theme_minimal() + 
  # set the font styles for the plot title and axis titles
  theme(plot.title   = element_text(face="bold",  color="black", size=18, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.title.x = element_text(face="bold",  color="black", size=16, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.title.y = element_text(face="bold",  color="black", size=16, hjust=0.5, vjust=0.0, angle=90)) + 
  # set the font styles for the value labels that show on each axis
  theme(axis.text.x  = element_text(face="plain", color="black", size=12, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.text.y  = element_text(face="plain", color="black", size=12, hjust=0.0, vjust=0.5, angle=0)) + 
  # set the font style for the facet labels
  theme(strip.text = element_text(face="bold", color="black", size=14, hjust=0.5)) + 
  # create the histogram; the alpha value ensures overlaps can be seen
  geom_histogram(color="darkgray", binwidth=1, breaks=seq(4,18,by=1), alpha=0.25, position="identity") + 
  # create stacked plots by X, one for each histogram
  facet_grid(X ~ .) + 
  # determine the fill color values of each histogram
  scale_fill_manual(values=c("#69b3a2","#404080")) + 
  # set the labels for the title and each axis
  labs(title="Histograms of Y by X", x="Y", y="Count") + 
  # set the ranges and value labels for each axis
  scale_x_continuous(breaks=seq(4,18,by=1), labels=seq(4,18,by=1), limits=c(4,18)) +
  scale_y_continuous(breaks=seq(0,8,by=2), labels=seq(0,8,by=2), limits=c(0,8))


##
## Lognormal distribution
##
## assume df has one factor (X) w/two levels (a,b) and continuous response Y
library(MASS) # for fitdistr
df <- read.csv("data/Distributions/1F2LBs_lognormal.csv")
fa = fitdistr(df[df$X == "a",]$Y, "lognormal")$estimate # create fit for X.a
ks.test(df[df$X == "a",]$Y, "plnorm", meanlog=fa[1], sdlog=fa[2])
fb = fitdistr(df[df$X == "b",]$Y, "lognormal")$estimate # create fit for X.b
ks.test(df[df$X == "b",]$Y, "plnorm", meanlog=fb[1], sdlog=fb[2])

# base plot histograms
par(mfrow=c(2,1))
  hist(df[df$X == "a",]$Y, 
       main="Histogram of Y for 'a'", 
       xlab="Y",
       col="skyblue", 
       xlim=c(0, 80), 
       ylim=c(0, 0.1), 
       breaks=seq(0, 80, 10), 
       axes=FALSE,
       freq=FALSE
  )
  axis(side=1, at=seq(0, 80, 10), labels=seq(0, 80, 10))
  axis(side=2, at=seq(0, 0.1, 0.025), labels=seq(0, 0.1, 0.025))
  curve(dlnorm(x, meanlog=fa[1], sdlog=fa[2]), col="blue", lty=1, lwd=3, add=TRUE)
  hist(df[df$X == "b",]$Y, 
       main="Histogram of Y for 'b'", 
       xlab="Y",
       col="plum2", 
       xlim=c(0, 80), 
       ylim=c(0, 0.1), 
       breaks=seq(0, 80, 10),
       axes=FALSE,
       freq=FALSE
  )
  axis(side=1, at=seq(0, 80, 10), labels=seq(0, 80, 10))
  axis(side=2, at=seq(0, 0.1, 0.025), labels=seq(0, 0.1, 0.025))
  curve(dlnorm(x, meanlog=fb[1], sdlog=fb[2]), col="purple", lty=1, lwd=3, add=TRUE)
par(mfrow=c(1,1))

# ggplot histograms
# http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
ggplot(data=df, aes(x=Y, col=X, fill=X)) + theme_minimal() + 
  # set the font styles for the plot title and axis titles
  theme(plot.title   = element_text(face="bold",  color="black", size=18, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.title.x = element_text(face="bold",  color="black", size=16, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.title.y = element_text(face="bold",  color="black", size=16, hjust=0.5, vjust=0.0, angle=90)) + 
  # set the font styles for the value labels that show on each axis
  theme(axis.text.x  = element_text(face="plain", color="black", size=12, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.text.y  = element_text(face="plain", color="black", size=12, hjust=0.0, vjust=0.5, angle=0)) + 
  # set the font style for the facet labels
  theme(strip.text = element_text(face="bold", color="black", size=14, hjust=0.5)) + 
  # create the histogram; the alpha value ensures overlaps can be seen
  geom_histogram(color="darkgray", binwidth=10, breaks=seq(0,80,by=10), alpha=0.25, position="identity") + 
  # create stacked plots by X, one for each histogram
  facet_grid(X ~ .) + 
  # determine the fill color values of each histogram
  scale_fill_manual(values=c("#69b3a2","#404080")) + 
  # set the labels for the title and each axis
  labs(title="Histograms of Y by X", x="Y", y="Count") + 
  # set the ranges and value labels for each axis
  scale_x_continuous(breaks=seq(0,80,by=10), labels=seq(0,80,by=10), limits=c(0,80)) +
  scale_y_continuous(breaks=seq(0,20,by=4), labels=seq(0,20,by=4), limits=c(0,20))


##
## Poisson distribution
##
## assume df has one factor (X) w/two levels (a,b) and nonnegative integer response Y
library(fitdistrplus) # for fitdist, gofstat
df <- read.csv("data/Distributions/1F2LBs_poisson.csv")
fa = fitdist(df[df$X == "a",]$Y, "pois") # create fit for X.a
gofstat(fa)
fb = fitdist(df[df$X == "b",]$Y, "pois") # create fit for X.b
gofstat(fb)

# alternatively
library(MASS) # for fitdistr
df <- read.csv("data/Distributions/1F2LBs_poisson.csv")
fa = fitdistr(df[df$X == "a",]$Y, "Poisson")$estimate
ks.test(df[df$X == "a",]$Y, "ppois", lambda=fa[1])
fb = fitdistr(df[df$X == "b",]$Y, "Poisson")$estimate
ks.test(df[df$X == "b",]$Y, "ppois", lambda=fb[1])

# if var/mean > 1.15, we have overdispersion
var(df[df$X == "a",]$Y) / mean(df[df$X == "a",]$Y) # 0.9784483
var(df[df$X == "b",]$Y) / mean(df[df$X == "b",]$Y) # 0.7765848

library(AER) # for dispersiontest
m = glm(Y ~ X, data=df, family=poisson)
dispersiontest(m) # p = 0.8604; since p > .05, it is not overdispersed

# base plot histograms
par(mfrow=c(2,1))
  hist(df[df$X == "a",]$Y, 
       main="Histogram of Y for 'a'", 
       xlab="Y",
       col="skyblue", 
       xlim=c(0, 10), 
       ylim=c(0, 0.3), 
       breaks=seq(0, 10, 1), 
       axes=FALSE,
       freq=FALSE
  )
  axis(side=1, at=seq(0, 10, 1), labels=seq(0, 10, 1))
  axis(side=2, at=seq(0, 0.3, 0.1), labels=seq(0, 0.3, 0.1))
  curve(dpois(round(x,0), lambda=fa[1]), col="blue", lty=1, lwd=3, add=TRUE)
  hist(df[df$X == "b",]$Y, 
       main="Histogram of Y for 'b'", 
       xlab="Y",
       col="plum2", 
       xlim=c(0, 10), 
       ylim=c(0, 0.3), 
       breaks=seq(0, 10, 1), 
       axes=FALSE,
       freq=FALSE
  )
  axis(side=1, at=seq(0, 10, 1), labels=seq(0, 10, 1))
  axis(side=2, at=seq(0, 0.3, 0.1), labels=seq(0, 0.3, 0.1))
  curve(dpois(round(x,0), lambda=fb[1]), col="purple", lty=1, lwd=3, add=TRUE)
par(mfrow=c(1,1))

# ggplot histograms
# http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
ggplot(data=df, aes(x=Y, col=X, fill=X)) + theme_minimal() + 
  # set the font styles for the plot title and axis titles
  theme(plot.title   = element_text(face="bold",  color="black", size=18, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.title.x = element_text(face="bold",  color="black", size=16, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.title.y = element_text(face="bold",  color="black", size=16, hjust=0.5, vjust=0.0, angle=90)) + 
  # set the font styles for the value labels that show on each axis
  theme(axis.text.x  = element_text(face="plain", color="black", size=12, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.text.y  = element_text(face="plain", color="black", size=12, hjust=0.0, vjust=0.5, angle=0)) + 
  # set the font style for the facet labels
  theme(strip.text = element_text(face="bold", color="black", size=14, hjust=0.5)) + 
  # create the histogram; the alpha value ensures overlaps can be seen
  geom_histogram(color="darkgray", binwidth=1, breaks=seq(0,10,by=1), alpha=0.25, position="identity") + 
  # create stacked plots by X, one for each histogram
  facet_grid(X ~ .) + 
  # determine the fill color values of each histogram
  scale_fill_manual(values=c("#69b3a2","#404080")) + 
  # set the labels for the title and each axis
  labs(title="Histograms of Y by X", x="Y", y="Count") + 
  # set the ranges and value labels for each axis
  scale_x_continuous(breaks=seq(0,10,by=1), labels=seq(0,10,by=1), limits=c(0,10)) +
  scale_y_continuous(breaks=seq(0,8,by=1), labels=seq(0,8,by=1), limits=c(0,8))


##
## Negative Binomial distribution
##
## assume df has one factor (X) w/two levels (a,b) and nonnegative integer response Y
library(fitdistrplus) # for fitdist, gofstat
df <- read.csv("data/Distributions/1F2LBs_negbin.csv")
fa = fitdist(df[df$X == "a",]$Y, "nbinom") # create fit for X.a
gofstat(fa)
fb = fitdist(df[df$X == "b",]$Y, "nbinom") # create fit for X.b
gofstat(fb)

# alternatively
library(MASS) # for fitdistr
df <- read.csv("data/Distributions/1F2LBs_negbin.csv")
fa = fitdistr(df[df$X == "a",]$Y, "negative binomial", lower=1e-6)$estimate
ks.test(df[df$X == "a",]$Y, "pnbinom", size=fa[1], mu=fa[2])
fb = fitdistr(df[df$X == "b",]$Y, "negative binomial", lower=1e-6)$estimate
ks.test(df[df$X == "b",]$Y, "pnbinom", size=fb[1], mu=fb[2])

# if var/mean > 1.15, we have overdispersion
var(df[df$X == "a",]$Y) / mean(df[df$X == "a",]$Y) # 4.825226
var(df[df$X == "b",]$Y) / mean(df[df$X == "b",]$Y) # 3.090988

library(AER) # for dispersiontest
m = glm(Y ~ X, data=df, family=poisson)
dispersiontest(m) # p = 2.867e-06; since p < .05, it is overdispersed

# base plot histograms
par(mfrow=c(2,1))
  hist(df[df$X == "a",]$Y, 
       main="Histogram of Y for 'a'", 
       xlab="Y",
       col="skyblue", 
       xlim=c(0, 40), 
       ylim=c(0, 0.075), 
       breaks=seq(0, 40, 5), 
       axes=FALSE,
       freq=FALSE
  )
  axis(side=1, at=seq(0, 40, 5), labels=seq(0, 40, 5))
  axis(side=2, at=seq(0, 0.075, 0.025), labels=seq(0, 0.075, 0.025))
  curve(dnbinom(round(x,0), size=fa[1], mu=fa[2]), col="purple", lty=1, lwd=3, add=TRUE)
  hist(df[df$X == "b",]$Y, 
       main="Histogram of Y for 'b'", 
       xlab="Y",
       col="plum2", 
       xlim=c(0, 40), 
       ylim=c(0, 0.075), 
       breaks=seq(0, 40, 5), 
       axes=FALSE,
       freq=FALSE
  )
  axis(side=1, at=seq(0, 40, 5), labels=seq(0, 40, 5))
  axis(side=2, at=seq(0, 0.075, 0.025), labels=seq(0, 0.075, 0.025))
  curve(dnbinom(round(x,0), size=fb[1], mu=fb[2]), col="purple", lty=1, lwd=3, add=TRUE)
par(mfrow=c(1,1))

# ggplot histograms
# http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
ggplot(data=df, aes(x=Y, col=X, fill=X)) + theme_minimal() + 
  # set the font styles for the plot title and axis titles
  theme(plot.title   = element_text(face="bold",  color="black", size=18, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.title.x = element_text(face="bold",  color="black", size=16, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.title.y = element_text(face="bold",  color="black", size=16, hjust=0.5, vjust=0.0, angle=90)) + 
  # set the font styles for the value labels that show on each axis
  theme(axis.text.x  = element_text(face="plain", color="black", size=12, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.text.y  = element_text(face="plain", color="black", size=12, hjust=0.0, vjust=0.5, angle=0)) + 
  # set the font style for the facet labels
  theme(strip.text = element_text(face="bold", color="black", size=14, hjust=0.5)) + 
  # create the histogram; the alpha value ensures overlaps can be seen
  geom_histogram(color="darkgray", binwidth=5, breaks=seq(0,40,by=5), alpha=0.25, position="identity") + 
  # create stacked plots by X, one for each histogram
  facet_grid(X ~ .) + 
  # determine the fill color values of each histogram
  scale_fill_manual(values=c("#69b3a2","#404080")) + 
  # set the labels for the title and each axis
  labs(title="Histograms of Y by X", x="Y", y="Count") + 
  # set the ranges and value labels for each axis
  scale_x_continuous(breaks=seq(0,40,by=5), labels=seq(0,40,by=5), limits=c(0,40)) +
  scale_y_continuous(breaks=seq(0,10,by=2), labels=seq(0,10,by=2), limits=c(0,10))


##
## Exponential distribution
##
## assume df has one factor (X) w/two levels (a,b) and continuous response Y
library(MASS) # for fitdistr
df <- read.csv("data/Distributions/1F2LBs_exponential.csv")
fa = fitdistr(df[df$X == "a",]$Y, "exponential")$estimate # create fit for X.a
ks.test(df[df$X == "a",]$Y, "pexp", rate=fa[1])
fb = fitdistr(df[df$X == "b",]$Y, "exponential")$estimate # create fit for X.b
ks.test(df[df$X == "b",]$Y, "pexp", rate=fb[1])

# base plot histograms
par(mfrow=c(2,1))
  hist(df[df$X == "a",]$Y, 
       main="Histogram of Y for 'a'", 
       xlab="Y",
       col="skyblue", 
       xlim=c(0, 50), 
       ylim=c(0, 0.1), 
       breaks=seq(0, 50, 5), 
       axes=FALSE, 
       freq=FALSE
  )
  axis(side=1, at=seq(0, 50, 5), labels=seq(0, 50, 5))
  axis(side=2, at=seq(0, 0.1, 0.025), labels=seq(0, 0.1, 0.025))
  curve(dexp(x, rate=fa[1]), col="blue", lty=1, lwd=3, add=TRUE)
  hist(df[df$X == "b",]$Y, 
       main="Histogram of Y for 'b'", 
       xlab="Y",
       col="plum2", 
       xlim=c(0, 50), 
       ylim=c(0, 0.1), 
       breaks=seq(0, 50, 5), 
       axes=FALSE,
       freq=FALSE
  )
  axis(side=1, at=seq(0, 50, 5), labels=seq(0, 50, 5))
  axis(side=2, at=seq(0, 0.1, 0.025), labels=seq(0, 0.1, 0.025))
  curve(dexp(x, rate=fb[1]), col="purple", lty=1, lwd=3, add=TRUE)
par(mfrow=c(1,1))

# ggplot histograms
# http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
ggplot(data=df, aes(x=Y, col=X, fill=X)) + theme_minimal() + 
  # set the font styles for the plot title and axis titles
  theme(plot.title   = element_text(face="bold",  color="black", size=18, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.title.x = element_text(face="bold",  color="black", size=16, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.title.y = element_text(face="bold",  color="black", size=16, hjust=0.5, vjust=0.0, angle=90)) + 
  # set the font styles for the value labels that show on each axis
  theme(axis.text.x  = element_text(face="plain", color="black", size=12, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.text.y  = element_text(face="plain", color="black", size=12, hjust=0.0, vjust=0.5, angle=0)) + 
  # set the font style for the facet labels
  theme(strip.text = element_text(face="bold", color="black", size=14, hjust=0.5)) + 
  # create the histogram; the alpha value ensures overlaps can be seen
  geom_histogram(color="darkgray", binwidth=5, breaks=seq(0,50,by=5), alpha=0.25, position="identity") + 
  # create stacked plots by X, one for each histogram
  facet_grid(X ~ .) + 
  # determine the fill color values of each histogram
  scale_fill_manual(values=c("#69b3a2","#404080")) + 
  # set the labels for the title and each axis
  labs(title="Histograms of Y by X", x="Y", y="Count") + 
  # set the ranges and value labels for each axis
  scale_x_continuous(breaks=seq(0,50,by=5), labels=seq(0,50,by=5), limits=c(0,50)) +
  scale_y_continuous(breaks=seq(0,12,by=2), labels=seq(0,12,by=2), limits=c(0,12))


##
## Gamma distribution
##
## assume df has one factor (X) w/two levels (a,b) and continuous response Y
library(MASS) # for fitdistr
df <- read.csv("data/Distributions/1F2LBs_gamma.csv")
fa = fitdistr(df[df$X == "a",]$Y, "gamma")$estimate # create fit for X.a
ks.test(df[df$X == "a",]$Y, "pgamma", shape=fa[1], rate=fa[2])
fb = fitdistr(df[df$X == "b",]$Y, "gamma")$estimate # create fit for X.b
ks.test(df[df$X == "b",]$Y, "pgamma", shape=fb[1], rate=fb[2])

# base plot histograms
par(mfrow=c(2,1))
  hist(df[df$X == "a",]$Y, 
       main="Histogram of Y for 'a'", 
       xlab="Y",
       col="skyblue", 
       xlim=c(0, 12), 
       ylim=c(0, 0.25), 
       breaks=seq(0, 12, 2), 
       axes=FALSE,
       freq=FALSE
  )
  axis(side=1, at=seq(0, 12, 2), labels=seq(0, 12, 2))
  axis(side=2, at=seq(0, 0.25, 0.05), labels=seq(0, 0.25, 0.05))
  curve(dgamma(x, shape=fa[1], rate=fa[2]), col="blue", lty=1, lwd=3, add=TRUE)
  hist(df[df$X == "b",]$Y, 
       main="Histogram of Y for 'b'", 
       xlab="Y",
       col="plum2", 
       xlim=c(0, 12), 
       ylim=c(0, 0.25), 
       breaks=seq(0, 12, 2), 
       axes=FALSE,
       freq=FALSE
  )
  axis(side=1, at=seq(0, 12, 2), labels=seq(0, 12, 2))
  axis(side=2, at=seq(0, 0.25, 0.05), labels=seq(0, 0.25, 0.05))
  curve(dgamma(x, shape=fb[1], rate=fb[2]), col="purple", lty=1, lwd=3, add=TRUE)
par(mfrow=c(1,1))

# ggplot histograms
# http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
ggplot(data=df, aes(x=Y, col=X, fill=X)) + theme_minimal() + 
  # set the font styles for the plot title and axis titles
  theme(plot.title   = element_text(face="bold",  color="black", size=18, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.title.x = element_text(face="bold",  color="black", size=16, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.title.y = element_text(face="bold",  color="black", size=16, hjust=0.5, vjust=0.0, angle=90)) + 
  # set the font styles for the value labels that show on each axis
  theme(axis.text.x  = element_text(face="plain", color="black", size=12, hjust=0.5, vjust=0.0, angle=0)) + 
  theme(axis.text.y  = element_text(face="plain", color="black", size=12, hjust=0.0, vjust=0.5, angle=0)) + 
  # set the font style for the facet labels
  theme(strip.text = element_text(face="bold", color="black", size=14, hjust=0.5)) + 
  # create the histogram; the alpha value ensures overlaps can be seen
  geom_histogram(color="darkgray", binwidth=2, breaks=seq(0,12,by=2), alpha=0.25, position="identity") + 
  # create stacked plots by X, one for each histogram
  facet_grid(X ~ .) + 
  # determine the fill color values of each histogram
  scale_fill_manual(values=c("#69b3a2","#404080")) + 
  # set the labels for the title and each axis
  labs(title="Histograms of Y by X", x="Y", y="Count") + 
  # set the ranges and value labels for each axis
  scale_x_continuous(breaks=seq(0,12,by=2), labels=seq(0,12,by=2), limits=c(0,12)) +
  scale_y_continuous(breaks=seq(0,14,by=2), labels=seq(0,14,by=2), limits=c(0,14))

