# Set working directory
setwd("~/Dropbox/NME") #    <----- Update this line to the location of the replication folder called NME on your machine

# Clear workspace
rm(list = ls())

# Set seed 
set.seed(3876055)

# Install packages
#install.packages("arm")
#install.packages("plm")

# Load packages
library(foreign)
library(plm)
library(arm)

# Load data
krup <- read.dta("Krupnikov Replication/Data/krup.dta")

# Estimate model
m <- glm(turnout ~ 
           # negativity
           negaboutdislike + negaboutlike +
           # resources
           income + education + age + unemployed +
           # evaluation of parties and candidates
           PIDStrength + AffectPID + care + AffectPRES +
           # social involvement
           lnYears + Church + homeowners + working + 
           # mobilization
           contacted + 
           # interest, exposure, and efficacy
           external + internal + interest + media_index + 
           # other demographics
           married + black + southern + hispanic + gender + 
           # state conditions
           closeness + governors + primaries + 
           # volume and year controls
           volume2 + dummy1988 + dummy2000 + dummy1992 + dummy1996, 
         data = krup, family = "binomial")

# Verify that I've successfully replicated the coefficients
display(m, digits = 3)

# Save mean and cov
beta.hat <- coef(m)
Sigma <- vcovHC(m, type = "HC0")
# Note that the vcovHC() function does not exactly replicate
# the standard errors that Krupnikov estimates using Stata's 
# robust option. Stata uses a small degrees of freedom correction.
# However, the estimated standard errors are very similar and 
# change nothing substantively.

# Simulate from the "posterior" using the CLT
s <- mvrnorm(10000, beta.hat, Sigma)

# a function to set all variables at their medians
choose.x <- function(m) {
  vars <- names(coefficients(m))[-1]
  vals <- apply(m$data, 2, median, na.rm = TRUE)[vars]
  vals <- c(1, vals)
}

## Compute first differences
tab <- matrix(NA, nrow = 3, ncol = 3)
colnames(tab) <- c("est", "lwr", "upr")
rownames(tab) <- c("0% to 20%", "0% to 40%", "0% to 60%")

# set all variables to median
x.lo <- x.hi <- choose.x(m)
# change negaaboutdislike to zero
x.lo["negaboutdislike"] <- 0
s.lo <- plogis(s%*%x.lo)

# change negaboutdislike to 0.2
x.hi["negaboutdislike"] <- .2
s.hi <- plogis(s%*%x.hi)
s.d <- (s.hi - s.lo)
q.d <- round(quantile(s.d, c(.5, .05, .95)), 3)
tab[1, ] <- q.d

# change negaboutdislike to 0.4
x.hi["negaboutdislike"] <- .4
s.hi <- plogis(s%*%x.hi)
s.d <- (s.hi - s.lo)
q.d <- round(quantile(s.d, c(.5, .05, .95)), 3)
tab[2, ] <- q.d

# change negaboutdislike to 0.6
x.hi["negaboutdislike"] <- .6
s.hi <- plogis(s%*%x.hi)
s.d <- (s.hi - s.lo)
q.d <- round(quantile(s.d, c(.5, .05, .95)), 3)
tab[3, ] <- q.d

print(tab)