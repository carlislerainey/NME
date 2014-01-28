
# Set the working directory 
setwd("~/Dropbox/NME") #    <----- Update this line to the location of the replication folder called NME

# Clear the workspace
rm(list = ls())

# Set seed
set.seed(4755427)

# Install required libraries  
#install.packages("arm")
#install.packages("sandwich")

# Load required libraries
library(arm)
library(sandwich)

# Create containers to store estimates and confidence intervals.
est.eneg <- matrix(NA, nrow = 1, ncol = 3)
est.m <- matrix(NA, nrow = 1, ncol = 3)

# Replicate Clark and Golder's results
source("Clark and Golder Replication/R Code/Stata_se.R")
est.eneg[1, ] <- q.d.lo
est.m[1, ] <- q.lo.d
rownames(est.eneg) <- rownames(est.m) <- "Clark and Golder's Estimate"

# Generate OLS results
source("Clark and Golder Replication/R Code/ols.R")
est.eneg <- rbind(est.eneg, q.d.lo)
est.m <- rbind(est.m, q.lo.d)
rownames(est.eneg)[nrow(est.m)] <- 
  rownames(est.m)[nrow(est.m)] <- "OLS Estimates and Std. Errors\nUsing Pooled Data"

# Generate random-effects results
source("Clark and Golder Replication/R Code/hier.R")
est.eneg <- rbind(est.eneg, q.d.lo)
est.m <- rbind(est.m, q.lo.d)
rownames(est.eneg)[nrow(est.m)] <- 
  rownames(est.m)[nrow(est.m)] <- "Random Intercept Model\nUsing Pooled Data"

# Generate cross-section results
source("Clark and Golder Replication/R Code/cs.R")
est.eneg <- rbind(est.eneg, q.d.lo)
est.m <- rbind(est.m, q.lo.d)
rownames(est.eneg)[nrow(est.m)] <- 
  rownames(est.m)[nrow(est.m)] <- "OLS Estimates with White's Std. Errors\nUsing Cross-Sectional Data"

# Generate cross-section results with robust regression
source("Clark and Golder Replication/R Code/cs_robust.R")
est.eneg <- rbind(est.eneg, q.d.lo)
est.m <- rbind(est.m, q.lo.d)
rownames(est.eneg)[nrow(est.m)] <- 
  rownames(est.m)[nrow(est.m)] <- "M Estimator with Boostrapped Std. Errors\nUsing Cross-Sectional Data"

# Generate cross-section results with robust regression and transformed outcome
source("Clark and Golder Replication/R Code/cs_robust_trans.R")
est.eneg <- rbind(est.eneg, q.d.lo)
est.m <- rbind(est.m, q.lo.d)
rownames(est.eneg)[nrow(est.m)] <- 
  rownames(est.m)[nrow(est.m)] <- "M Estimator with Boostrapped Std. Errors\nUsing Cross-Sectional Data\nand Log-Transformed Outcome"

library(compactr)
pdf("Manuscript/Figures/cg.pdf", 
           height = 4.5, width = 8, family = "serif")
par(mfrow = c(1, 2), oma = c(3, .5, 1, .5), mar = c(1,1,1,1))
# left panel - social hetergeneity
eplot(NULL, xlim = c(-1.7, 1.7), ylim = c(-.9, -.1), 
      xat = c(-1.2, -.62, 0, .62, 1.2),
      anny = FALSE,
      xlab = "Expected Change in the\nEffective Number of Parties",
      xlabpos = 2.5,
      main = "The Effect of Social Heterogeneity")
abline(v = .62, xpd = FALSE, lty = 3)
abline(v = -.62, xpd = FALSE, lty = 3)
for (i in 1:nrow(est.eneg)) {
  est <- est.eneg
  lines(c(est[i, 1], est[i, 3]), c(-i/(nrow(est) + 1), -i/(nrow(est) + 1)), lwd = 2) 
  points(est[i, 2], -i/(nrow(est) + 1), pch = 19, cex = .7)
  text(est[i, 2], -i/(nrow(est) + 1), rownames(est)[i], pos = 3, cex = .6)
}
# right panel - district magnitude
eplot(NULL, xlim = c(-1.7, 1.7), ylim = c(-.9, -.1), 
      xat = c(-1.2, -.62, 0, .62, 1.2),
      anny = FALSE,
      xlab = "Expected Change in the\nEffective Number of Parties",
      xlabpos = 2.5,
      main = "The Effect of Electoral Permissiveness")
abline(v = .62, xpd = FALSE, lty = 3)
abline(v = -.62, xpd = FALSE, lty = 3)
for (i in 1:nrow(est.eneg)) {
  est <- est.m
  lines(c(est[i, 1], est[i, 3]), c(-i/(nrow(est) + 1), -i/(nrow(est) + 1)), lwd = 2) 
  points(est[i, 2], -i/(nrow(est) + 1), pch = 19, cex = .7)
  text(est[i, 2], -i/(nrow(est) + 1), rownames(est)[i], pos = 3, cex = .6)
}
dev.off()