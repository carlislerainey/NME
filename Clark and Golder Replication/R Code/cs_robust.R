# Load the data
cg <- read.csv("Clark and Golder Replication/Data/cg.csv")
# Reduce the data set to only the variables used.
cg <- cg[, c("year", "enep1", "eneg", "avemag", "uppertier", "enpres", "proximity1", "country")]
# Listwise delete
cg <- na.omit(cg)

countries <- as.character(unique(cg$country))
cg.new <- NULL
for (i in 1:length(countries)) {
  sub.cg <- cg[as.character(cg$country) == countries[i], ]
  new.obs <- sub.cg[sub.cg$year == max(sub.cg$year), ]
  cg.new <- rbind(cg.new, new.obs)
}
cg <- as.data.frame(cg.new)

## Obtain informal Bayesian posterior simulations using bootstrap

# Bootstrap
n.bs <- 5000
bs.sim <- matrix(NA, nrow = n.bs, ncol = length(coef(m)))
for (i in 1:n.bs) {
  bs.samp <- sample(1:nrow(cg), nrow(cg), replace = T)
  bs.data <- cg[bs.samp, ]
  bs.est <- rlm(enep1 ~ eneg*log(avemag) + eneg*uppertier + enpres*proximity1, 
                method = "M", data = bs.data, maxit = 100000)
  bs.sim[i, ] <- coef(bs.est)
}
sim <- bs.sim

# Create vectors to store hypothetical values
x.lo.lo <- numeric(9)
names(x.lo.lo) <- c("(Intercept)", "eneg", "log(avemag)", "uppertier", "enpres",
                    "proximity1", "eneg:log(avemag)", "eneg:uppertier", "enpres:proximity1")
x.hi.lo <- x.lo.hi <- x.lo.lo

# Set values
eneg.lo <- 1.06
eneg.hi <- 2.48
ln.avemag.lo <- log(1)
ln.avemag.hi <- log(7)

# Create vectors to compute EY
x.lo.lo["(Intercept)"] <- 1 
x.lo.lo["eneg"] <- eneg.lo
x.lo.lo["log(avemag)"] <- ln.avemag.lo
x.lo.lo["uppertier"] <- 0
x.lo.lo["enpres"] <- 0
x.lo.lo["proximity1"] <- 0
x.lo.lo["eneg:log(avemag)"] <- x.lo.lo["eneg"]*x.lo.lo["log(avemag)"]
x.lo.lo["eneg:uppertier"] <- x.lo.lo["eneg"]*x.lo.lo["uppertier"]
x.lo.lo["enpres:proximity1"] <- x.lo.lo["enpres"]*x.lo.lo["proximity1"]

x.hi.lo <- x.lo.lo
x.hi.lo["(Intercept)"] <- 1 
x.hi.lo["eneg"] <- eneg.hi
x.hi.lo["log(avemag)"] <- ln.avemag.lo
x.hi.lo["eneg:log(avemag)"] <- x.hi.lo["eneg"]*x.hi.lo["log(avemag)"]
x.hi.lo["eneg:uppertier"] <- x.hi.lo["eneg"]*x.hi.lo["uppertier"]
x.hi.lo["enpres:proximity1"] <- x.hi.lo["enpres"]*x.hi.lo["proximity1"]

x.lo.hi <- x.lo.lo
x.lo.hi["(Intercept)"] <- 1 
x.lo.hi["eneg"] <- eneg.lo
x.lo.hi["log(avemag)"] <- ln.avemag.hi
x.lo.hi["eneg:log(avemag)"] <- x.lo.hi["eneg"]*x.lo.hi["log(avemag)"]
x.lo.hi["eneg:uppertier"] <- x.lo.hi["eneg"]*x.lo.hi["uppertier"]
x.lo.hi["enpres:proximity1"] <- x.lo.hi["enpres"]*x.lo.hi["proximity1"]

# Calculate Quantities of Interest
ev.lo.lo <- sim%*%x.lo.lo
ev.hi.lo <- sim%*%x.hi.lo
ev.lo.hi <- sim%*%x.lo.hi

# Simulated effect of ENEG when M is low.
ev.d.lo <- ev.hi.lo - ev.lo.lo

# Simulated effect of M with ENEG is low.
ev.lo.d <- ev.lo.hi - ev.lo.lo

# Create confidence intervals
q.d.lo <- quantile(ev.d.lo, c(.05, .5, .95))
q.lo.d <- quantile(ev.lo.d, c(.05, .5, .95))