# initializing ----
library(data.table)
library(nlme)
library(AICcmodavg)
library(nls.multstart)
library(ggplot2)
library(beezdemand)
library(plot3D)
library(gridExtra)
# 
# testAIC <- c(0, 1, 2, 3, 20, 20, 20, 20, 20, 20, 20)
# # (testAIC-min(testAIC))
# exp(-(1/2) * (testAIC-min(testAIC))) / sum(exp(-(1/2) * (testAIC-min(testAIC))))
# # exp(-(1/2) * (paperAIC-min(paperAIC))) / sum(exp(-(1/2) * (paperAIC-min(paperAIC))))


# use mean k above all points for determining k
# setting the seed
set.seed(09264)

### Below is not run but included for transparency, data organization process----
## fagerstrom cleaner----
# fagerstrom <- function(x){
#     fcase(
#         x %in% c("No", "All others", "After 60 minutes", "10 or less"), 0,
#         x %in% c("Yes","The first one in the morning", "30-60 minutes", "20-Nov"), 1,
#         x %in% c("6-30 minutes", "21-30"), 2,
#         x %in% c("Within 5 minutes", "31 or more"), 3
#     )
# }
## data import ----
# 
# 
# allData <- fread("delayeddemandalldata.csv", na.strings = "")
# allData <- allData[-c(1:2),]
# allData$FTNDnum <- apply(allData[, lapply(.SD, fagerstrom), .SDcols = grepl("FTND", names(allData))], 1, sum)
# allData[, id := paste0("A", 1:nrow(allData))] 
# cptData <- allData[, c(grep("id|cpt|FTNDnum", names(allData))), with = FALSE]
# 
# 
# 
# 
# longData <- melt(cptData, id.vars = c("id", "FTNDnum"), value.name = "z")[order(id, variable)]
# longData[, levels(variable)]
# 
# delays <- c(round(5/60, 4), 6, 24, 7*24, 30*24, 90*24, 365*24, 24*365*5)
# prices <- c(.01, .03, .06, .13, .25, .5, 1, 2, 4, 8)
# 
# longData[, y := rep(delays, each = length(prices), length(unique(id)))]
# longData[, x := rep(prices, length(unique(id)) * length(delays))]
# longData[, z := as.numeric(z)]
# longData[, .N, by = id]$N
# cleanLong <- longData[complete.cases(longData$z)]

## Removing points above 200 ---- 
# badNames <- cleanLong[z > 200, unique(id)]
# length(badNames)
# cleanLong <- cleanLong[!id %in% badNames]
# 
## Systematic 
# systematic3D <- function(data){
#     set1 <- data[x == min(x) & y == min(y), z, by = id]$id[which(data[x == min(x) & y == min(y), z, by = id]$z -
#                                                                      data[x == max(x) & y == max(y), z, by = id]$z < 0)]
#     set2 <- data[x == min(x) & y == min(y), z, by = id]$id[which(data[x == min(x) & y == min(y), z, by = id]$z -
#                                                                      data[x == min(x) & y == max(y), z, by = id]$z < 0)]
#     set3 <- data[x == min(x) & y == min(y), z, by = id]$id[which(data[x == min(x) & y == min(y), z, by = id]$z -
#                                                                      data[x == max(x) & y == min(y), z, by = id]$z < 0)]
#     return(((list(set1, set2, set3))))
# }
# length(Reduce(intersect, systematic3D(longData)))
# 
# lapply(systematic3D(longData), length)
# length(unique(unlist(systematic3D(longData))))
# missingDataNames <- cleanLong[, .N, by = id][which(cleanLong[, .N, by = id]$N< 80), id]
# cleanLong <- cleanLong[!id %in% missingDataNames]
# doubleBadNames <- unlist(systematic3D(cleanLong))
# length(doubleBadNames)
# cleanLong <- cleanLong[!id %in% doubleBadNames]
# cleanLong[, unique(id)]
# 
# write.csv(allData[id %in% cleanLong[, unique(id)], .SD, .SDcols = c(grepl("Duration|Marital|Income|Dependents|Age|Race|Race_7_TEXT|Education|Employment|FTNDnum", names(allData)))],
#           "delayeddemanddemo.csv")
# write.csv(cleanLong,
#           "delayeddemandcpt.csv")
# 
# durations <- function(x){
#     return(c(range(x),
#              mean(x),
#              median(x)))
# }
# 
# allData[id %in% cleanLong[, unique(id)], .SD, 
#         .SDcols = c(grepl("Duration|Marital|Income|Dependents|Age|Race|Race_7_TEXT|Education|Employment|FTNDnum", names(allData)))][
#             , durations(as.numeric(`Duration (in seconds)`))/60
#         ]
# 
# 
# cleanLong[, length(unique(id))]

# Data analysis begins ----
cleanLong <- fread("delayeddemandcpt.csv")
GetKZ <- function (dat, mnrange = TRUE) {
    if (mnrange) {
        dat1 <- aggregate(z ~ x, dat, mean)
        (log10(max(dat1$z[dat1$z > 0], na.rm = TRUE)) - log10(min(dat1$z[dat1$z >
                                                                             0], na.rm = TRUE)))
    }
    else {
        (log10(max(dat$z[dat$z > 0], na.rm = TRUE)) - log10(min(dat$z[dat$z >
                                                                          0], na.rm = TRUE)))
    }
}
cleanLong[, k := GetKZ(data.frame(z, x))]


# 2D demand and discounting ----
# base demand ----
expDemand <- nls_multstart(z ~ (10^(q0) * 10^(k *(exp(-10^(alpha)*10^(q0)*x)-1))), data = subset(cleanLong, y == min(y)) ,
                           iter = 250,
                           start_lower = c(q0 = .1, alpha = -4),
                           start_upper = c(q0 = 2, alpha = 4))



expDemandCovGNLS <- gnls(z ~ (10^(q0) * 10^(k *(exp(-10^(alpha)*10^(q0)*x)-1))),
                         data = subset(cleanLong, y == min(y)),
                         params = list(q0 ~ scale(FTNDnum),  
                                       alpha ~ scale(FTNDnum)
                         ),
                         start = list(fixed = c(coef(expDemand)[1], 0, coef(expDemand)[2], 0)),
                         verbose = 2,
                         control = list(msMaxIter = 50000,
                                        niterEM = 5000,
                                        maxIter = 5000,
                                        pnlsTol = .01,
                                        tolerance = .5,
                                        apVar = T,
                                        minScale = .0000001,
                                        opt = "optim"), na.action = na.omit)


expDemandMLM <- nlme(z ~ (10^(q0) * 10^(k *(exp(-10^(alpha)*10^(q0)*x)-1))),
                     data = subset(cleanLong, y == min(y)),
                     fixed = list(q0 ~ 1,  
                                  alpha ~ 1),
                     random = list(pdSymm(q0 + alpha ~ 1)),
                     start = list(fixed = coef(expDemand)),
                     groups = ~id,
                     method = "ML",
                     verbose = 2,
                     control = list(msMaxIter = 50000,
                                    niterEM = 5000,
                                    maxIter = 5000,
                                    pnlsTol = .01,
                                    tolerance = .01,
                                    apVar = T,
                                    minScale = .0000001,
                                    opt = "optim"), na.action = na.omit)

expDemandCovMLM <- nlme(z ~ (10^(q0) * 10^(k *(exp(-10^(alpha)*10^(q0)*x)-1))),
                        data = subset(cleanLong, y == min(y)),
                        fixed = list(q0 ~ scale(FTNDnum),  
                                     alpha ~ scale(FTNDnum)),
                        random = list(pdSymm(q0 + alpha ~ 1)),
                        start = list(fixed = coef(expDemandCovGNLS)),
                        groups = ~id,
                        method = "ML",
                        verbose = 2,
                        control = list(msMaxIter = 50000,
                                       niterEM = 5000,
                                       maxIter = 5000,
                                       pnlsTol = .01,
                                       tolerance = .01,
                                       apVar = T,
                                       minScale = .0000001,
                                       opt = "optim"), na.action = na.omit)

summary(expDemandCovMLM)
round(summary(expDemandCovMLM)$tTable, 3)

# ----
exp.5Demand <- nls_multstart(z ~ (10^(q0) * 10^((k+.5) *(exp(-10^(alpha)*10^(q0)*x)-1))), data = subset(cleanLong, y == min(y)) ,
                           iter = 250,
                           start_lower = c(q0 = .1, alpha = -4),
                           start_upper = c(q0 = 2, alpha = 4))



exp.5DemandCovGNLS <- gnls(z ~ (10^(q0) * 10^((k+.5) *(exp(-10^(alpha)*10^(q0)*x)-1))),
                         data = subset(cleanLong, y == min(y)),
                         params = list(q0 ~ scale(FTNDnum),  
                                       alpha ~ scale(FTNDnum)
                         ),
                         start = list(fixed = c(coef(exp.5Demand)[1], 0, coef(exp.5Demand)[2], 0)),
                         verbose = 2,
                         control = list(msMaxIter = 50000,
                                        niterEM = 5000,
                                        maxIter = 5000,
                                        pnlsTol = .01,
                                        tolerance = .5,
                                        apVar = T,
                                        minScale = .0000001,
                                        opt = "optim"), na.action = na.omit)


exp.5DemandMLM <- nlme(z ~ (10^(q0) * 10^((k+.5) *(exp(-10^(alpha)*10^(q0)*x)-1))),
                     data = subset(cleanLong, y == min(y)),
                     fixed = list(q0 ~ 1,  
                                  alpha ~ 1),
                     random = list(pdSymm(q0 + alpha ~ 1)),
                     start = list(fixed = coef(exp.5Demand)),
                     groups = ~id,
                     method = "ML",
                     verbose = 2,
                     control = list(msMaxIter = 50000,
                                    niterEM = 5000,
                                    maxIter = 5000,
                                    pnlsTol = .01,
                                    tolerance = .01,
                                    apVar = T,
                                    minScale = .0000001,
                                    opt = "optim"), na.action = na.omit)

exp.5DemandCovMLM <- nlme(z ~ (10^(q0) * 10^((k+.5) *(exp(-10^(alpha)*10^(q0)*x)-1))),
                        data = subset(cleanLong, y == min(y)),
                        fixed = list(q0 ~ scale(FTNDnum),  
                                     alpha ~ scale(FTNDnum)),
                        random = list(pdSymm(q0 + alpha ~ 1)),
                        start = list(fixed = coef(exp.5DemandCovGNLS)),
                        groups = ~id,
                        method = "ML",
                        verbose = 2,
                        control = list(msMaxIter = 50000,
                                       niterEM = 5000,
                                       maxIter = 5000,
                                       pnlsTol = .01,
                                       tolerance = .01,
                                       apVar = T,
                                       minScale = .0000001,
                                       opt = "optim"), na.action = na.omit)

summary(expDemandCovMLM)
round(summary(exp.5DemandCovMLM)$tTable, 3)
# ----

# Rachlin Demand ----
rachlinDemand <- nls_multstart(z ~ 10^q0/(1 + 10^b * x ^ s), data = subset(cleanLong, y == min(y)) ,
                               iter = 250,
                               start_lower = c(q0 = .1,  b = -4, s = .1),
                               start_upper = c(q0 = 2,  b = 4, s = 4))



rachlinDemandCovGNLS <- gnls(z ~ 10^q0/(1 + 10^b * x ^ s),
                             data = subset(cleanLong, y == min(y)),
                             params = list(q0 ~ scale(FTNDnum),  
                                           b ~ scale(FTNDnum),
                                           s ~ scale(FTNDnum)
                             ),
                             start = list(fixed = c(coef(rachlinDemand)[1], 0, coef(rachlinDemand)[2], 0, coef(rachlinDemand)[3], 0)),
                             verbose = 2,
                             control = list(msMaxIter = 50000,
                                            niterEM = 5000,
                                            maxIter = 5000,
                                            pnlsTol = .01,
                                            tolerance = .05,
                                            apVar = T,
                                            minScale = .0000001,
                                            opt = "optim"), na.action = na.omit)


rachlinDemandMLM <- nlme(z ~ 10^q0/(1 + 10^b * x ^ s),
                         data = subset(cleanLong, y == min(y)),
                         fixed = list(q0 ~ 1,  
                                      b ~ 1,
                                      s ~ 1
                         ),
                         random = list(pdSymm(q0 + b + s~ 1)),
                         start = list(fixed = coef(rachlinDemand)),
                         groups = ~id,
                         method = "ML",
                         verbose = 2,
                         control = list(msMaxIter = 50000,
                                        niterEM = 5000,
                                        maxIter = 5000,
                                        pnlsTol = .01,
                                        tolerance = .5,
                                        apVar = T,
                                        minScale = .0000001,
                                        opt = "optim"), na.action = na.omit)

rachlinDemandCovMLM <- nlme(z ~ 10^q0/(1 + 10^b * x ^ s),
                            data = subset(cleanLong, y == min(y)),
                            fixed = list(q0 ~ scale(FTNDnum),  
                                         b ~ scale(FTNDnum),
                                         s ~ scale(FTNDnum)
                            ),
                            random = list(pdSymm(q0 + b + s~ 1)),
                            start = list(fixed = coef(rachlinDemandCovGNLS)),
                            groups = ~id,
                            method = "ML",
                            verbose = 2,
                            control = list(msMaxIter = 50000,
                                           niterEM = 5000,
                                           maxIter = 5000,
                                           pnlsTol = .01,
                                           tolerance = .5,
                                           apVar = T,
                                           minScale = .0000001,
                                           opt = "optim"), na.action = na.omit)

summary(rachlinDemandCovMLM)
round(summary(rachlinDemandCovMLM)$tTable, 3)

anova(exp.5DemandCovMLM, rachlinDemandCovMLM)
anova(expDemandCovMLM, exp.5DemandCovMLM, rachlinDemandCovMLM)
anova(exp.5DemandMLM, rachlinDemandMLM)
anova(expDemandMLM, exp.5DemandMLM, rachlinDemandMLM)

# Rachlin delay ----
rachlinDelay <- nls_multstart(z ~ 10^q0/(1 + 10^b * y ^ s), data = subset(cleanLong, x == min(x)) ,
                              iter = 250,
                              start_lower = c(q0 = .1,  b = -4, s = .1),
                              start_upper = c(q0 = 2,  b = 4, s = 4))



rachlinDelayCovGNLS <- gnls(z ~ 10^q0/(1 + 10^b * y ^ s),
                            data = subset(cleanLong, x == min(x)),
                            params = list(q0 ~ scale(FTNDnum),  
                                          b ~ scale(FTNDnum),
                                          s ~ scale(FTNDnum)
                            ),
                            start = list(fixed = c(coef(rachlinDelay)[1], 0, coef(rachlinDelay)[2], 0, coef(rachlinDelay)[3], 0)),
                            verbose = 2,
                            control = list(msMaxIter = 50000,
                                           niterEM = 5000,
                                           maxIter = 5000,
                                           pnlsTol = .01,
                                           tolerance = .5,
                                           apVar = T,
                                           minScale = .0000001,
                                           opt = "optim"), na.action = na.omit)


rachlinDelayMLM <- nlme(z ~ 10^q0/(1 + 10^b * y ^ s),
                        data = subset(cleanLong, x == min(x)),
                        fixed = list(q0 ~ 1,  
                                     b ~ 1,
                                     s ~ 1
                        ),
                        random = list(pdSymm(q0 + b + s~ 1)),
                        start = list(fixed = coef(rachlinDelay)),
                        groups = ~id,
                        method = "ML",
                        verbose = 2,
                        control = list(msMaxIter = 50000,
                                       niterEM = 5000,
                                       maxIter = 5000,
                                       pnlsTol = .01,
                                       tolerance = 1.1,
                                       apVar = T,
                                       minScale = .0000001,
                                       opt = "optim"), na.action = na.omit)

rachlinDelayCovMLM <- nlme(z ~ 10^q0/(1 + 10^b * y ^ s),
                           data = subset(cleanLong, x == min(x)),
                           fixed = list(q0 ~ scale(FTNDnum),  
                                        b ~ scale(FTNDnum),
                                        s ~ scale(FTNDnum)
                           ),
                           random = list(pdSymm(q0 + b + s~ 1)),
                           start = list(fixed = coef(rachlinDelayCovGNLS)),
                           groups = ~id,
                           method = "ML",
                           verbose = 2,
                           control = list(msMaxIter = 50000,
                                          niterEM = 5000,
                                          maxIter = 5000,
                                          pnlsTol = .01,
                                          tolerance = .5,
                                          apVar = T,
                                          minScale = .0000001,
                                          opt = "optim"), na.action = na.omit)

summary(rachlinDelayCovMLM)
round(summary(rachlinDelayCovMLM)$tTable, 3)


# Exponentiated Delay ----
expDelay <- nls_multstart(z ~ (10^(q0) * 10^(k *(exp(-10^(alpha)*10^(q0)*y)-1))), data = subset(cleanLong, x == min(x)) ,
                          iter = 250,
                          start_lower = c(q0 = .1, alpha = -4),
                          start_upper = c(q0 = 2, alpha = 4))



expDelayCovGNLS <- gnls(z ~ (10^(q0) * 10^(k *(exp(-10^(alpha)*10^(q0)*y)-1))),
                        data = subset(cleanLong, x == min(x)),
                        params = list(q0 ~ scale(FTNDnum),  
                                      alpha ~ scale(FTNDnum)
                        ),
                        start = list(fixed = c(coef(expDelay)[1], 0, coef(expDelay)[2], 0)),
                        verbose = 2,
                        control = list(msMaxIter = 50000,
                                       niterEM = 5000,
                                       maxIter = 5000,
                                       pnlsTol = .01,
                                       tolerance = .5,
                                       apVar = T,
                                       minScale = .0000001,
                                       opt = "optim"), na.action = na.omit)


expDelayMLM <- nlme(z ~ (10^(q0) * 10^(k *(exp(-10^(alpha)*10^(q0)*y)-1))),
                    data = subset(cleanLong, x == min(x)),
                    fixed = list(q0 ~ 1,  
                                 alpha ~ 1),
                    random = list(pdSymm(q0 + alpha ~ 1)),
                    start = list(fixed = coef(expDelay)),
                    groups = ~id,
                    method = "ML",
                    verbose = 2,
                    control = list(msMaxIter = 50000,
                                   niterEM = 5000,
                                   maxIter = 5000,
                                   pnlsTol = .01,
                                   tolerance = .01,
                                   apVar = T,
                                   minScale = .0000001,
                                   opt = "optim"), na.action = na.omit)

expDelayCovMLM <- nlme(z ~ (10^(q0) * 10^(k *(exp(-10^(alpha)*10^(q0)*y)-1))),
                       data = subset(cleanLong, x == min(x)),
                       fixed = list(q0 ~ scale(FTNDnum),  
                                    alpha ~ scale(FTNDnum)),
                       random = list(pdSymm(q0 + alpha ~ 1)),
                       start = list(fixed = coef(expDelayCovGNLS)),
                       groups = ~id,
                       method = "ML",
                       verbose = 2,
                       control = list(msMaxIter = 50000,
                                      niterEM = 5000,
                                      maxIter = 5000,
                                      pnlsTol = .01,
                                      tolerance = .01,
                                      apVar = T,
                                      minScale = .0000001,
                                      opt = "optim"), na.action = na.omit)
summary(expDelayCovMLM)
round(summary(expDelayCovMLM)$tTable, 3)
expDelayCovMLM <- nlme(z ~ (10^(q0) * 10^(k *(exp(-10^(alpha)*10^(q0)*y)-1))),
                       data = subset(cleanLong, x == min(x)),
                       fixed = list(q0 ~ scale(FTNDnum),  
                                    alpha ~ scale(FTNDnum)),
                       random = list(pdSymm(q0 + alpha ~ 1)),
                       start = list(fixed = coef(expDelayCovGNLS)),
                       groups = ~id,
                       method = "ML",
                       verbose = 2,
                       control = list(msMaxIter = 50000,
                                      niterEM = 5000,
                                      maxIter = 5000,
                                      pnlsTol = .01,
                                      tolerance = .01,
                                      apVar = T,
                                      minScale = .0000001,
                                      opt = "optim"), na.action = na.omit)
summary(expDelayCovMLM)
round(summary(expDelayCovMLM)$tTable, 3)


exp.5Delay <- nls_multstart(z ~ (10^(q0) * 10^((k+.5) *(exp(-10^(alpha)*10^(q0)*y)-1))), data = subset(cleanLong, x == min(x)) ,
                          iter = 250,
                          start_lower = c(q0 = .1, alpha = -4),
                          start_upper = c(q0 = 2, alpha = 4))



exp.5DelayCovGNLS <- gnls(z ~ (10^(q0) * 10^((k+.5) *(exp(-10^(alpha)*10^(q0)*y)-1))),
                        data = subset(cleanLong, x == min(x)),
                        params = list(q0 ~ scale(FTNDnum),  
                                      alpha ~ scale(FTNDnum)
                        ),
                        start = list(fixed = c(coef(exp.5Delay)[1], 0, coef(exp.5Delay)[2], 0)),
                        verbose = 2,
                        control = list(msMaxIter = 50000,
                                       niterEM = 5000,
                                       maxIter = 5000,
                                       pnlsTol = .01,
                                       tolerance = .5,
                                       apVar = T,
                                       minScale = .0000001,
                                       opt = "optim"), na.action = na.omit)


exp.5DelayMLM <- nlme(z ~ (10^(q0) * 10^((k+.5) *(exp(-10^(alpha)*10^(q0)*y)-1))),
                    data = subset(cleanLong, x == min(x)),
                    fixed = list(q0 ~ 1,  
                                 alpha ~ 1),
                    random = list(pdSymm(q0 + alpha ~ 1)),
                    start = list(fixed = coef(exp.5Delay)),
                    groups = ~id,
                    method = "ML",
                    verbose = 2,
                    control = list(msMaxIter = 50000,
                                   niterEM = 5000,
                                   maxIter = 5000,
                                   pnlsTol = .01,
                                   tolerance = .2,
                                   apVar = T,
                                   minScale = .0000001,
                                   opt = "optim"), na.action = na.omit)

exp.5DelayCovMLM <- nlme(z ~ (10^(q0) * 10^((k+.5) *(exp(-10^(alpha)*10^(q0)*y)-1))),
                       data = subset(cleanLong, x == min(x)),
                       fixed = list(q0 ~ scale(FTNDnum),  
                                    alpha ~ scale(FTNDnum)),
                       random = list(pdSymm(q0 + alpha ~ 1)),
                       start = list(fixed = coef(exp.5DelayCovGNLS)),
                       groups = ~id,
                       method = "ML",
                       verbose = 2,
                       control = list(msMaxIter = 50000,
                                      niterEM = 5000,
                                      maxIter = 5000,
                                      pnlsTol = .01,
                                      tolerance = .01,
                                      apVar = T,
                                      minScale = .0000001,
                                      opt = "optim"), na.action = na.omit)

summary(expDelayCovMLM)
round(summary(exp.5DelayCovMLM)$tTable, 3)

anova(rachlinDelayMLM, expDelayMLM, exp.5DelayMLM)
anova(rachlinDelayCovMLM, expDelayCovMLM, exp.5DelayCovMLM)

AICc(exp.5DemandCovMLM)
AICc(rachlinDemandCovMLM)

AICc(exp.5DelayCovMLM)
AICc(rachlinDelayCovMLM)

anova(rachlinDemandMLM, expDemandMLM, exp.5DemandMLM)
anova(rachlinDemandCovMLM, expDemandCovMLM, exp.5DemandCovMLM)

twoDcomparisons <- rbind(
    anova(expDemandMLM, exp.5DemandMLM, rachlinDemandMLM),
    anova(rachlinDelayMLM, expDelayMLM, exp.5DelayMLM)
)

rSquarerDemand <- function(beep){
    1 - sum((predict(beep) - subset(cleanLong, y == min(y))$z)^2) / sum((mean(subset(cleanLong, y == min(y))$z) - subset(cleanLong, y == min(y))$z)^2)
}
RMSEDemand <- function(beep){
    sqrt(mean((predict(beep) - subset(cleanLong, y == min(y))$z)^2))
}
MAEDemand <- function(beep){
    mean(abs(predict(beep) - subset(cleanLong, y == min(y))$z))
}

rSquarerDelay <- function(beep){
    1 - sum((predict(beep) - subset(cleanLong, x == min(x))$z)^2) / sum((mean(subset(cleanLong, x == min(x))$z) - subset(cleanLong, x == min(x))$z)^2)
}
RMSEDelay <- function(beep){
    sqrt(mean((predict(beep) - subset(cleanLong, x == min(x))$z)^2))
}
MAEDelay <- function(beep){
    mean(abs(predict(beep) - subset(cleanLong, x == min(x))$z))
}


r22D <- c(rSquarerDemand(expDemandMLM),
          rSquarerDemand(exp.5DemandMLM),
          rSquarerDemand(rachlinDemandMLM),
          rSquarerDelay(rachlinDelayMLM),
          rSquarerDelay(expDelayMLM),
          rSquarerDelay(exp.5DelayMLM)
          
)
rmse2D <- c(RMSEDemand(expDemandMLM),
            RMSEDemand(exp.5DemandMLM),
            RMSEDemand(rachlinDemandMLM),
            RMSEDelay(rachlinDelayMLM),
            RMSEDelay(expDelayMLM),
            RMSEDelay(exp.5DelayMLM)
)
mae2D <- c(MAEDemand(expDemandMLM),
           MAEDemand(exp.5DemandMLM),
           MAEDemand(rachlinDemandMLM),
           MAEDelay(rachlinDelayMLM),
           MAEDelay(expDelayMLM),
           MAEDelay(exp.5DelayMLM)
)

AICc2D <- c(AICc(expDemandMLM),
            AICc(exp.5DemandMLM),
            AICc(rachlinDemandMLM),
            AICc(rachlinDelayMLM),
            AICc(expDelayMLM),
            AICc(exp.5DelayMLM)
)


twoDcomparisons$call <- NULL
twoDcomparisons <- data.frame(twoDcomparisons)[,-c(6:8)]
data.frame(twoDcomparisons, RMSE = rmse2D, MAE = mae2D, R2 = r22D)

rbind(AICc(expDemandMLM), AICc(exp.5DemandMLM), AICc(rachlinDemandMLM))
rbind(AICc(expDelayMLM), AICc(exp.5DelayMLM), AICc(rachlinDelayMLM))

anova(expDemandCovMLM, rachlinDemandCovMLM)
anova(rachlinDelayCovMLM, expDelayCovMLM)
weightMaker <- function(x){
    weights <- exp(-(1/2) * (x-min(x)))
    probs <- exp(-(1/2) * (x-min(x))) / sum(exp(-(1/2) * (x-min(x))))
    return(list(weights, probs))
}

weightMaker(c(AICc(exp.5DemandMLM), AICc(rachlinDemandMLM)))
weightMaker(c(AICc(rachlinDelayMLM), AICc(exp.5DelayMLM)))
weightMaker(c(BIC(exp.5DemandMLM), BIC(rachlinDemandMLM)))
weightMaker(c(BIC(rachlinDelayMLM), BIC(exp.5DelayMLM)))


# Equations used for analyses, ordered as presented in manuscript
koffExpDelDamMazurDenomForm <- z ~ (10^(q0) * 10^((k + .5) * (exp(-10^(alpha) * 10^(q0) * (x)) - 1))) / (1 + 10^b * y)
koffExpDelDamRachlinDenomForm <- z ~ (10^(q0) * 10^(k * (exp(-10^(alpha) * 10^(q0) * (x)) - 1))) / (1 + 10^b * y ^ s)
koff.5DelDamRachlinDenomForm <- z ~ (10^(q0) * 10^((k + .5) * (exp(-10^(alpha) * 10^(q0) * (x)) - 1))) / (1 + 10^b * y ^ s)
koffExpDelDamMGDenomForm <- z ~ (10^(q0) * 10^((k + .5) * (exp(-10^(alpha) * 10^(q0) * (x)) - 1))) / ((1 + 10^b * y )^ s)
koffMazurAddForm <- z ~ (10^(q0) * 10^((k + .5) * (exp(-10^(alpha) * 10^(q0) * (x)) - 1))) - (10^q0 * (1- (1/(1 + 10^b * y))))
koffMGAddForm <- z ~ (10^(q0) * 10^((k + .5) * (exp(-10^(alpha) * 10^(q0) * (x)) - 1))) - (10^q0 * (1- (1/(1 + 10^b * y) ^ s)))
koffRachlinAddForm <- z ~ (10^(q0) * 10^((k + .5) * (exp(-10^(alpha) * 10^(q0) * (x)) - 1))) - (10^q0 * (1- (1/(1 + 10^b * y ^ s))))
koffExpDelDamMazurDenomAlt <- z ~ (10^(q0) * 10^((k + .5) * (exp(-10^(alpha) * 10^(q0) * (y)) - 1))) / (1 + 10^b * x)
koffExpDelDamRachlinDenomAlt <- z ~ (10^(q0) * 10^((k + .5) * (exp(-10^(alpha) * 10^(q0) * (y)) - 1))) / (1 + 10^b * x ^ s)
koffExpDelDamMGDenomAlt <- z ~ (10^(q0) * 10^((k + .5) * (exp(-10^(alpha) * 10^(q0) * (y)) - 1))) / ((1 + 10^b * x )^ s)
koffMazurAddAlt <- z ~ (10^(q0) * 10^((k + .5) * (exp(-10^(alpha) * 10^(q0) * (y)) - 1))) - (10^q0 * (1- (1/(1 + 10^b * x))))
koffMGAddAlt <- z ~ (10^(q0) * 10^((k + .5) * (exp(-10^(alpha) * 10^(q0) * (y)) - 1))) - (10^q0 * (1- (1/(1 + 10^b * x) ^ s)))
koffRachlinAddAlt <- z ~ (10^(q0) * 10^((k + .5) * (exp(-10^(alpha) * 10^(q0) * (y)) - 1))) - (10^q0 * (1- (1/(1 + 10^b * x ^ s))))
delDamMazurForm <- z ~ (10^(q0)) / ((1 + 10^b * y) * (1 + 10^c * x))
delDamMGForm <- z ~ (10^(q0)) / (((1 + 10^b * y) ^ s) * ((1 + 10^c * x) ^ t))
delDamRachlinForm <- z ~ (10^(q0)) / ((1 + 10^b * y ^ s) * (1 + 10^c * x ^ t))
koffExpDelDamForm <- z ~ 10^(q0) * 10^(k * (exp(-10^(alpha) * 10^(q0) * (x)) - 1)) * 10^(k * (exp(-10^(beta) * 10^(q0) * (y)) - 1))
koffExpDelDam.5Form <- z ~ 10^(q0) * 10^((k + .5) * (exp(-10^(alpha) * 10^(q0) * (x)) - 1)) * 10^((k + .5) * (exp(-10^(beta) * 10^(q0) * (y)) - 1))
koffExpDelDamScalesForm <- z ~ 10^(q0) * 10^(k * (exp(-10^(alpha) * 10^(q0) * (x^t)) - 1)) * 10^(k * (exp(-10^(beta) * 10^(q0) * (y^s)) - 1))
koffExpDelDamScales.5Form <- z ~ 10^(q0) * 10^((k+.5) * (exp(-10^(alpha) * 10^(q0) * (x^t)) - 1)) * 10^((k + .5) * (exp(-10^(beta) * 10^(q0) * (y^s)) - 1))


# Determinining start values for MLMs ----

delDamKoffMazurAdd <- nls_multstart(koffMazurAddForm, data = cleanLong,
                                   iter = 250,
                                   start_lower = c(q0 = .1, alpha = -4, b = -4),
                                   start_upper = c(q0 = 2, alpha = 4, b = 4))
delDamKoffMGAdd <- nls_multstart(koffMGAddForm, data = cleanLong,
                                   iter = 250,
                                   start_lower = c(q0 = .1, alpha = -4, b = -4, s = .1),
                                   start_upper = c(q0 = 2, alpha = 4, b = 4, s = 4))
delDamKoffRachAdd <- nls_multstart(koffRachlinAddForm, data = cleanLong,
                                   iter = 250,
                                   start_lower = c(q0 = .1, alpha = -4, b = -4, s = .1),
                                   start_upper = c(q0 = 2, alpha = 4, b = 4, s = 4))

delDamKoffMazurDenomLog <- nls_multstart(koffExpDelDamMazurDenomForm, data = cleanLong,
                                         iter = 250,
                                         start_lower = c(q0 = .1, alpha = -4, b = -4),
                                         start_upper = c(q0 = 2, alpha = 4, b = 4))

delDamKoffRachlinDenomLog <- nls_multstart(koffExpDelDamRachlinDenomForm, data = cleanLong,
                                           iter = 250,
                                           start_lower = c(q0 = .1, alpha = -4, b = -4, s = .1),
                                           start_upper = c(q0 = 2, alpha = 4, b = 4, s = 4))

delDamKoffMGDenomLog <- nls_multstart(koffExpDelDamMGDenomForm, data = cleanLong,
                                           iter = 250,
                                           start_lower = c(q0 = .1, alpha = -4, b = -4, s = .1),
                                           start_upper = c(q0 = 2, alpha = 4, b = 4, s = 4))

delDamKoff.5RachlinDenomLog <- nls_multstart(koff.5DelDamRachlinDenomForm, data = cleanLong,
                                           iter = 250,
                                           start_lower = c(q0 = .1, alpha = -4, b = -4, s = .1),
                                           start_upper = c(q0 = 2, alpha = 4, b = 4, s = 4))

altKoffMazurAdd <- nls_multstart(koffMazurAddAlt, data = cleanLong,
                                 iter = 250,
                                 start_lower = c(q0 = .1, alpha = -4, b = -4),
                                 start_upper = c(q0 = 2, alpha = 4, b = 4))
altKoffMGAdd <- nls_multstart(koffMGAddAlt, data = cleanLong,
                              iter = 250,
                              start_lower = c(q0 = .1, alpha = -4, b = -4, s = .1),
                              start_upper = c(q0 = 2, alpha = 4, b = 4, s = 4))
altKoffRachAdd <- nls_multstart(koffRachlinAddAlt, data = cleanLong,
                                iter = 250,
                                start_lower = c(q0 = .1, alpha = -4, b = -4, s = .1),
                                start_upper = c(q0 = 2, alpha = 4, b = 4, s = 4))

altKoffMazurDenomLog <- nls_multstart(koffExpDelDamMazurDenomAlt, data = cleanLong,
                                      iter = 250,
                                      start_lower = c(q0 = .1, alpha = -4, b = -4),
                                      start_upper = c(q0 = 2, alpha = 4, b = 4))

altKoffMGDenomLog <- nls_multstart(koffExpDelDamMGDenomAlt, data = cleanLong,
                                   iter = 250,
                                   start_lower = c(q0 = .1, alpha = -4, b = -4, s = .1),
                                   start_upper = c(q0 = 2, alpha = 4, b = 4, s = 4))

altKoffRachlinDenomLog <- nls_multstart(koffExpDelDamRachlinDenomAlt, data = cleanLong,
                                        iter = 250,
                                        start_lower = c(q0 = .1, alpha = -4, b = -4, s = .1),
                                        start_upper = c(q0 = 2, alpha = 4, b = 4, s = 4))

delDamMazurDenomLog <- nls_multstart(delDamMazurForm, data = cleanLong,
                                           iter = 250,
                                           start_lower = c(q0 = .1, b = -4, c = -4),
                                           start_upper = c(q0 = 2, b = 4, c = 4))

delDamMGDenomLog <- nls_multstart(delDamMGForm, data = cleanLong,
                                           iter = 250,
                                           start_lower = c(q0 = .1, b = -4, s = .1, c = -4, t = .1),
                                           start_upper = c(q0 = 2, b = 4, s = 4, c = 4, t = 4))

delDamRachlinDenomLog <- nls_multstart(delDamRachlinForm, data = cleanLong,
                                           iter = 250,
                                           start_lower = c(q0 = .1, b = -4, s = .1, c = -4, t = .1),
                                           start_upper = c(q0 = 2, b = 4, s = 4, c = 4, t = 4))

delDamExpLog <- nls_multstart(koffExpDelDamForm, data = cleanLong,
                                           iter = 250,
                                           start_lower = c(q0 = .1, alpha = -4, beta = -4),
                                           start_upper = c(q0 = 2, alpha = 4, beta = 4))
delDamExp.5Log <- nls_multstart(koffExpDelDam.5Form, data = cleanLong,
                                           iter = 250,
                                           start_lower = c(q0 = .1, alpha = -4, beta = -4),
                                           start_upper = c(q0 = 2, alpha = 4, beta = 4))

delDamExpScalesLog <- nls_multstart(koffExpDelDamScalesForm, data = data.frame(cleanLong),
                                           iter = 250,
                                           start_lower = c(q0 = .1, alpha = -4, t = .1, beta = -4, s = .1),
                                           start_upper = c(q0 = 2, alpha = 4, t = 4, beta = 4, s = 4))

delDamExpScales.5Log <- nls_multstart(koffExpDelDamScales.5Form, data = data.frame(cleanLong),
                                           iter = 250,
                                           start_lower = c(q0 = .1, alpha = -4, t = .1, beta = -4, s = .1),
                                           start_upper = c(q0 = 2, alpha = 4, t = 4, beta = 4, s = 4))

# MLM comparisons----
nlmeMazurAddLog <- nlme(koffMazurAddForm, 
                   data = cleanLong,
                   fixed = list(q0 ~ 1,  
                                alpha ~ 1,
                                b ~ 1),
                   random = list(pdSymm(q0 + alpha + b~ 1)),
                   start = list(fixed = coef(delDamKoffMazurAdd)), 
                   groups = ~id,
                   method = "ML",
                   verbose = 2,
                   control = list(msMaxIter = 50000,
                                  niterEM = 5000,
                                  maxIter = 5000,
                                  pnlsTol = .0001,
                                  tolerance = .3,
                                  apVar = T,
                                  minScale = .0000001,
                                  opt = "optim"))
nlmeMGAddLog <- nlme(koffMGAddForm, 
                   data = cleanLong,
                   fixed = list(q0 ~ 1,  
                                alpha ~ 1,
                                b ~ 1,
                                s ~ 1),
                   random = list(pdSymm(q0 + alpha + b + s ~ 1)),
                   start = list(fixed = coef(delDamKoffMGAdd)), 
                   groups = ~id,
                   method = "ML",
                   verbose = 2,
                   control = list(msMaxIter = 50000,
                                  niterEM = 5000,
                                  maxIter = 5000,
                                  pnlsTol = .0001,
                                  tolerance = .3,
                                  apVar = T,
                                  minScale = .0000001,
                                  opt = "optim"))
nlmeRachAddLog <- nlme(koffRachlinAddForm, 
                   data = cleanLong,
                   fixed = list(q0 ~ 1,  
                                alpha ~ 1,
                                b ~ 1,
                                s ~ 1),
                   random = list(pdSymm(q0 + alpha + b + s ~ 1)),
                   start = list(fixed = coef(delDamKoffRachAdd)), 
                   groups = ~id,
                   method = "ML",
                   verbose = 2,
                   control = list(msMaxIter = 50000,
                                  niterEM = 5000,
                                  maxIter = 5000,
                                  pnlsTol = .0001,
                                  tolerance = .3,
                                  apVar = T,
                                  minScale = .0000001,
                                  opt = "optim"))



nlmeMixedMazurLog <- nlme(koffExpDelDamMazurDenomForm, 
                            data = cleanLong,
                            fixed = list(q0 ~ 1,  
                                         alpha ~ 1,
                                         b ~ 1),
                            random = list(pdSymm(q0 + alpha + b ~ 1)),
                            start = list(fixed = coef(delDamKoffMazurDenomLog)), 
                            groups = ~id,
                            method = "ML",
                            verbose = 2,
                            control = list(msMaxIter = 50000,
                                           niterEM = 5000,
                                           maxIter = 5000,
                                           pnlsTol = .0001,
                                           tolerance = .2,
                                           apVar = T,
                                           minScale = .0000001,
                                           opt = "optim"))

nlmeMixedRachlinLog <- nlme(koffExpDelDamRachlinDenomForm, 
                              data = cleanLong,
                              fixed = list(q0 ~ 1,  
                                           alpha ~ 1,
                                           b ~ 1,
                                           s ~ 1),
                              random = list(pdSymm(q0 + alpha + b + s ~ 1)),
                              start = list(fixed = coef(delDamKoffRachlinDenomLog)), 
                              groups = ~id,
                              method = "ML",
                              verbose = 2,
                              control = list(msMaxIter = 50000,
                                             niterEM = 5000,
                                             maxIter = 5000,
                                             pnlsTol = .0001,
                                             tolerance = .3,
                                             apVar = T,
                                             minScale = .0000001,
                                             opt = "optim"))

nlmeMixedMGLog <- nlme(koffExpDelDamMGDenomForm, 
                              data = cleanLong,
                              fixed = list(q0 ~ 1,  
                                           alpha ~ 1,
                                           b ~ 1,
                                           s ~ 1),
                              random = list(pdSymm(q0 + alpha + b + s ~ 1)),
                              start = list(fixed = coef(delDamKoffMGDenomLog)), 
                              groups = ~id,
                              method = "ML",
                              verbose = 2,
                              control = list(msMaxIter = 50000,
                                             niterEM = 5000,
                                             maxIter = 5000,
                                             pnlsTol = .0001,
                                             tolerance = .5,
                                             apVar = T,
                                             minScale = .0000001,
                                             opt = "optim"))

nlmeMixed.5RachlinLog <- nlme(koff.5DelDamRachlinDenomForm, 
                             data = cleanLong,
                             fixed = list(q0 ~ 1,  
                                          alpha ~ 1,
                                          b ~ 1,
                                          s ~ 1),
                             random = list(pdSymm(q0 + alpha + b + s ~ 1)),
                             start = list(fixed = coef(delDamKoff.5RachlinDenomLog)), 
                             groups = ~id,
                             method = "ML",
                             verbose = 2,
                             control = list(msMaxIter = 50000,
                                            niterEM = 5000,
                                            maxIter = 5000,
                                            pnlsTol = .0001,
                                            tolerance = .4,
                                            apVar = T,
                                            minScale = .0000001,
                                            opt = "optim"))

nlmeMazurAddLogAlt <- nlme(koffMazurAddAlt, 
                           data = cleanLong,
                           fixed = list(q0 ~ 1,  
                                        alpha ~ 1,
                                        b ~ 1),
                           random = list(pdSymm(q0 + alpha + b~ 1)),
                           start = list(fixed = coef(altKoffMazurAdd)), 
                           groups = ~id,
                           method = "ML",
                           verbose = 2,
                           control = list(msMaxIter = 50000,
                                          niterEM = 5000,
                                          maxIter = 5000,
                                          pnlsTol = .0001,
                                          tolerance = .2,
                                          apVar = T,
                                          minScale = .0000001,
                                          opt = "optim"))
nlmeMGAddLogAlt <- nlme(koffMGAddAlt, 
                        data = cleanLong,
                        fixed = list(q0 ~ 1,  
                                     alpha ~ 1,
                                     b ~ 1,
                                     s ~ 1),
                        random = list(pdSymm(q0 + alpha + b + s ~ 1)),
                        start = list(fixed = coef(altKoffMGAdd)), 
                        groups = ~id,
                        method = "ML",
                        verbose = 2,
                        control = list(msMaxIter = 50000,
                                       niterEM = 5000,
                                       maxIter = 5000,
                                       pnlsTol = .0001,
                                       tolerance = .3,
                                       apVar = T,
                                       minScale = .0000001,
                                       opt = "optim"))
nlmeRachAddLogAlt <- nlme(koffRachlinAddAlt, 
                          data = cleanLong,
                          fixed = list(q0 ~ 1,  
                                       alpha ~ 1,
                                       b ~ 1,
                                       s ~ 1),
                          random = list(pdSymm(q0 + alpha + b + s ~ 1)),
                          start = list(fixed = coef(altKoffRachAdd)), 
                          groups = ~id,
                          method = "ML",
                          verbose = 2,
                          control = list(msMaxIter = 50000,
                                         niterEM = 5000,
                                         maxIter = 5000,
                                         pnlsTol = .0001,
                                         tolerance = .2,
                                         apVar = T,
                                         minScale = .0000001,
                                         opt = "optim"))

nlmeMixedMazurLogAlt <- nlme(koffExpDelDamMazurDenomAlt, 
                             data = cleanLong,
                             fixed = list(q0 ~ 1,  
                                          alpha ~ 1,
                                          b ~ 1),
                             random = list(pdSymm(q0 + alpha + b ~ 1)),
                             start = list(fixed = coef(altKoffMazurDenomLog)), 
                             groups = ~id,
                             method = "ML",
                             verbose = 2,
                             control = list(msMaxIter = 50000,
                                            niterEM = 5000,
                                            maxIter = 5000,
                                            pnlsTol = .0001,
                                            tolerance = .2,
                                            apVar = T,
                                            minScale = .0000001,
                                            opt = "optim"))

nlmeMixedMGLogAlt <- nlme(koffExpDelDamMGDenomAlt, 
                          data = cleanLong,
                          fixed = list(q0 ~ 1,  
                                       alpha ~ 1,
                                       b ~ 1,
                                       s ~ 1),
                          random = list(pdSymm(q0 + alpha + b + s ~ 1)),
                          start = list(fixed = coef(altKoffMGDenomLog)), 
                          groups = ~id,
                          method = "ML",
                          verbose = 2,
                          control = list(msMaxIter = 50000,
                                         niterEM = 5000,
                                         maxIter = 5000,
                                         pnlsTol = .0001,
                                         tolerance = .5,
                                         apVar = T,
                                         minScale = .0000001,
                                         opt = "optim"))

nlmeMixedRachlinLogAlt <- nlme(koffExpDelDamRachlinDenomAlt, 
                               data = cleanLong,
                               fixed = list(q0 ~ 1,  
                                            alpha ~ 1,
                                            b ~ 1,
                                            s ~ 1),
                               random = list(pdSymm(q0 + alpha + b + s ~ 1)),
                               start = list(fixed = coef(altKoffRachlinDenomLog)), 
                               groups = ~id,
                               method = "ML",
                               verbose = 2,
                               control = list(msMaxIter = 50000,
                                              niterEM = 5000,
                                              maxIter = 5000,
                                              pnlsTol = .0001,
                                              tolerance = .4,
                                              apVar = T,
                                              minScale = .0000001,
                                              opt = "optim"))

nlmeMazurOnlyLog <- nlme(delDamMazurForm, 
                         data = cleanLong,
                         fixed = list(q0 ~ 1,  
                                      b ~ 1,
                                      c ~ 1
                         ),
                         random = list(pdSymm(q0 + b + c ~ 1)),
                         start = list(fixed = coef(delDamMazurDenomLog)), 
                         groups = ~id,
                         method = "ML",
                         verbose = 2,
                         control = list(msMaxIter = 50000,
                                        niterEM = 5000,
                                        maxIter = 5000,
                                        pnlsTol = .0001,
                                        tolerance = .05,
                                        apVar = T,
                                        minScale = .0000001,
                                        opt = "optim"))
nlmeMGOnlyLog <- nlme(delDamMGForm, 
                      data = cleanLong,
                      fixed = list(q0 ~ 1,  
                                   b ~ 1,
                                   s ~ 1,
                                   c ~ 1,
                                   t ~ 1),
                      random = list(pdSymm(q0 + b + s + c + t ~ 1)),
                      start = list(fixed = coef(delDamMGDenomLog)), 
                      groups = ~id,
                      method = "ML",
                      verbose = 2,
                      control = list(msMaxIter = 50000,
                                     niterEM = 5000,
                                     maxIter = 5000,
                                     pnlsTol = .0001,
                                     tolerance = .85,
                                     apVar = T,
                                     minScale = .0000001,
                                     opt = "optim"))

nlmeRachlinOnlyLog <- nlme(delDamRachlinForm, 
                           data = cleanLong,
                           fixed = list(q0 ~ 1,  
                                        b ~ 1,
                                        s ~ 1,
                                        c ~ 1,
                                        t ~ 1),
                           random = list(pdSymm(q0 + b + s + c + t ~ 1)),
                           start = list(fixed = coef(delDamRachlinDenomLog)), 
                           groups = ~id,
                           method = "ML",
                           verbose = 2,
                           control = list(msMaxIter = 50000,
                                          niterEM = 5000,
                                          maxIter = 5000,
                                          pnlsTol = .0001,
                                          tolerance = .05,
                                          apVar = T,
                                          minScale = .0000001,
                                          opt = "optim"))

nlmeExpOnlyLog <- nlme(koffExpDelDamForm, 
                       data = cleanLong,
                       fixed = list(q0 ~ 1,  
                                    alpha ~ 1,
                                    beta ~ 1),
                       random = list(pdSymm(q0 + alpha + beta ~ 1)),
                       start = list(fixed = coef(delDamExpLog)), 
                       groups = ~id,
                       method = "ML",
                       verbose = 2,
                       control = list(msMaxIter = 50000,
                                      niterEM = 5000,
                                      maxIter = 5000,
                                      pnlsTol = .0001,
                                      tolerance = .05,
                                      apVar = T,
                                      minScale = .0000001,
                                      opt = "optim"))

nlmeExpOnly.5Log <- nlme(koffExpDelDam.5Form, 
                       data = cleanLong,
                       fixed = list(q0 ~ 1,  
                                    alpha ~ 1,
                                    beta ~ 1),
                       random = list(pdSymm(q0 + alpha + beta ~ 1)),
                       start = list(fixed = coef(delDamExp.5Log)), 
                       groups = ~id,
                       method = "ML",
                       verbose = 2,
                       control = list(msMaxIter = 50000,
                                      niterEM = 5000,
                                      maxIter = 5000,
                                      pnlsTol = .0001,
                                      tolerance = .05,
                                      apVar = T,
                                      minScale = .0000001,
                                      opt = "optim"))

nlmeExpScalesLog <- nlme(koffExpDelDamScalesForm, 
                       data = data.frame(cleanLong),
                       fixed = list(q0 ~ 1,  
                                    alpha ~ 1,
                                    t ~ 1,
                                    beta ~ 1,
                                    s ~ 1),
                       random = list(pdSymm(q0 + alpha  + t + beta + s~ 1)),
                       start = list(fixed = coef(delDamExpScalesLog)), 
                       groups = ~id,
                       method = "ML",
                       verbose = 2,
                       control = list(msMaxIter = 50000,
                                      niterEM = 5000,
                                      maxIter = 5000,
                                      pnlsTol = .0001,
                                      tolerance = .25,
                                      apVar = T,
                                      minScale = .0000001,
                                      opt = "optim"))

nlmeExpScales.5Log <- nlme(koffExpDelDamScales.5Form, 
                       data = data.frame(cleanLong),
                       fixed = list(q0 ~ 1,  
                                    alpha ~ 1,
                                    t ~ 1,
                                    beta ~ 1,
                                    s ~ 1),
                       random = list(pdSymm(q0 + alpha  + t + beta + s~ 1)),
                       start = list(fixed = coef(delDamExpScales.5Log)), 
                       groups = ~id,
                       method = "ML",
                       verbose = 2,
                       control = list(msMaxIter = 50000,
                                      niterEM = 5000,
                                      maxIter = 5000,
                                      pnlsTol = .0001,
                                      tolerance = .2,
                                      apVar = T,
                                      minScale = .0000001,
                                      opt = "optim"))


anova(nlmeExpScalesLog, nlmeExpScales.5Log, nlmeRachlinOnlyLog)

# other metrics ----

RMSE <- function(beep){
    sqrt(mean((predict(beep) - cleanLong$z)^2))
}
MAE <- function(beep){
    mean(abs(predict(beep) - cleanLong$z))
}

rSquarer <- function(beep){
    1 - sum((predict(beep) - cleanLong$z)^2) / sum((mean(cleanLong$z) - cleanLong$z)^2)
}

r2Overall <- c(rSquarer(nlmeMazurAddLog),
               rSquarer(nlmeMGAddLog),
               rSquarer(nlmeRachAddLog),
               rSquarer(nlmeMixedMazurLog),
               rSquarer(nlmeMixedMGLog),
               # rSquarer(nlmeMixedRachlinLog),
               rSquarer(nlmeMixed.5RachlinLog),
               rSquarer(nlmeMazurAddLogAlt),
               rSquarer(nlmeMGAddLogAlt),
               rSquarer(nlmeRachAddLogAlt),
               rSquarer(nlmeMixedMazurLogAlt),
               rSquarer(nlmeMixedMGLogAlt),
               rSquarer(nlmeMixedRachlinLogAlt),
               rSquarer(nlmeMazurOnlyLog),
               rSquarer(nlmeMGOnlyLog),
               rSquarer(nlmeRachlinOnlyLog),
               # rSquarer(nlmeExpOnlyLog),
               rSquarer(nlmeExpOnly.5Log),
               # rSquarer(nlmeExpScalesLog),
               rSquarer(nlmeExpScales.5Log)
)
rmseOverall <- c(RMSE(nlmeMazurAddLog),
                 RMSE(nlmeMGAddLog),
                 RMSE(nlmeRachAddLog),
                 RMSE(nlmeMixedMazurLog),
                 RMSE(nlmeMixedMGLog),
                 # RMSE(nlmeMixedRachlinLog),
                 RMSE(nlmeMixed.5RachlinLog),
                 RMSE(nlmeMazurAddLogAlt),
                 RMSE(nlmeMGAddLogAlt),
                 RMSE(nlmeRachAddLogAlt),
                 RMSE(nlmeMixedMazurLogAlt),
                 RMSE(nlmeMixedMGLogAlt),
                 RMSE(nlmeMixedRachlinLogAlt),
                 RMSE(nlmeMazurOnlyLog),
                 RMSE(nlmeMGOnlyLog),
                 RMSE(nlmeRachlinOnlyLog),
                 RMSE(nlmeExpOnlyLog),
                 # RMSE(nlmeExpOnly.5Log),
                 # RMSE(nlmeExpScalesLog),
                 RMSE(nlmeExpScales.5Log)
)
maeOverall <- c(MAE(nlmeMazurAddLog),
                MAE(nlmeMGAddLog),
                MAE(nlmeRachAddLog),
                MAE(nlmeMixedMazurLog),
                MAE(nlmeMixedMGLog),
                # MAE(nlmeMixedRachlinLog),
                MAE(nlmeMixed.5RachlinLog),
                MAE(nlmeMazurAddLogAlt),
                MAE(nlmeMGAddLogAlt),
                MAE(nlmeRachAddLogAlt),
                MAE(nlmeMixedMazurLogAlt),
                MAE(nlmeMixedMGLogAlt),
                MAE(nlmeMixedRachlinLogAlt),
                MAE(nlmeMazurOnlyLog),
                MAE(nlmeMGOnlyLog),
                MAE(nlmeRachlinOnlyLog),
                # MAE(nlmeExpOnlyLog),
                MAE(nlmeExpOnly.5Log),
                # MAE(nlmeExpScalesLog),
                MAE(nlmeExpScales.5Log)
)
AICcOverall <- c(AICc(nlmeMazurAddLog),
                AICc(nlmeMGAddLog),
                AICc(nlmeRachAddLog),
                AICc(nlmeMixedMazurLog),
                AICc(nlmeMixedMGLog),
                # AICc(nlmeMixedRachlinLog),
                AICc(nlmeMixed.5RachlinLog),
                AICc(nlmeMazurAddLogAlt),
                AICc(nlmeMGAddLogAlt),
                AICc(nlmeRachAddLogAlt),
                AICc(nlmeMixedMazurLogAlt),
                AICc(nlmeMixedMGLogAlt),
                AICc(nlmeMixedRachlinLogAlt),
                AICc(nlmeMazurOnlyLog),
                AICc(nlmeMGOnlyLog),
                AICc(nlmeRachlinOnlyLog),
                # AICc(nlmeExpOnlyLog),
                AICc(nlmeExpOnly.5Log),
                # AICc(nlmeExpScalesLog),
                AICc(nlmeExpScales.5Log)
)

allModelComparisons <- anova((nlmeMazurAddLog),
                             (nlmeMGAddLog),
                             (nlmeRachAddLog),
                             (nlmeMixedMazurLog),
                             (nlmeMixedMGLog),
                             # (nlmeMixedRachlinLog),
                             (nlmeMixed.5RachlinLog),
                             (nlmeMazurAddLogAlt),
                             (nlmeMGAddLogAlt),
                             (nlmeRachAddLogAlt),
                             (nlmeMixedMazurLogAlt),
                             (nlmeMixedMGLogAlt),
                             (nlmeMixedRachlinLogAlt),
                             (nlmeMazurOnlyLog),
                             (nlmeMGOnlyLog),
                             (nlmeRachlinOnlyLog),
                             # (nlmeExpOnlyLog),
                             (nlmeExpOnly.5Log),
                             # (nlmeExpScalesLog),
                             (nlmeExpScales.5Log))
allModelComparisons$call <- NULL
allModelComparisons <- data.frame(allModelComparisons)[,-c(6:8)]
data.frame(allModelComparisons, RMSE = rmseOverall, MAE = maeOverall, R2 = r2Overall, AICc = AICcOverall)[, c(1,2,3,9,4,5,6,7,8)]

cbind(weightMaker(AICcOverall)[[2]])
cbind(weightMaker(allModelComparisons$BIC)[[2]])
cbind(AICcOverall)
# Select MLMs with FTND ----
delDamExpGNLSFTND <- gnls(koffExpDelDamRachlinDenomForm,
                          data = cleanLong,
                          params = list(q0 ~ scale(FTNDnum),  
                                        alpha ~ scale(FTNDnum),
                                        b ~ scale(FTNDnum),
                                        s ~ scale(FTNDnum)),
                          start = list(fixed = c(coef(delDamKoffRachlinDenomLog)[1], 0, coef(delDamKoffRachlinDenomLog)[2], 0,
                                                 coef(delDamKoffRachlinDenomLog)[3], 0, coef(delDamKoffRachlinDenomLog)[4]), 0),
                          verbose = 2,
                          control = list(msMaxIter = 50000,
                                         niterEM = 5000,
                                         maxIter = 5000,
                                         pnlsTol = .01,
                                         tolerance = .1,
                                         apVar = T,
                                         minScale = .0000001,
                                         opt = "optim"), na.action = na.omit)


nlmedelDamExpFTND <- nlme(koffExpDelDamRachlinDenomForm,
                          data = cleanLong,
                          fixed = list(q0 ~ scale(FTNDnum),  
                                       alpha ~ scale(FTNDnum),
                                       b ~ scale(FTNDnum),
                                       s ~ scale(FTNDnum)),
                          random = list(pdSymm(q0 + alpha + b + s ~ 1)),
                          start = list(fixed = coef(delDamExpGNLSFTND)),
                          groups = ~id,
                          method = "ML",
                          verbose = 2,
                          control = list(msMaxIter = 50000,
                                         niterEM = 5000,
                                         maxIter = 5000,
                                         pnlsTol = .01,
                                         tolerance = .01,
                                         apVar = T,
                                         minScale = .0000001,
                                         opt = "optim"), na.action = na.omit)

summary(nlmedelDamExpFTND)
round(summary(nlmedelDamExpFTND)$tTable, 3)

delDamRachlinGNLSFTND <- gnls(delDamRachlinForm,
                              data = cleanLong,
                              params = list(q0 ~ scale(FTNDnum),  
                                            b ~ scale(FTNDnum),
                                            s ~ scale(FTNDnum),
                                            c ~ scale(FTNDnum),
                                            t ~ scale(FTNDnum)),
                              start = list(fixed = c(coef(delDamRachlinDenomLog)[1], 0, coef(delDamRachlinDenomLog)[2], 0,
                                                     coef(delDamRachlinDenomLog)[3], 0, coef(delDamRachlinDenomLog)[4], 0,
                                                     coef(delDamRachlinDenomLog)[5]), 0),
                              verbose = 2,
                              control = list(msMaxIter = 50000,
                                             niterEM = 5000,
                                             maxIter = 5000,
                                             pnlsTol = .01,
                                             tolerance = .1,
                                             apVar = T,
                                             minScale = .0000001,
                                             opt = "optim"), na.action = na.omit)


nlmedelDamRachFTND <- nlme(delDamRachlinForm,
                           data = cleanLong,
                           fixed = list(q0 ~ scale(FTNDnum),  
                                        b ~ scale(FTNDnum),
                                        s ~ scale(FTNDnum),
                                        c ~ scale(FTNDnum),
                                        t ~ scale(FTNDnum)),
                           random = list(pdSymm(q0 + b + s + c + t~ 1)),
                           start = list(fixed = coef(delDamRachlinGNLSFTND)),
                           groups = ~id,
                           method = "ML",
                           verbose = 2,
                           control = list(msMaxIter = 50000,
                                          niterEM = 5000,
                                          maxIter = 5000,
                                          pnlsTol = .01,
                                          tolerance = .2,
                                          apVar = T,
                                          minScale = .0000001,
                                          opt = "optim"), na.action = na.omit)

summary(nlmedelDamRachFTND)
round(summary(nlmedelDamRachFTND)$tTable, 3)

anova(nlmedelDamExpFTND, nlmedelDamRachFTND)

delDamExpScalesGNLSFTND <- gnls(koffExpDelDamScalesForm,
                              data = cleanLong,
                              params = list(q0 ~ scale(FTNDnum),  
                                            alpha ~ scale(FTNDnum),
                                            t ~ scale(FTNDnum),
                                            beta ~ scale(FTNDnum),
                                            s ~ scale(FTNDnum)),
                              start = list(fixed = c(coef(delDamExpScalesLog)[1], 0, coef(delDamExpScalesLog)[2], 0,
                                                     coef(delDamExpScalesLog)[3], 0, coef(delDamExpScalesLog)[4], 0,
                                                     coef(delDamExpScalesLog)[5]), 0),
                              verbose = 2,
                              control = list(msMaxIter = 50000,
                                             niterEM = 5000,
                                             maxIter = 5000,
                                             pnlsTol = .01,
                                             tolerance = .1,
                                             apVar = T,
                                             minScale = .0000001,
                                             opt = "optim"), na.action = na.omit)


nlmedelDamExpScalesFTND <- nlme(koffExpDelDamScalesForm,
                           data = cleanLong,
                           fixed = list(q0 ~ scale(FTNDnum),  
                                        alpha ~ scale(FTNDnum),
                                        t ~ scale(FTNDnum),
                                        beta ~ scale(FTNDnum),
                                        s ~ scale(FTNDnum)),
                           random = list(pdSymm(q0 + alpha + t + beta + s~ 1)),
                           start = list(fixed = coef(delDamExpScalesGNLSFTND)),
                           groups = ~id,
                           method = "ML",
                           verbose = 2,
                           control = list(msMaxIter = 50000,
                                          niterEM = 5000,
                                          maxIter = 5000,
                                          pnlsTol = .01,
                                          tolerance = .2,
                                          apVar = T,
                                          minScale = .0000001,
                                          opt = "optim"), na.action = na.omit)

nlmedelDamExpScales.5FTND <- nlme(koffExpDelDamScales.5Form,
                           data = cleanLong,
                           fixed = list(q0 ~ scale(FTNDnum),  
                                        alpha ~ scale(FTNDnum),
                                        t ~ scale(FTNDnum),
                                        beta ~ scale(FTNDnum),
                                        s ~ scale(FTNDnum)),
                           random = list(pdSymm(q0 + alpha + t + beta + s~ 1)),
                           start = list(fixed = coef(delDamExpScalesGNLSFTND)),
                           groups = ~id,
                           method = "ML",
                           verbose = 2,
                           control = list(msMaxIter = 50000,
                                          niterEM = 5000,
                                          maxIter = 5000,
                                          pnlsTol = .01,
                                          tolerance = .3,
                                          apVar = T,
                                          minScale = .0000001,
                                          opt = "optim"), na.action = na.omit)

anova(nlmedelDamExpFTND, nlmedelDamExpScalesFTND, nlmedelDamExpScales.5FTND, nlmedelDamRachFTND)

summary(nlmedelDamExpFTND)$tTable |> round(3)
summary(nlmedelDamExpScalesFTND)$tTable |> round(3)
summary(nlmedelDamExpScales.5FTND)$tTable |> round(3)
summary(nlmedelDamRachFTND)$tTable |> round(3)
rbind(
AICc(nlmedelDamRachFTND),
AICc(nlmedelDamExpScales.5FTND))

rbind(
BIC(nlmedelDamRachFTND),
BIC(nlmedelDamExpScales.5FTND))

rbind(
logLik(nlmedelDamRachFTND),
logLik(nlmedelDamExpScales.5FTND))

# Publication Figures ----



# 2D figure ----
yPoints <- 1.1^seq(-60, 114, len = 100)
xPoints <- 1.1^seq(-60, 25, len = 100)
pricePoints <- data.frame(id = rep(unique(cleanLong$id), each = 100), x = xPoints, k = 1.154949)
delayPoints <- data.frame(id = rep(unique(cleanLong$id), each = 100), y = yPoints, k = 1.154949)

allIndividualPredictionsPrice <- rbind(
    data.frame(predict(rachlinDemandMLM, newdata = pricePoints, level = 0:1), Model  = 'Rachlin'),
    data.frame(predict(exp.5DemandMLM, newdata = pricePoints, level = 0:1), Model  = 'EXPD'))

allIndividualPredictionsDelay <- rbind(
    data.frame(predict(rachlinDelayMLM, newdata = delayPoints, level = 0:1), Model  = 'Rachlin'),
    data.frame(predict(exp.5DelayMLM, newdata = delayPoints, level = 0:1), Model  = 'EXPD'))


allIndividualPredictionsPrice$xs <- xPoints
allIndividualPredictionsDelay$ys <- yPoints


candidateIndividuals <- c("A113", "A204", "A411","A261", "A337", "A413")

svg("2dfits.svg", width = 10, height = 7)
grid.arrange(
    subset(cleanLong, id %in% unique(cleanLong$id)[which(unique(cleanLong$id) %in% candidateIndividuals)] & y == min(y)) |> 
        ggplot(aes(x = x, y = z)) +
        geom_point(pch = 21, size = 2) +
        geom_line(data = subset(allIndividualPredictionsPrice, id %in% unique(cleanLong$id)[which(unique(cleanLong$id) %in% candidateIndividuals)]), 
                  aes(x = xs, y = predict.id, linetype = Model)) +
        scale_linetype_manual(values = c(5,1)) +
        facet_wrap(facets = ~id, ncol = 2) +
        theme_classic() +
        scale_x_log10(breaks = (unique(cleanLong$x)), labels = c("$0.01", "$0.03", "$0.06", "$0.13", "$0.25", "$0.50", "$1.00", "$2.00", "$4.00", "  $8.00"))+
        
        theme(legend.position = "top", plot.title = element_text(hjust = .5), aspect.ratio = .75, axis.text = element_text(colour = "black"),
              axis.text.x = element_text(angle = 50, vjust = 1, hjust=1)) +
        guides(linetype = guide_legend(nrow = 1)) +
        labs(title = "2D Price",
             y = "Cigarettes Purchased", x = "Price"),
    
    subset(cleanLong, id %in% unique(cleanLong$id)[which(unique(cleanLong$id) %in% candidateIndividuals)] & x == min(x)) |> 
        # ggplot(aes(x = ifelse(x == 0, .01, x), y = y)) +
        ggplot(aes(x = y, y = z)) +
        geom_point(pch = 22, size = 2) +
        geom_line(data = subset(allIndividualPredictionsDelay, id %in% unique(cleanLong$id)[which(unique(cleanLong$id) %in% candidateIndividuals)]), 
                  aes(x = ys, y = predict.id, linetype = Model)) +
        scale_linetype_manual(values = c(5,1)) +
        facet_wrap(facets = ~id, ncol = 2) +
        theme_classic() +
        scale_x_log10(breaks = (unique(cleanLong$y)), labels = c("5 Min", "6 Hour", "1 Day", "1 Week", "1 Month", "3 Month", "1 Year", "5 Year"))+
        theme(legend.position = "top", plot.title = element_text(hjust = .5), aspect.ratio = .75, axis.text = element_text(colour = "black"),
              axis.text.x = element_text(angle = 50, vjust = 1, hjust=1)) +
        guides(linetype = guide_legend(nrow = 1)) +
        labs(title = "2D Delay",
             y = "Cigarettes Purchased", x = "Delay"), ncol = 2
)
dev.off()

# Fixed Effects ----

# par(mfrow = c(2,1), mar = c(3,1,3,1))
svg("3dfixedeffects.svg", height = 11, width = 11)
par(mfrow = c(2,2), mar = c(3.5,3,3,2.5))
# dev.off()
grains <- 30
expand.grid(1:grains, 1:grains)
predictGrid <- expand.grid(x = xlong <- (1.1^seq(-50, 27, length.out = grains)), 
                           y = ylong <- (1.1^seq(-40, 120, length.out = grains)),
                           k = GetKZ(cleanLong), z = 1)
mixedGrid <- data.frame(id = rep(unique(cleanLong$id), each = nrow(expand.grid(1:grains, 1:grains))), 
                        predictGrid)

with(cleanLong[, .(z = median(z, na.rm = TRUE)), by = .(x,y)], {
    boldLines <- rbind(subset(cleanLong, y == min(y)), subset(cleanLong, x == min(x)))
    scatter3D(z = z, x = log10(x), y = log10(y), colkey = FALSE, zlim = c(0, 50), xlim = c(-2.069634, 1.25), ylim = c(-1.655707, 5.25),
              col = NA, pch = NA, phi = 18, theta = 135, ticktype = "detailed", xlab = "X - log10 Price",
              ylab = "Y - log10 Delay", zlab = "Z - Cigarettes Purchased", cex = 1.25, d =3, cex.lab = 1.5)
    scatter3D(z = jitter(subset(boldLines, y == min(y))$z), x = log10((subset(boldLines, y == min(y))$x)), 
              y = rep(-1.655707, 935), colkey = FALSE, add = TRUE, pch = 16, cex = .65,
              col = rgb(0,0,0,.1))
    scatter3D(z = jitter(subset(boldLines, x == min(x))$z), x = rep(-2.069634, 765),
              y = log10(subset(boldLines, x == min(x))$y), colkey = FALSE, add = TRUE, pch = 15, cex = .65,
              col = rgb(0,0,0,.1))
    scatter3D(z = z, x = log10(x), y = log10(y), colkey = FALSE, add = TRUE, zlim = c(0, 40), xlim = c(-2.25, 1.25), ylim = c(-1.5, 5),
              col = NA, pch = NA, phi = 18, theta = 135, ticktype = "detailed", xlab = "X - log10 Price",
              zlim = c(0, 25),
              surf = list(z = matrix(
                  subset(predict(nlmeMixedRachlinLogAlt, newdata = mixedGrid, level = 0:1), 
                         id == "A113")$predict.fixed, 
                  ncol = grains),
                  x = log10(xlong), y = log10(ylong),
                  facets = TRUE, col = grey(.9),
                  alpha = .05, border = grey(.6))
    )
    mtext(expression(frac(italic(Q)[0]~"*"~ 10 ^ italic(k)  (-italic(alpha)*italic(Q)[0]*Delay-1), 1 +italic(b) * Price^italic(s))), line = -1, cex = 1.25)
    mtext(bquote(italic(R)^2 == .(round(rSquarer(nlmeMixedRachlinLogAlt), 4))), side = 1, line = .35)
    mtext(paste0("MAE = ", round(MAE(nlmeMixedRachlinLogAlt), 4)), side = 1, line = 1.35)
    mtext(paste0("AICc = ", round(AICc(nlmeMixedRachlinLogAlt), 2)), side = 1, line = 2.35)
}
)

with(cleanLong[, .(z = median(z, na.rm = TRUE)), by = .(x,y)], {
    boldLines <- rbind(subset(cleanLong, y == min(y)), subset(cleanLong, x == min(x)))
    scatter3D(z = z, x = log10(x), y = log10(y), colkey = FALSE, zlim = c(0, 50), xlim = c(-2.069634, 1.25), ylim = c(-1.655707, 5.25),
              col = NA, pch = NA, phi = 18, theta = 135, ticktype = "detailed", xlab = "X - log10 Price",
              ylab = "Y - log10 Delay", zlab = "Z - Cigarettes Purchased", cex = 1.25, d =3, cex.lab = 1.5)
    scatter3D(z = jitter(subset(boldLines, y == min(y))$z), x = log10((subset(boldLines, y == min(y))$x)), 
              y = rep(-1.655707, 935), colkey = FALSE, add = TRUE, pch = 16, cex = .65,
              col = rgb(0,0,0,.1))
    scatter3D(z = jitter(subset(boldLines, x == min(x))$z), x = rep(-2.069634, 765),
              y = log10(subset(boldLines, x == min(x))$y), colkey = FALSE, add = TRUE, pch = 15, cex = .65,
              col = rgb(0,0,0,.1))
    scatter3D(z = z, x = log10(x), y = log10(y), colkey = FALSE, add = TRUE, zlim = c(0, 40), xlim = c(-2.25, 1.25), ylim = c(-1.5, 5),
              col = NA, pch = NA, phi = 18, theta = 135, ticktype = "detailed", xlab = "X - log10 Price",
              zlim = c(0, 25),

              surf = list(z = matrix(
                  subset(predict(nlmeMixedRachlinLog, newdata = mixedGrid, level = 0:1), 
                         id == "A113")$predict.fixed, 
                  ncol = grains),
                  x = log10(xlong), y = log10(ylong),
                  facets = TRUE, col = grey(.9),
                  alpha = .05, border = grey(.6))
    )
    mtext(expression(frac(italic(Q)[0]~"*"~ 10 ^ italic(k)  (-italic(alpha)*italic(Q)[0]*Price-1), 1 +italic(b) * Delay^italic(s))), line = -1, cex = 1.25)
    mtext(bquote(italic(R)^2 == .(round(rSquarer(nlmeMixedRachlinLog), 4))), side = 1, line = .35)
    mtext(paste0("MAE = ", round(MAE(nlmeMixedRachlinLog), 4)), side = 1, line = 1.35)
    mtext(paste0("AICc = ", round(AICc(nlmeMixedRachlinLog), 2)), side = 1, line = 2.35)
}
)
 


with(cleanLong[, .(z = median(z, na.rm = TRUE)), by = .(x,y)], {
    boldLines <- rbind(subset(cleanLong, y == min(y)), subset(cleanLong, x == min(x)))
    scatter3D(z = z, x = log10(x), y = log10(y), colkey = FALSE, zlim = c(0, 50), xlim = c(-2.069634, 1.25), ylim = c(-1.655707, 5.25),
              col = NA, pch = NA, phi = 18, theta = 135, ticktype = "detailed", xlab = "X - log10 Price",
              ylab = "Y - log10 Delay", zlab = "Z - Cigarettes Purchased", cex = 1.25, d =3, cex.lab = 1.5)
    scatter3D(z = jitter(subset(boldLines, y == min(y))$z), x = log10((subset(boldLines, y == min(y))$x)), 
              y = rep(-1.655707, 935), colkey = FALSE, add = TRUE, pch = 16, cex = .65,
              col = rgb(0,0,0,.1))
    scatter3D(z = jitter(subset(boldLines, x == min(x))$z), x = rep(-2.069634, 765),
              y = log10(subset(boldLines, x == min(x))$y), colkey = FALSE, add = TRUE, pch = 15, cex = .65,
              col = rgb(0,0,0,.1))
    scatter3D(z = z, x = log10(x), y = log10(y), colkey = FALSE, add = TRUE, zlim = c(0, 40), xlim = c(-2.25, 1.25), ylim = c(-1.5, 5),
              col = NA, pch = NA, phi = 18, theta = 135, ticktype = "detailed", xlab = "X - log10 Price",
              zlim = c(0, 25),

              surf = list(z = matrix(
                  subset(predict(nlmeExpScales.5Log, newdata = mixedGrid, level = 0:1), 
                         id == "A113")$predict.fixed, 
                  ncol = grains),
                  x = log10(xlong), y = log10(ylong),
                  facets = TRUE, col = grey(.9),
                  alpha = .05, border = grey(.6))
    )
    mtext(expression(italic(Q)[0] ~"*"~ 10 ^ italic(k)  (-italic(alpha)*italic(Q)[0]*Price^italic(t)-1)~"*"~10 ^ italic(k)  (-italic(beta)*italic(Q)[0]*Delay^italic(s)-1) ), line = -1, cex = 1.25)
    mtext(bquote(italic(R)^2 == .(round(rSquarer(nlmeExpScales.5Log), 4))), side = 1, line = .35)
    mtext(paste0("MAE = ", round(MAE(nlmeExpScales.5Log), 4)), side = 1, line = 1.35)
    mtext(paste0("AICc = ", round(AICc(nlmeExpScales.5Log), 2)), side = 1, line = 2.35)
}
)
    
with(cleanLong[, .(z = median(z, na.rm = TRUE)), by = .(x,y)], {
    boldLines <- rbind(subset(cleanLong, y == min(y)), subset(cleanLong, x == min(x)))
    scatter3D(z = z, x = log10(x), y = log10(y), colkey = FALSE, zlim = c(0, 50), xlim = c(-2.069634, 1.25), ylim = c(-1.655707, 5.25),
              col = NA, pch = NA, phi = 18, theta = 135, ticktype = "detailed", xlab = "X - log10 Price",
              ylab = "Y - log10 Delay", zlab = "Z - Cigarettes Purchased", cex = 1.25, d =3, cex.lab = 1.5)
    scatter3D(z = jitter(subset(boldLines, y == min(y))$z), x = log10((subset(boldLines, y == min(y))$x)), 
              y = rep(-1.655707, 935), colkey = FALSE, add = TRUE, pch = 16, cex = .65,
              col = rgb(0,0,0,.1))
    scatter3D(z = jitter(subset(boldLines, x == min(x))$z), x = rep(-2.069634, 765),
              y = log10(subset(boldLines, x == min(x))$y), colkey = FALSE, add = TRUE, pch = 15, cex = .65,
              col = rgb(0,0,0,.1))
    scatter3D(z = z, x = log10(x), y = log10(y), colkey = FALSE, add = TRUE, zlim = c(0, 40), xlim = c(-2.25, 1.25), ylim = c(-1.5, 5),
              col = NA, pch = NA, phi = 18, theta = 135, ticktype = "detailed", xlab = "X - log10 Price",
              zlim = c(0, 25),

              surf = list(z = matrix(
                  subset(predict(nlmeRachlinOnlyLog, newdata = mixedGrid, level = 0:1), 
                         id == "A113")$predict.fixed, 
                  ncol = grains),
                  x = log10(xlong), y = log10(ylong),
                  facets = TRUE, col = grey(.9),
                  alpha = .05, border = grey(.6))
    )
    mtext(expression(frac(italic(Q)[0], (1 +italic(c) * Price^italic(t))(1 +italic(b) * Delay^italic(s)))), line = -1, cex = 1.25)
    mtext(bquote(italic(R)^2 == .(round(rSquarer(nlmeRachlinOnlyLog), 4))), side = 1, line = .35)
    mtext(paste0("MAE = ", round(MAE(nlmeRachlinOnlyLog), 4)), side = 1, line = 1.35)
    mtext(paste0("AICc = ", round(AICc(nlmeRachlinOnlyLog), 2)), side = 1, line = 2.35)
}
)

dev.off()


# Individual Publication Pictures ----
svg("3dindividuals.svg", width = 6, height = 9.5)

par(mfrow = c(3,2), mar = c(2,2,3,2))

partic <- "A113"
with(cleanLong[id == partic, ], {
    scatter3D(z = z, x = log10(x), y = log10(y), colkey = FALSE, xlim = c(-2.069634, 1.25), ylim = c(-1.655707, 5.25), zlim = c(0, max(z) + 5),
              col = "black", pch = 21, phi = 18, theta = 135, ticktype = "detailed", xlab = "X - log10 Price",
              ylab = "Y - log10 Delay", zlab = "Z - Cigarettes Purchased", cex = 1, d =3, 
              # xlim = c(-1.75, 1.75), ylim = c(-.5, 6),
              # zlim = c(0, max(z) +1),
              panel.first = points3D(x = log10(x),
                                     y = log10(y),
                                     cex = .25, pch = 21,
                                     z = rep(0, length(x)), add = TRUE, colkey = FALSE, col = "black")
              ,
              surf = list(z = matrix(
                  subset(predict(nlmeRachlinOnlyLog, newdata = mixedGrid, level = 0:1), 
                         id == partic)$predict.id, 
                  ncol = grains),
                  x = log10(xlong), y = log10(ylong),
                  facets = TRUE, col = grey(.9), 
                  fit = (subset(predict(nlmeRachlinOnlyLog, level = 0:1), id == partic)$predict.id),
                  alpha = .15, border = grey(.7)))
    mtext(paste("ID", partic), line = .5, font = 2)
    mtext(expression(Rachlin[Price]~"*"~Rachlin[Delay]), line = -1.25)
}
)

partic <- "A204"
with(cleanLong[id == partic, ], {
    scatter3D(z = z, x = log10(x), y = log10(y), colkey = FALSE, xlim = c(-2.069634, 1.25), ylim = c(-1.655707, 5.25), zlim = c(0, max(z) + 5),
              col = "black", pch = 21, phi = 18, theta = 135, ticktype = "detailed", xlab = "X - log10 Price",
              ylab = "Y - log10 Delay", zlab = "Z - Cigarettes Purchased", cex = 1, d =3, 
              # xlim = c(-1.75, 1.75), ylim = c(-.5, 6),
              # zlim = c(0, max(z) +1),
              panel.first = points3D(x = log10(x),
                                     y = log10(y),
                                     cex = .25, pch = 21,
                                     z = rep(0, length(x)), add = TRUE, colkey = FALSE, col = "black")
              ,
              surf = list(z = matrix(
                  subset(predict(nlmeRachlinOnlyLog, newdata = mixedGrid, level = 0:1), 
                         id == partic)$predict.id, 
                  ncol = grains),
                  x = log10(xlong), y = log10(ylong),
                  facets = TRUE, col = grey(.9), 
                  fit = (subset(predict(nlmeRachlinOnlyLog, level = 0:1), id == partic)$predict.id),
                  alpha = .15, border = grey(.7)))
    mtext(paste("ID", partic), line = .5, font = 2)
    mtext(expression(Rachlin[Price]~"*"~Rachlin[Delay]), line = -1.25)
}
)

partic <- "A261"
with(cleanLong[id == partic, ], {
    scatter3D(z = z, x = log10(x), y = log10(y), colkey = FALSE, xlim = c(-2.069634, 1.25), ylim = c(-1.655707, 5.25), zlim = c(0, max(z) + 5),
              col = "black", pch = 21, phi = 18, theta = 135, ticktype = "detailed", xlab = "X - log10 Price",
              ylab = "Y - log10 Delay", zlab = "Z - Cigarettes Purchased", cex = 1, d =3, 
              # xlim = c(-1.75, 1.75), ylim = c(-.5, 6),
              # zlim = c(0, max(z) +1),
              panel.first = points3D(x = log10(x),
                                     y = log10(y),
                                     cex = .25, pch = 21,
                                     z = rep(0, length(x)), add = TRUE, colkey = FALSE, col = "black")
              ,
              surf = list(z = matrix(
                  subset(predict(nlmeRachlinOnlyLog, newdata = mixedGrid, level = 0:1), 
                         id == partic)$predict.id, 
                  ncol = grains),
                  x = log10(xlong), y = log10(ylong),
                  facets = TRUE, col = grey(.9), 
                  fit = (subset(predict(nlmeRachlinOnlyLog, level = 0:1), id == partic)$predict.id),
                  alpha = .15, border = grey(.7)))
    mtext(paste("ID", partic), line = .5, font = 2)
    mtext(expression(Rachlin[Price]~"*"~Rachlin[Delay]), line = -1.25)
}
)

partic <- "A337"
with(cleanLong[id == partic, ], {
    scatter3D(z = z, x = log10(x), y = log10(y), colkey = FALSE, xlim = c(-2.069634, 1.25), ylim = c(-1.655707, 5.25), zlim = c(0, max(z) + 5),
              col = "black", pch = 21, phi = 18, theta = 135, ticktype = "detailed", xlab = "X - log10 Price",
              ylab = "Y - log10 Delay", zlab = "Z - Cigarettes Purchased", cex = 1, d =3, 
              # xlim = c(-1.75, 1.75), ylim = c(-.5, 6),
              # zlim = c(0, max(z) +1),
              panel.first = points3D(x = log10(x),
                                     y = log10(y),
                                     cex = .25, pch = 21,
                                     z = rep(0, length(x)), add = TRUE, colkey = FALSE, col = "black")
              ,
              surf = list(z = matrix(
                  subset(predict(nlmeRachlinOnlyLog, newdata = mixedGrid, level = 0:1), 
                         id == partic)$predict.id, 
                  ncol = grains),
                  x = log10(xlong), y = log10(ylong),
                  facets = TRUE, col = grey(.9), 
                  fit = (subset(predict(nlmeRachlinOnlyLog, level = 0:1), id == partic)$predict.id),
                  alpha = .15, border = grey(.7)))
    mtext(paste("ID", partic), line = .5, font = 2)
    mtext(expression(Rachlin[Price]~"*"~Rachlin[Delay]), line = -1.25)
}
)

partic <- "A411"
with(cleanLong[id == partic, ], {
    scatter3D(z = z, x = log10(x), y = log10(y), colkey = FALSE, xlim = c(-2.069634, 1.25), ylim = c(-1.655707, 5.25), zlim = c(0, max(z) + 5),
              col = "black", pch = 21, phi = 18, theta = 135, ticktype = "detailed", xlab = "X - log10 Price",
              ylab = "Y - log10 Delay", zlab = "Z - Cigarettes Purchased", cex = 1, d =3, 
              # xlim = c(-1.75, 1.75), ylim = c(-.5, 6),
              # zlim = c(0, max(z) +1),
              panel.first = points3D(x = log10(x),
                                     y = log10(y),
                                     cex = .25, pch = 21,
                                     z = rep(0, length(x)), add = TRUE, colkey = FALSE, col = "black")
              ,
              surf = list(z = matrix(
                  subset(predict(nlmeRachlinOnlyLog, newdata = mixedGrid, level = 0:1), 
                         id == partic)$predict.id, 
                  ncol = grains),
                  x = log10(xlong), y = log10(ylong),
                  facets = TRUE, col = grey(.9), 
                  fit = (subset(predict(nlmeRachlinOnlyLog, level = 0:1), id == partic)$predict.id),
                  alpha = .15, border = grey(.7)))
    mtext(paste("ID", partic), line = .5, font = 2)
    mtext(expression(Rachlin[Price]~"*"~Rachlin[Delay]), line = -1.25)
}
)
partic <- "A413"
with(cleanLong[id == partic, ], {
    scatter3D(z = z, x = log10(x), y = log10(y), colkey = FALSE, xlim = c(-2.069634, 1.25), ylim = c(-1.655707, 5.25), zlim = c(0, max(z) + 5),
              col = "black", pch = 21, phi = 18, theta = 135, ticktype = "detailed", xlab = "X - log10 Price",
              ylab = "Y - log10 Delay", zlab = "Z - Cigarettes Purchased", cex = 1, d =3, 
              # xlim = c(-1.75, 1.75), ylim = c(-.5, 6),
              # zlim = c(0, max(z) +1),
              panel.first = points3D(x = log10(x),
                                     y = log10(y),
                                     cex = .25, pch = 21,
                                     z = rep(0, length(x)), add = TRUE, colkey = FALSE, col = "black")
              ,
              surf = list(z = matrix(
                  subset(predict(nlmeRachlinOnlyLog, newdata = mixedGrid, level = 0:1), 
                         id == partic)$predict.id, 
                  ncol = grains),
                  x = log10(xlong), y = log10(ylong),
                  facets = TRUE, col = grey(.9), 
                  fit = (subset(predict(nlmeRachlinOnlyLog, level = 0:1), id == partic)$predict.id),
                  alpha = .15, border = grey(.7)))
    mtext(paste("ID", partic), line = .5, font = 2)
    mtext(expression(Rachlin[Price]~"*"~Rachlin[Delay]), line = -1.25)
}
)


dev.off()


# Supplemental Files ----


yPoints <- 1.1^seq(-60, 114, len = 100)
xPoints <- 1.1^seq(-60, 25, len = 100)
# note that the equations use (k +.5) and thus are actually calculated correctly.
pricePoints <- data.frame(id = rep(unique(cleanLong$id), each = 100), x = xPoints, k = 1.154949)
delayPoints <- data.frame(id = rep(unique(cleanLong$id), each = 100), y = yPoints, k = 1.154949)

allIndividualPredictionsPrice <- rbind(
    data.frame(predict(rachlinDemandMLM, newdata = pricePoints, level = 0:1), Model  = 'RachlinPrice'),
    data.frame(predict(exp.5DemandMLM, newdata = pricePoints, level = 0:1), Model  = 'EXPDPrice'))

allIndividualPredictionsDelay <- rbind(
    data.frame(predict(rachlinDelayMLM, newdata = delayPoints, level = 0:1), Model  = 'RachlinDelay'),
    data.frame(predict(exp.5DelayMLM, newdata = delayPoints, level = 0:1), Model  = 'EXPDDelay'))


allIndividualPredictionsPrice$xs <- xPoints
allIndividualPredictionsDelay$ys <- yPoints


pdf("2dsupplemental.pdf", width = 16, height = 16, onefile = TRUE)
subset(cleanLong, y == min(y)) |> 
    # ggplot(aes(x = ifelse(x == 0, .01, x), y = y)) +
    ggplot(aes(x = x, y = z)) +
    geom_point(pch = 21, size = 2) +
    geom_line(data = allIndividualPredictionsPrice, 
              aes(x = xs, y = predict.id, linetype = Model)) +
    scale_linetype_manual(values = c(5,1)) +
    # scale_linetype_manual(values = c(1)) +
    # scale_x_log10()
    facet_wrap(facets = ~id) +
    theme_classic() +
    scale_x_log10(breaks = (unique(cleanLong$x)), labels = c("$0.01", "$0.03", "$0.06", "$0.13", "$0.25", "$0.50", "$1.00", "$2.00", "$4.00", "  $8.00"))+
    
    theme(legend.position = "top", plot.title = element_text(hjust = .5), aspect.ratio = .75, axis.text = element_text(colour = "black"),
          axis.text.x = element_text(angle = 50, vjust = 1, hjust=1)) +
    guides(linetype = guide_legend(nrow = 1)) +
    coord_cartesian(ylim = c(0,60)) +
    # coord_fixed() +
    labs(title = "2D Price",
         y = "Cigarettes Purchased", x = "Price")
subset(cleanLong, x == min(x)) |> 
    # ggplot(aes(x = ifelse(x == 0, .01, x), y = y)) +
    ggplot(aes(x = y, y = z)) +
    geom_point(pch = 22, size = 2) +
    geom_line(data = allIndividualPredictionsDelay, 
              aes(x = ys, y = predict.id, linetype = Model)) +
    scale_linetype_manual(values = c(5,1)) +
    # scale_linetype_manual(values = c(1)) +
    # scale_x_log10()
    facet_wrap(facets = ~id) +
    theme_classic() +
    scale_x_log10(breaks = (unique(cleanLong$y)), labels = c("5 Min", "6 Hour", "1 Day", "1 Week", "1 Month", "3 Month", "1 Year", "5 Year"))+
    # scale_x_log10() +
    
    theme(legend.position = "top", plot.title = element_text(hjust = .5), aspect.ratio = .75, axis.text = element_text(colour = "black"),
          axis.text.x = element_text(angle = 50, vjust = 1, hjust=1)) +        guides(linetype = guide_legend(nrow = 1)) +
    coord_cartesian(ylim = c(0,60)) +
    # coord_fixed() +
    labs(title = "2D Delay",
         y = "Cigarettes Purchased", x = "Delay")

dev.off()

pdf("3dsupplementalfigures.pdf", width = 15, height = 14, onefile = TRUE)

for(a in 1:length(unique(predict(nlmeMazurAddLog, level = 0:1)$id))){
    par(mfrow = c(5,4), mar = c(2.5,2,1,2))
    partic <- unique(predict(nlmeMazurAddLog, level = 0:1)$id)[a]
    with(cleanLong[id == partic, ],
         scatter3D(z = z, x = log10(x), y = log10(y), colkey = FALSE, xlim = c(-2.069634, 1.25), ylim = c(-1.655707, 5.25), zlim = c(0, max(z) + 5),
                   col = "black", pch = 21, phi = 18, theta = 135, ticktype = "detailed", xlab = "X - log10 Price",
                   ylab = "Y - log10 Delay", zlab = "Z - Cigarettes Purchased", cex = 1, d =3, 
                   # xlim = c(-1.75, 1.75), ylim = c(-.5, 6),
                   # zlim = c(0, max(z) +1),
                   panel.first = points3D(x = log10(x),
                                          y = log10(y),
                                          cex = .25, pch = 21,
                                          z = rep(0, length(x)), add = TRUE, colkey = FALSE, col = "black")
                   ,
                   surf = list(z = matrix(
                       subset(predict(nlmeMazurAddLog, newdata = mixedGrid, level = 0:1), 
                              id == partic)$predict.id, 
                       ncol = grains),
                       x = log10(xlong), y = log10(ylong),
                       facets = TRUE, col = grey(.9), 
                       fit = (subset(predict(nlmeMazurAddLog, level = 0:1), id == partic)$predict.id),
                       alpha = .15, border = grey(.7)),
                   main = paste0("ID ", partic, ": EXPDPrice + MazurDelay"))
         
    )
    with(cleanLong[id == partic, ],
         scatter3D(z = z, x = log10(x), y = log10(y), colkey = FALSE, xlim = c(-2.069634, 1.25), ylim = c(-1.655707, 5.25), zlim = c(0, max(z) + 5),
                   col = "black", pch = 21, phi = 18, theta = 135, ticktype = "detailed", xlab = "X - log10 Price",
                   ylab = "Y - log10 Delay", zlab = "Z - Cigarettes Purchased", cex = 1, d =3, 
                   # xlim = c(-1.75, 1.75), ylim = c(-.5, 6),
                   # zlim = c(0, max(z) +1),
                   panel.first = points3D(x = log10(x),
                                          y = log10(y),
                                          cex = .25, pch = 21,
                                          z = rep(0, length(x)), add = TRUE, colkey = FALSE, col = "black")
                   ,
                   surf = list(z = matrix(
                       subset(predict(nlmeMGAddLog, newdata = mixedGrid, level = 0:1), 
                              id == partic)$predict.id, 
                       ncol = grains),
                       x = log10(xlong), y = log10(ylong),
                       facets = TRUE, col = grey(.9), 
                       fit = (subset(predict(nlmeMGAddLog, level = 0:1), id == partic)$predict.id),
                       alpha = .15, border = grey(.7)),
                   main = paste0("ID ", partic, ": EXPDPrice + MGDelay"))
         
    )
    with(cleanLong[id == partic, ],
         scatter3D(z = z, x = log10(x), y = log10(y), colkey = FALSE, xlim = c(-2.069634, 1.25), ylim = c(-1.655707, 5.25), zlim = c(0, max(z) + 5),
                   col = "black", pch = 21, phi = 18, theta = 135, ticktype = "detailed", xlab = "X - log10 Price",
                   ylab = "Y - log10 Delay", zlab = "Z - Cigarettes Purchased", cex = 1, d =3, 
                   # xlim = c(-1.75, 1.75), ylim = c(-.5, 6),
                   # zlim = c(0, max(z) +1),
                   panel.first = points3D(x = log10(x),
                                          y = log10(y),
                                          cex = .25, pch = 21,
                                          z = rep(0, length(x)), add = TRUE, colkey = FALSE, col = "black")
                   ,
                   surf = list(z = matrix(
                       subset(predict(nlmeRachAddLog, newdata = mixedGrid, level = 0:1), 
                              id == partic)$predict.id, 
                       ncol = grains),
                       x = log10(xlong), y = log10(ylong),
                       facets = TRUE, col = grey(.9), 
                       fit = (subset(predict(nlmeRachAddLog, level = 0:1), id == partic)$predict.id),
                       alpha = .15, border = grey(.7)),
                   main = paste0("ID ", partic, ": EXPDPrice + RachlinDelay"))
         
    )
    with(cleanLong[id == partic, ],
         scatter3D(z = z, x = log10(x), y = log10(y), colkey = FALSE, xlim = c(-2.069634, 1.25), ylim = c(-1.655707, 5.25), zlim = c(0, max(z) + 5),
                   col = "black", pch = 21, phi = 18, theta = 135, ticktype = "detailed", xlab = "X - log10 Price",
                   ylab = "Y - log10 Delay", zlab = "Z - Cigarettes Purchased", cex = 1, d =3, 
                   # xlim = c(-1.75, 1.75), ylim = c(-.5, 6),
                   # zlim = c(0, max(z) +1),
                   panel.first = points3D(x = log10(x),
                                          y = log10(y),
                                          cex = .25, pch = 21,
                                          z = rep(0, length(x)), add = TRUE, colkey = FALSE, col = "black")
                   ,
                   surf = list(z = matrix(
                       subset(predict(nlmeMixedMazurLog, newdata = mixedGrid, level = 0:1), 
                              id == partic)$predict.id, 
                       ncol = grains),
                       x = log10(xlong), y = log10(ylong),
                       facets = TRUE, col = grey(.9), 
                       fit = (subset(predict(nlmeMixedMazurLog, level = 0:1), id == partic)$predict.id),
                       alpha = .15, border = grey(.7)),
                   main = paste0("ID ", partic, ": EXPDPrice * MazurDelay"))
    )
    
    with(cleanLong[id == partic, ],
         scatter3D(z = z, x = log10(x), y = log10(y), colkey = FALSE, xlim = c(-2.069634, 1.25), ylim = c(-1.655707, 5.25), zlim = c(0, max(z) + 5),
                   col = "black", pch = 21, phi = 18, theta = 135, ticktype = "detailed", xlab = "X - log10 Price",
                   ylab = "Y - log10 Delay", zlab = "Z - Cigarettes Purchased", cex = 1, d =3, 
                   # xlim = c(-1.75, 1.75), ylim = c(-.5, 6),
                   # zlim = c(0, max(z) +1),
                   panel.first = points3D(x = log10(x),
                                          y = log10(y),
                                          cex = .25, pch = 21,
                                          z = rep(0, length(x)), add = TRUE, colkey = FALSE, col = "black")
                   ,
                   surf = list(z = matrix(
                       subset(predict(nlmeMixedMGLog, newdata = mixedGrid, level = 0:1), 
                              id == partic)$predict.id, 
                       ncol = grains),
                       x = log10(xlong), y = log10(ylong),
                       facets = TRUE, col = grey(.9), 
                       fit = (subset(predict(nlmeMixedMGLog, level = 0:1), id == partic)$predict.id),
                       alpha = .15, border = grey(.7)),
                   main = paste0("ID ", partic, ": EXPDPrice * MGDelay"))
    )
    
    with(cleanLong[id == partic, ],
         scatter3D(z = z, x = log10(x), y = log10(y), colkey = FALSE, xlim = c(-2.069634, 1.25), ylim = c(-1.655707, 5.25), zlim = c(0, max(z) + 5),
                   col = "black", pch = 21, phi = 18, theta = 135, ticktype = "detailed", xlab = "X - log10 Price",
                   ylab = "Y - log10 Delay", zlab = "Z - Cigarettes Purchased", cex = 1, d =3, 
                   # xlim = c(-1.75, 1.75), ylim = c(-.5, 6),
                   # zlim = c(0, max(z) +1),
                   panel.first = points3D(x = log10(x),
                                          y = log10(y),
                                          cex = .25, pch = 21,
                                          z = rep(0, length(x)), add = TRUE, colkey = FALSE, col = "black")
                   ,
                   surf = list(z = matrix(
                       subset(predict(nlmeMixed.5RachlinLog, newdata = mixedGrid, level = 0:1), 
                              id == partic)$predict.id, 
                       ncol = grains),
                       x = log10(xlong), y = log10(ylong),
                       facets = TRUE, col = grey(.9), 
                       fit = (subset(predict(nlmeMixed.5RachlinLog, level = 0:1), id == partic)$predict.id),
                       alpha = .15, border = grey(.7)),
                   main = paste0("ID ", partic, ": EXDPPrice * RachlinDelay"))
         
    )
    
    with(cleanLong[id == partic, ],
         scatter3D(z = z, x = log10(x), y = log10(y), colkey = FALSE, xlim = c(-2.069634, 1.25), ylim = c(-1.655707, 5.25), zlim = c(0, max(z) + 5),
                   col = "black", pch = 21, phi = 18, theta = 135, ticktype = "detailed", xlab = "X - log10 Price",
                   ylab = "Y - log10 Delay", zlab = "Z - Cigarettes Purchased", cex = 1, d =3, 
                   # xlim = c(-1.75, 1.75), ylim = c(-.5, 6),
                   # zlim = c(0, max(z) +1),
                   panel.first = points3D(x = log10(x),
                                          y = log10(y),
                                          cex = .25, pch = 21,
                                          z = rep(0, length(x)), add = TRUE, colkey = FALSE, col = "black")
                   ,
                   surf = list(z = matrix(
                       subset(predict(nlmeMazurAddLogAlt, newdata = mixedGrid, level = 0:1), 
                              id == partic)$predict.id, 
                       ncol = grains),
                       x = log10(xlong), y = log10(ylong),
                       facets = TRUE, col = grey(.9), 
                       fit = (subset(predict(nlmeMazurAddLogAlt, level = 0:1), id == partic)$predict.id),
                       alpha = .15, border = grey(.7)),
                   main = paste0("ID ", partic, ": MazurPrice + EXPDDelay"))
         
    )
    with(cleanLong[id == partic, ],
         scatter3D(z = z, x = log10(x), y = log10(y), colkey = FALSE, xlim = c(-2.069634, 1.25), ylim = c(-1.655707, 5.25), zlim = c(0, max(z) + 5),
                   col = "black", pch = 21, phi = 18, theta = 135, ticktype = "detailed", xlab = "X - log10 Price",
                   ylab = "Y - log10 Delay", zlab = "Z - Cigarettes Purchased", cex = 1, d =3, 
                   # xlim = c(-1.75, 1.75), ylim = c(-.5, 6),
                   # zlim = c(0, max(z) +1),
                   panel.first = points3D(x = log10(x),
                                          y = log10(y),
                                          cex = .25, pch = 21,
                                          z = rep(0, length(x)), add = TRUE, colkey = FALSE, col = "black")
                   ,
                   surf = list(z = matrix(
                       subset(predict(nlmeMGAddLogAlt, newdata = mixedGrid, level = 0:1), 
                              id == partic)$predict.id, 
                       ncol = grains),
                       x = log10(xlong), y = log10(ylong),
                       facets = TRUE, col = grey(.9), 
                       fit = (subset(predict(nlmeMGAddLogAlt, level = 0:1), id == partic)$predict.id),
                       alpha = .15, border = grey(.7)),
                   main = paste0("ID ", partic, ": MGPrice + EXPDelay"))
         
    )
    with(cleanLong[id == partic, ],
         scatter3D(z = z, x = log10(x), y = log10(y), colkey = FALSE, xlim = c(-2.069634, 1.25), ylim = c(-1.655707, 5.25), zlim = c(0, max(z) + 5),
                   col = "black", pch = 21, phi = 18, theta = 135, ticktype = "detailed", xlab = "X - log10 Price",
                   ylab = "Y - log10 Delay", zlab = "Z - Cigarettes Purchased", cex = 1, d =3, 
                   # xlim = c(-1.75, 1.75), ylim = c(-.5, 6),
                   # zlim = c(0, max(z) +1),
                   panel.first = points3D(x = log10(x),
                                          y = log10(y),
                                          cex = .25, pch = 21,
                                          z = rep(0, length(x)), add = TRUE, colkey = FALSE, col = "black")
                   ,
                   surf = list(z = matrix(
                       subset(predict(nlmeRachAddLogAlt, newdata = mixedGrid, level = 0:1), 
                              id == partic)$predict.id, 
                       ncol = grains),
                       x = log10(xlong), y = log10(ylong),
                       facets = TRUE, col = grey(.9), 
                       fit = (subset(predict(nlmeRachAddLogAlt, level = 0:1), id == partic)$predict.id),
                       alpha = .15, border = grey(.7)),
                   main = paste0("ID ", partic, ": RachlinPrice + EXPDDelay"))
         
    )
    with(cleanLong[id == partic, ],
         scatter3D(z = z, x = log10(x), y = log10(y), colkey = FALSE, xlim = c(-2.069634, 1.25), ylim = c(-1.655707, 5.25), zlim = c(0, max(z) + 5),
                   col = "black", pch = 21, phi = 18, theta = 135, ticktype = "detailed", xlab = "X - log10 Price",
                   ylab = "Y - log10 Delay", zlab = "Z - Cigarettes Purchased", cex = 1, d =3, 
                   # xlim = c(-1.75, 1.75), ylim = c(-.5, 6),
                   # zlim = c(0, max(z) +1),
                   panel.first = points3D(x = log10(x),
                                          y = log10(y),
                                          cex = .25, pch = 21,
                                          z = rep(0, length(x)), add = TRUE, colkey = FALSE, col = "black")
                   ,
                   surf = list(z = matrix(
                       subset(predict(nlmeMixedMazurLogAlt, newdata = mixedGrid, level = 0:1), 
                              id == partic)$predict.id, 
                       ncol = grains),
                       x = log10(xlong), y = log10(ylong),
                       facets = TRUE, col = grey(.9), 
                       fit = (subset(predict(nlmeMixedMazurLogAlt, level = 0:1), id == partic)$predict.id),
                       alpha = .15, border = grey(.7)),
                   main = paste0("ID ", partic, ": MazurPrice * EXPDDelay"))
    )
    
    with(cleanLong[id == partic, ],
         scatter3D(z = z, x = log10(x), y = log10(y), colkey = FALSE, xlim = c(-2.069634, 1.25), ylim = c(-1.655707, 5.25), zlim = c(0, max(z) + 5),
                   col = "black", pch = 21, phi = 18, theta = 135, ticktype = "detailed", xlab = "X - log10 Price",
                   ylab = "Y - log10 Delay", zlab = "Z - Cigarettes Purchased", cex = 1, d =3, 
                   # xlim = c(-1.75, 1.75), ylim = c(-.5, 6),
                   # zlim = c(0, max(z) +1),
                   panel.first = points3D(x = log10(x),
                                          y = log10(y),
                                          cex = .25, pch = 21,
                                          z = rep(0, length(x)), add = TRUE, colkey = FALSE, col = "black")
                   ,
                   surf = list(z = matrix(
                       subset(predict(nlmeMixedMGLogAlt, newdata = mixedGrid, level = 0:1), 
                              id == partic)$predict.id, 
                       ncol = grains),
                       x = log10(xlong), y = log10(ylong),
                       facets = TRUE, col = grey(.9), 
                       fit = (subset(predict(nlmeMixedMGLogAlt, level = 0:1), id == partic)$predict.id),
                       alpha = .15, border = grey(.7)),
                   main = paste0("ID ", partic, ": MGPrice * EXPDDelay"))
    )
    
    
    with(cleanLong[id == partic, ], 
         scatter3D(z = z, x = log10(x), y = log10(y), colkey = FALSE, xlim = c(-2.069634, 1.25), ylim = c(-1.655707, 5.25), zlim = c(0, max(z) + 5),
                   col = "black", pch = 21, phi = 18, theta = 135, ticktype = "detailed", xlab = "X - log10 Price",
                   ylab = "Y - log10 Delay", zlab = "Z - Cigarettes Purchased", cex = 1, d =3, 
                   # xlim = c(-1.75, 1.75), ylim = c(-.5, 6),
                   # zlim = c(0, max(z) +1),
                   panel.first = points3D(x = log10(x),
                                          y = log10(y),
                                          cex = .25, pch = 21,
                                          z = rep(0, length(x)), add = TRUE, colkey = FALSE, col = "black")
                   ,
                   surf = list(z = matrix(
                       subset(predict(nlmeMixedRachlinLogAlt, newdata = mixedGrid, level = 0:1), 
                              id == partic)$predict.id, 
                       ncol = grains),
                       x = log10(xlong), y = log10(ylong),
                       facets = TRUE, col = grey(.9), 
                       fit = (subset(predict(nlmeMixedRachlinLogAlt, level = 0:1), id == partic)$predict.id),
                       alpha = .15, border = grey(.7)),
                   main = paste0("ID ", partic, ": RachlinPrice * EXPDDelay"))
    )
    

    
    with(cleanLong[id == partic, ], {
        scatter3D(z = z, x = log10(x), y = log10(y), colkey = FALSE, xlim = c(-2.069634, 1.25), ylim = c(-1.655707, 5.25), zlim = c(0, max(z) + 5),
                  col = "black", pch = 21, phi = 18, theta = 135, ticktype = "detailed", xlab = "X - log10 Price",
                  ylab = "Y - log10 Delay", zlab = "Z - Cigarettes Purchased", cex = 1, d =3, 
                  # xlim = c(-1.75, 1.75), ylim = c(-.5, 6),
                  # zlim = c(0, max(z) +1),
                  panel.first = points3D(x = log10(x),
                                         y = log10(y),
                                         cex = .25, pch = 21,
                                         z = rep(0, length(x)), add = TRUE, colkey = FALSE, col = "black")
                  ,
                  surf = list(z = matrix(
                      subset(predict(nlmeMazurOnlyLog, newdata = mixedGrid, level = 0:1), 
                             id == partic)$predict.id, 
                      ncol = grains),
                      x = log10(xlong), y = log10(ylong),
                      facets = TRUE, col = grey(.9), 
                      fit = (subset(predict(nlmeMazurOnlyLog, level = 0:1), id == partic)$predict.id),
                      alpha = .15, border = grey(.7)),
                  main = paste0("ID ", partic, ": MazurPrice * MazurDelay"))
    }
    )
    with(cleanLong[id == partic, ], {
        scatter3D(z = z, x = log10(x), y = log10(y), colkey = FALSE, xlim = c(-2.069634, 1.25), ylim = c(-1.655707, 5.25), zlim = c(0, max(z) + 5),
                  col = "black", pch = 21, phi = 18, theta = 135, ticktype = "detailed", xlab = "X - log10 Price",
                  ylab = "Y - log10 Delay", zlab = "Z - Cigarettes Purchased", cex = 1, d =3, 
                  # xlim = c(-1.75, 1.75), ylim = c(-.5, 6),
                  # zlim = c(0, max(z) +1),
                  panel.first = points3D(x = log10(x),
                                         y = log10(y),
                                         cex = .25, pch = 21,
                                         z = rep(0, length(x)), add = TRUE, colkey = FALSE, col = "black")
                  ,
                  surf = list(z = matrix(
                      subset(predict(nlmeMGOnlyLog, newdata = mixedGrid, level = 0:1), 
                             id == partic)$predict.id, 
                      ncol = grains),
                      x = log10(xlong), y = log10(ylong),
                      facets = TRUE, col = grey(.9),
                      fit = (subset(predict(nlmeMGOnlyLog, level = 0:1), id == partic)$predict.id),
                      alpha = .15, border = grey(.7)),
                  main = paste0("ID ", partic, ": MGPrice * MGDelay"))
    }
    )

    with(cleanLong[id == partic, ], {
        scatter3D(z = z, x = log10(x), y = log10(y), colkey = FALSE, xlim = c(-2.069634, 1.25), ylim = c(-1.655707, 5.25), zlim = c(0, max(z) + 5),
                  col = "black", pch = 21, phi = 18, theta = 135, ticktype = "detailed", xlab = "X - log10 Price",
                  ylab = "Y - log10 Delay", zlab = "Z - Cigarettes Purchased", cex = 1, d =3, 
                  # xlim = c(-1.75, 1.75), ylim = c(-.5, 6),
                  # zlim = c(0, max(z) +1),
                  panel.first = points3D(x = log10(x),
                                         y = log10(y),
                                         cex = .25, pch = 21,
                                         z = rep(0, length(x)), add = TRUE, colkey = FALSE, col = "black")
                  ,
                  surf = list(z = matrix(
                      subset(predict(nlmeRachlinOnlyLog, newdata = mixedGrid, level = 0:1), 
                             id == partic)$predict.id, 
                      ncol = grains),
                      x = log10(xlong), y = log10(ylong),
                      facets = TRUE, col = grey(.9), 
                      fit = (subset(predict(nlmeRachlinOnlyLog, level = 0:1), id == partic)$predict.id),
                      alpha = .15, border = grey(.7)),
                  main = paste0("ID ", partic, ": RachlinPrice * RachlinDelay"))
    }
    )
    with(cleanLong[id == partic, ], {
        scatter3D(z = z, x = log10(x), y = log10(y), colkey = FALSE, xlim = c(-2.069634, 1.25), ylim = c(-1.655707, 5.25), zlim = c(0, max(z) + 5),
                  col = "black", pch = 21, phi = 18, theta = 135, ticktype = "detailed", xlab = "X - log10 Price",
                  ylab = "Y - log10 Delay", zlab = "Z - Cigarettes Purchased", cex = 1, d =3, 
                  # xlim = c(-1.75, 1.75), ylim = c(-.5, 6),
                  # zlim = c(0, max(z) +1),
                  panel.first = points3D(x = log10(x),
                                         y = log10(y),
                                         cex = .25, pch = 21,
                                         z = rep(0, length(x)), add = TRUE, colkey = FALSE, col = "black")
                  ,
                  surf = list(z = matrix(
                      subset(predict(nlmeExpOnly.5Log, newdata = mixedGrid, level = 0:1), 
                             id == partic)$predict.id, 
                      ncol = grains),
                      x = log10(xlong), y = log10(ylong),
                      facets = TRUE, col = grey(.9), 
                      fit = (subset(predict(nlmeExpOnly.5Log, level = 0:1), id == partic)$predict.id),
                      alpha = .15, border = grey(.7)),
                  main = paste0("ID ", partic, ": EXPDPrice * EXPDDelay"))
    }
    )
    with(cleanLong[id == partic, ], {
        scatter3D(z = z, x = log10(x), y = log10(y), colkey = FALSE, xlim = c(-2.069634, 1.25), ylim = c(-1.655707, 5.25), zlim = c(0, max(z) + 5),
                  col = "black", pch = 21, phi = 18, theta = 135, ticktype = "detailed", xlab = "X - log10 Price",
                  ylab = "Y - log10 Delay", zlab = "Z - Cigarettes Purchased", cex = 1, d =3, 
                  # xlim = c(-1.75, 1.75), ylim = c(-.5, 6),
                  # zlim = c(0, max(z) +1),
                  panel.first = points3D(x = log10(x),
                                         y = log10(y),
                                         cex = .25, pch = 21,
                                         z = rep(0, length(x)), add = TRUE, colkey = FALSE, col = "black")
                  ,
                  surf = list(z = matrix(
                      subset(predict(nlmeExpScales.5Log, newdata = mixedGrid, level = 0:1), 
                             id == partic)$predict.id, 
                      ncol = grains),
                      x = log10(xlong), y = log10(ylong),
                      facets = TRUE, col = grey(.9), 
                      fit = (subset(predict(nlmeExpScales.5Log, level = 0:1), id == partic)$predict.id),
                      alpha = .15, border = grey(.7)),
                  main = paste0("ID ", partic, ": EXPDtPrice * EXPDsDelay"))
    }
    )

}
dev.off()

