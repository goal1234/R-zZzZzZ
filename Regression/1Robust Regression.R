

#  Iteratively Reweighted Least Squares (IRLS).
# 在一定条件下进行更新

cdata <- read.dta("https://stats.idre.ucla.edu/stat/data/crime.dta")
summary(cdata)



summary(ols <- lm(crime ~ poverty + single, data = cdata))


opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(ols, las = 1)
par(opar)

cdata[c(9, 25, 51), 1:2]

dl <- cook.distance(ols)
r <- stdres(ols)
a <- cbind(cdata, dl, r)
a[dl > 4/ 51, ]

rabs <- abs(r)
a <- cbind(cdata, d1, r, rabs)
asorted <- a[order(-rabs), ]
asorted[1:10, ]


# 
library(MASS)
summary(rr.huber <- rlm(crime ~ poverty + single, data = cdata))

hweights <- data.frame(state = cdata$state, resid = rr.huber$resid, weight = rr.huber$w)
hweights2 <- hweights[order(rr.huber$w), ]
hweights2[1:15, ]


rr.bisquare <- rlm(crime ~ poverty + single, data=cdata, psi = psi.bisquare)
summary(rr.bisquare)



biweights <- data.frame(state = cdata$state, resid = rr.bisquare$resid, weight = rr.bisquare$w)
biweights2 <- biweights[order(rr.bisquare$w), ]
biweights2[1:15, ]


