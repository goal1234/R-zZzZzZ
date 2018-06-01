require(foreign)
require(ggplot2)
require(truncreg)
require(boot)



dat <- read.dta("https://stats.idre.ucla.edu/stat/data/truncreg.dta")

summary(dat)

# histogram of achiv coloured by program type
ggplot(dat, aes(achiv, fill = prog)) +
  geom_histogram(binwidth=3)



# boxplot of achiv by program type
ggplot(dat, aes(prog, achiv)) +
  geom_boxplot() +
  geom_jitter()


m <- truncreg(achiv ~ langscore + prog, data = dat, point = 40, direction = "left")

summary(m)
# update old model dropping prog
m2 <- update(m, . ~ . - prog)

pchisq(-2 * (logLik(m2) - logLik(m)), df = 2, lower.tail = FALSE)

# create mean centered langscore to use later
dat <- within(dat, {mlangscore <- langscore - mean(langscore)})

malt <- truncreg(achiv ~ 0 + mlangscore + prog, data = dat, point = 40)
summary(malt)

f <- function(data, i) {
  require(truncreg)
  m <- truncreg(formula = achiv ~ langscore + prog, data = data[i, ], point = 40)
  as.vector(t(summary(m)$coefficients[, 1:2]))
}

set.seed(10)

(res <- boot(dat, f, R = 1200, parallel = "snow", ncpus = 4))



# basic parameter estimates with percentile and bias adjusted CIs
parms <- t(sapply(c(1, 3, 5, 7, 9), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "bca"))
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5], bcaLL = bca[4],
              bcaLL = bca[5]))
}))

# add row names
row.names(parms) <- names(coef(m))
# print results
parms



dat$yhat <- fitted(m)

# correlation
(r <- with(dat, cor(achiv, yhat)))


# rough variance accounted for
r^2
