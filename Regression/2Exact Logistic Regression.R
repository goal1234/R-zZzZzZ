# when the sample size is too small for a regular logistic regression 

require(elrm)

dat <- read.table(text = "
female  apcalc    admit       num
                  0        0        0         7
                  0        0        1         1
                  0        1        0         3
                  0        1        1         7
                  1        0        0         5
                  1        0        1         1
                  1        1        0         0
                  1        1        1         6",
                  header = TRUE)
                  

## expand dataset by repeating each row num times and drop the num
## variable
dat <- dat[rep(1:nrow(dat), dat$num), -4]

## look at various tables
xtabs(~female + apcalc, data = dat)

xtabs(~female + admit, data = dat)
xtabs(~apcalc + admit, data = dat)

xtabs(~female + apcalc + admit, data = dat)

x <- xtabs(~admit + interaction(female, apcalc), data = dat)
x  # view cross tabs

cdat <- cdat <- data.frame(female = rep(0:1, 2), apcalc = rep(0:1, each = 2), 
                           admit = x[2, ], ntrials = colSums(x))
cdat  # view collapsed data set



## model with female predictor only
m.female <- elrm(formula = admit/ntrials ~ female, interest = ~female, iter = 22000, 
                 dataset = cdat, burnIn = 2000)

## summary of model including estimates and CIs
summary(m.female)


## trace plot and histogram of sampled values from the sufficient
## statistic
plot(m.female)

## model with apcalc predictor only
m.apcalc <- elrm(formula = admit/ntrials ~ apcalc, interest = ~apcalc, iter = 22000, 
                 dataset = cdat, burnIn = 2000)
## summary of model including estimates and CIs
summary(m.apcalc)

## trace plot and histogram of sampled values from the sufficient
## statistic
plot(m.apcalc)


http://www.jstatsoft.org/v21/i03/paper
