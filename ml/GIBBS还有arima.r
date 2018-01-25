#sample from multidimensional distributions using Gibbs sampling#
if(FALSE){
    这是一种模拟计算的方法,不停的抽样就会产生接近p()的分布，但这并不容易做到，
    于是拒绝一定的采样结果，接收一定的结果，在X轴上平移一个分布,在y轴上平移一个
    在分布中按照重要性的分布去抽样，背后是马尔科夫链子，在转移矩阵对应的值进行采样
}

#---简述下算法---#
if(FALSE){
    1.Select the initial values
    2.For repeat:
        2.1 For sample from distribution
    3.Repeat step 2 until the distribution of vector stabilizes.
    4.The subsequent iterations of point 2 will provide a randomization of .
}


gibbs_normal_sampling <- function(n_iteration, init_point, ro){
  # init point is some numeric vector of length equals to 2
  theta_1 <- c(init_point[1], numeric(n_iteration))
  theta_2 <- c(init_point[2], numeric(n_iteration))
  # get one assign 
  for (i in 2:(n_iteration+1)) {
    theta_1[i] <- rnorm(1, ro * theta_2[i-1], sqrt(1 - ro^2))
    theta_2[i] <- rnorm(1, ro * theta_1[i], sqrt(1 - ro^2))
  }
  list(theta_1, theta_2)
}

#---通过对GIBBS采样估计系数---#

#--描述下算法--#
if(FALSE){
    1.Choose starting point for the mean
    2.In the -th iteration do:
            With the probability:
            draw for
            Calculate:
            Generate:
    3.Perform step 2. until the distribution of vector stabilizes.
}


#--公式目标--#
#看来会写公式是很必要的#

set.seed(1314)
mu_1 <- 10
mu_2 <- 2
sigma_1 <- 1
sigma_2 <- 2
pi_known <- 0.7
n <- 2000
pi_sampled <- rbinom(n, 1, pi_known)
y_1 <- rnorm(n, mu_1, sigma_1)
y_2 <- rnorm(n, mu_2, sigma_2)
y <- (1 - pi_sampled) * y_1 + pi_sampled * y_2


#---计算的参数设定---#
mu_init <- c(12, 0)
n_iterations <- 300
delta <- vector("list", n_iterations)
mu_1_vec <- c(mu_init[1], numeric(n_iterations))
mu_2_vec <- c(mu_init[2], numeric(n_iterations))

delta_probability <- function(i, y, mu_1_vec, mu_2_vec, sigma_1, sigma_2, pi_known) {
  pi_known * dnorm(y, mu_2_vec[i - 1], sigma_2) /
      ((1- pi_known) * dnorm(y, mu_1_vec[i - 1], sigma_1) + pi_known * dnorm(y, mu_2_vec[i - 1], sigma_2))
}

mu_1_mean <- function(delta, i, y) {
  sum((1 - delta[[i - 1]]) * y) / (1 + sum(1 - delta[[i - 1]]))
}

mu_2_mean <- function(delta, i, y) {
  sum(delta[[i - 1]] * y) / (1 + sum(delta[[i - 1]]))
}

for (j in 2:(n_iterations + 1)) {
  #这个设计在第二个list里，同时用了map_int()#
  #应用了purrr包里面的函数映射
  delta[[j - 1]] <- y %>% map_int(
    ~ rbinom(1, 1, delta_probability(j, ., mu_1_vec, mu_2_vec, sigma_1, sigma_2, pi_known)))
  mu_1_vec[j] <- rnorm(1, mu_1_mean(delta, j, y), sqrt(1 / (1 + sum(1 - delta[[j - 1]]))))
  mu_2_vec[j] <- rnorm(1, mu_2_mean(delta, j, y), sqrt(1 / (1 + sum(delta[[j - 1]]))))
}



#---一个arima模型的练习---#
#---在这个沉闷的下午---#

suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(forecast))
suppressPackageStartupMessages(library(astsa))
suppressPackageStartupMessages(library(lmtest))
suppressPackageStartupMessages(library(fUnitRoots))
#--这个包不在cran上--#
suppressPackageStartupMessages(library(FitARMA))
suppressPackageStartupMessages(library(strucchange))
suppressPackageStartupMessages(library(reshape))
suppressPackageStartupMessages(library(Rmisc))
suppressPackageStartupMessages(library(fBasics))

#EDA
url <- "https://www.openintro.org/stat/data/arbuthnot.csv"
abhutondot <- read.csv(url, header=TRUE)
nrow(abhutondot)

head(abhutondot)

#---还是长型的数据比较好---#
abhutondot_rs <- melt(abhutondot, id = c('year'))
head(abhutondot_rs)
tail(abhutondot_rs)

ggplot(data = abhutondot_rs, aes(x = year)) + geom_line(aes(y = value, colour = variable)) +
       scale_colour_manual(values = c("blue", "red"))

t.test(value ~ variable, data = abhutondot_rs)

basicStats(abhutondot[-1])
p1 <- ggplot(data = abhutondot_rs, aes(x = variable, y = value)) + geom_boxplot()
p2 <- ggplot(data = abhutondot, aes(boys)) + geom_density()
p3 <- ggplot(data = abhutondot, aes(girls)) + geom_density()
multiplot(p1, p2, p3, cols = 3)

excess_frac <- (abhutondot$boys - abhutondot$girls)/abhutondot$girls
excess_ts <- ts(excess_frac, frequency = 1, start = abhutondot$year[1])
autoplot(excess_ts)

basicStats(excess_frac)

urdftest_lag = floor(12*(length(excess_ts)/100)^0.25)
urdfTest(excess_ts, type = "nc", lags = urdftest_lag, doplot = FALSE)

#---acf还有pacf进行定阶---#
par(mfrow = c(1,2))
acf(excess_ts)
pacf(excess_frac)

#---一言不合就看下lm模型---#
summary(lm(excess_ts~1))

(break_point <- breakpoints(excess_ts ~ 1))
plot(break_point)

summary(break_point)

plot(excess_ts)
lines(fitted(break_point, breaks = 1), col = 4)
lines(confint(break_point, breaks = 1))

fitted(break_point)[1]
fitted(breakpoint)[length(excess_ts)]

break_date <- breakdates(break_point)
win_1 <- window(excess_ts, end = break_date)
win_2 <- window(excess_ts, start = break_date + 1)
t.test(win_1, win_2)

#---ARIMA Models---#
#这个模型做的太长效果很不好#
#-需要进行的步骤有限#

if(FALSE){
    为了拟合(p, d, q)的参数
    非季节性模型
    季节性模型
    加入一个漂移
}

#---here we go---#
#Model 1

(model_1 <- auto.arima(excess_ts, stepwise = FALSE, trace = TRUE)
summary(model_1)

coeftest(model_1)

#Model #2
model_2 <- Arima(excess_ts, order = c(1,0,0), 
                    seasonal = list(order = c(0,0,1), period = 10), 
                    include.mean = TRUE)
summary(model_2)

#Model #3
model_3 <- Arima(excess_ts, order = c(1,0,0), 
                    seasonal = list(order = c(1,0,0), period = 10), 
                    include.mean = TRUE)
summary(model_3)

coeftest(model_3)

#Model #4
level <- c(rep(0, break_point$breakpoints), 
              rep(1, length(excess_ts) - break_point$breakpoints))

model_4 <- Arima(excess_ts, order = c(0,0,0), 
                    seasonal = list(order = c(0,0,1), period = 10), 
                    xreg = level, include.mean = TRUE)
summary(model_4)

coeftest(model_4)

#Model #5
model_5 <- Arima(excess_ts, order = c(1,0,0), 
                 seasonal = list(order = c(0,0,1), period=10), 
                 xreg = level, include.mean = TRUE)
summary(model_5)
coeftest(model_5)

#Model #6
model_6 <- Arima(excess_ts, order = c(1,0,0), xreg = level, include.mean = TRUE)
summary(model_6)

#---Models Residuals Analysis---#
checkresiduals(model_1)
#-必然要做的一个检验-#
LjungBoxTest(residuals(model_1), k = 2, lag.max = 20)
sarima(excess_ts, p = 1, d = 1, q = 1)

#-Model #2 Residuals Diagnostic-#
checkresiduals(model_2)
LjungBoxTest(residuals(model_2), k = 2, lag.max = 20)
sarima(excess_ts, p = 1, d = 0, q = 0, P = 0, D = 0, Q = 1, S = 10)

#-Model #3 Residuals Diagnostic-#
checkresiduals(model_3)
LjungBoxTest(residuals(model_3), k = 2, lag.max = 20)

sarima(excess_ts, p = 1, d = 0, q = 0, P = 1, D = 0, Q = 0, S = 10)

#---Model #4 Residuals Diagnostic---#
checkresiduals(model_4)

LjungBoxTest(residuals(model_4), k = 1, lag.max = 20)

sarima(excess_ts, p = 0, d = 0, q = 0, P = 0, D = 0, Q = 1, S = 10, xreg = level)

#--Model #6 Residuals Diagnostic--#
checkresiduals(model_6)
LjungBoxTest(residuals(model_6), k = 1, lag.max = 20)
sarima(excess_ts, p = 1, d = 0, q = 0, xreg = level)

f <- data.frame(col_1_res = c(model_1$aic, model_2$aic, model_3$aic, model_4$aic, model_6$aic),
                    col_2_res = c(model_1$aicc, model_2$aicc, model_3$aicc, model_4$aicc, model_6$aicc),
                    col_3_res = c(model_1$bic, model_2$bic, model_3$bic, model_4$bic, model_6$bic))

colnames(df) <- c("AIC", "AICc", "BIC")
rownames(df) <- c("ARIMA(1,1,1)", 
                  "ARIMA(1,0,0)(0,0,1)[10]", 
                  "ARIMA(1,0,0)(1,0,0)[10]", 
                  "ARIMA(0,0,0)(0,0,1)[10] with level shift", 
                  "ARIMA(1,0,0) with level shift")
df

#----Forecast----#
h_fut <- 20
plot(forecast(model_4, h = h_fut, xreg = rep(1, h_fut)))







