
#---极大似然估计---#
#（1） 写出似然函数；
#（2） 对似然函数取对数，并整理；
#（3） 求导数 ；
#（4） 解似然方程 

#-写出似然函数-#
normal <- function(theta,x){
  mu <- theta[1]
  sigma2 <- theta[2]
  n <- length(x)
  logL <- -0.5*n*log(2*pi)-0.5*n*log(sigma2)-(1/(2*sigma2))*sum((x-mu)**2)
  return (-logL)
}

#-进行优化-#
x <- rnorm(100)
result <- optim(c(0,1),normal,x=x)

#--用maxlik包进行估计--#
#-该函数默认是正的似然函数值
normal <- function(theta){
  mu <- theta[1]
  sigma <- theta[2]
  logL <- -0.5*N*log(2*pi) - N*log(sigma) - sum(0.5*(x - mu)^2/sigma^2)
  return (logL)
}
library(maxLik)
result <- maxLik(normal,start=c(0,1))
print(result)


#---#
library(MASS)
data("geyser")
attach(geyser)
hist(waiting)

#---写出这个函数的分布---#
LL <- function(params,data){
  t1 <- dnorm(data, params[2], params[3])
  t2 <- dnorm(data, params[4], params[5])
  f <- params[1]*t1 + (1 - params[1])*t2
  ll <- sum(log(f))
  return(-ll)
}

#---参数估计---#
hist(waiting, freq = F)
lines(density(waiting))
geyser.res<-nlminb(c(0.5,50,10,80,10),LL,data=waiting,
                   lower=c(0.0001,-Inf,0.0001,-Inf,-Inf,0.0001),
                   upper=c(0.9999,Inf,Inf,Inf,Inf))

#---估计结果---#
#拟合的参数
geyser.res$par

#拟合的效果
X<-seq(40,120,length=100)
#读出估计的参数
p<-geyser.res$par[1]
mu1<-geyser.res$par[2]
sig1<-geyser.res$par[3]
mu2<-geyser.res$par[4]
sig2<-geyser.res$par[5]

#将估计的参数函数代入原密度函数。
f<-p*dnorm(X,mu1,sig1)+(1-p)*dnorm(X,mu2,sig2)
hist(waiting,probability=T,col=0,ylab="Density",
     ylim=c(0,0.04),xlab="Eruption waiting times")

#画出拟合的曲线
lines(X,f)

detach()

od <- options(digits = 5)
x <- 0:10
y <- c(26, 17, 13, 12, 20, 5, 9, 8, 5, 4, 8)

library(stats4)
## Easy one-dimensional MLE:[＃简单的一维的MLE：]
nLL <- function(lambda) -sum(stats::dpois(y, lambda, log=TRUE))
fit0 <- mle(nLL, start = list(lambda = 5), nobs = NROW(y))
x



#---常见的函数---#
#d_density() p_分布，q_分位数函数，r_随机函数
rnorm(n, mean = 0, sd = 1)

x <- rnorm(1000)
look <- function(x){
  par(mfrow = c(2,2))
  plot(x)
  boxplot(x)
  hist(x)
  qqnorm(x)
  par(mfrow = c(1,1))
}
look(x)

for( i in 1:30){
  x <- rexp(1000,rate = i)
  look(x)
}

#拟合一个回归模型

x1 <- rexp(1000, rate = 55)
x2 <- rexp(1000, rate = 0.1)
y <- rnorm(1000,mean = 10)
plot(lm(y~x1+x2))
look(x)

#-------------------------------------------#
#观察系数的变化
for(i in seq(1,300,30)){
  x <- rnorm(1000,10,i)
  plot(density(x))
}

#---一个函数---#

x1 <- rnorm(100,2)
y1 <- rnorm(6,3)

model1 <- function(x = theta,theta = x,d){
  
  #---函数表达---#
  res = theta[1]*x^0 + theta[2]*x^1 +theta[3]*x^2 +
    theta[4]*x^3 + theta[5]*x^4 +theta[6]*x^5
  return(res)
  
  #---定义损失---#
  len <- length(res)
  loss <- vector()
  for(i in 1:len){
    if(abs(e[i]) <= d){
      var <- e[i]^2/2
    }else{
      var <- d*(abs(e[i]) - d/2)
    }
    loss <- c(loss,var)
  }
  return(loss)
  
}

#---优化---#
theta <- optimize(model1, c(0,1),d = 1.345)

#---一个函数END---#







