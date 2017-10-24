
#---������Ȼ����---#
#��1�� д����Ȼ������
#��2�� ����Ȼ����ȡ��������������
#��3�� ���� ��
#��4�� ����Ȼ���� 

#-д����Ȼ����-#
normal <- function(theta,x){
  mu <- theta[1]
  sigma2 <- theta[2]
  n <- length(x)
  logL <- -0.5*n*log(2*pi)-0.5*n*log(sigma2)-(1/(2*sigma2))*sum((x-mu)**2)
  return (-logL)
}

#-�����Ż�-#
x <- rnorm(100)
result <- optim(c(0,1),normal,x=x)

#--��maxlik�����й���--#
#-�ú���Ĭ����������Ȼ����ֵ
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

#---д����������ķֲ�---#
LL <- function(params,data){
  t1 <- dnorm(data, params[2], params[3])
  t2 <- dnorm(data, params[4], params[5])
  f <- params[1]*t1 + (1 - params[1])*t2
  ll <- sum(log(f))
  return(-ll)
}

#---��������---#
hist(waiting, freq = F)
lines(density(waiting))
geyser.res<-nlminb(c(0.5,50,10,80,10),LL,data=waiting,
                   lower=c(0.0001,-Inf,0.0001,-Inf,-Inf,0.0001),
                   upper=c(0.9999,Inf,Inf,Inf,Inf))

#---���ƽ��---#
#��ϵĲ���
geyser.res$par

#��ϵ�Ч��
X<-seq(40,120,length=100)
#�������ƵĲ���
p<-geyser.res$par[1]
mu1<-geyser.res$par[2]
sig1<-geyser.res$par[3]
mu2<-geyser.res$par[4]
sig2<-geyser.res$par[5]

#�����ƵĲ�����������ԭ�ܶȺ�����
f<-p*dnorm(X,mu1,sig1)+(1-p)*dnorm(X,mu2,sig2)
hist(waiting,probability=T,col=0,ylab="Density",
     ylim=c(0,0.04),xlab="Eruption waiting times")

#������ϵ�����
lines(X,f)

detach()

od <- options(digits = 5)
x <- 0:10
y <- c(26, 17, 13, 12, 20, 5, 9, 8, 5, 4, 8)

library(stats4)
## Easy one-dimensional MLE:[���򵥵�һά��MLE��]
nLL <- function(lambda) -sum(stats::dpois(y, lambda, log=TRUE))
fit0 <- mle(nLL, start = list(lambda = 5), nobs = NROW(y))
x



#---�����ĺ���---#
#d_density() p_�ֲ���q_��λ��������r_�������
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

#���һ���ع�ģ��

x1 <- rexp(1000, rate = 55)
x2 <- rexp(1000, rate = 0.1)
y <- rnorm(1000,mean = 10)
plot(lm(y~x1+x2))
look(x)

#-------------------------------------------#
#�۲�ϵ���ı仯
for(i in seq(1,300,30)){
  x <- rnorm(1000,10,i)
  plot(density(x))
}

#---һ������---#

x1 <- rnorm(100,2)
y1 <- rnorm(6,3)

model1 <- function(x = theta,theta = x,d){
  
  #---��������---#
  res = theta[1]*x^0 + theta[2]*x^1 +theta[3]*x^2 +
    theta[4]*x^3 + theta[5]*x^4 +theta[6]*x^5
  return(res)
  
  #---������ʧ---#
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

#---�Ż�---#
theta <- optimize(model1, c(0,1),d = 1.345)

#---һ������END---#






