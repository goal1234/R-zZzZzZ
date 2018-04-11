setwd("E:/R学习/TSData")    #设定工作位置
w<-read.csv('sf.csv')    #读取数据文件
w<-ts(w[,-1],start = c(1899,1),freq=1)    #从时间中提取
plot(w,main='',type='o',pch=16)     #绘图
w<-read.csv('NZRainfall.csv')
x<-ts(w[,-1],start = c(2000,1),freq=12)
plot(x,ylab = 'Monthly rainfall(mm)',type='o',pch=16,nc=1,main = '')
title('New Zealand Rainfall')
w<-read.csv('marriages_divorces-1899-2011.csv')
x<-ts(w[,1],start = c(1855),freq=1)
plot(x,ylab='Number of Marriages and Divorces',main='',type='o',pch=16,plot.type = 'single',lty=1:2)
title('Marriages and Divorce in Scotland')
legend('topleft',c('Marriage','Divorce'),lty=1:2)

##生产白噪声序列
set.seed(10);x=rnorm(150) #设个随机种子
par(mfrow=c(1,2))  #图片分
ts.plot(x);acf(x)  #画图

#模拟arima序列
set.seed(10) #设个随机种子
rw<-arima.sim(list(order=c(0,1,0)),n=200)
par(mfrow=c(2,2));ts.plot(rw);acf(rw);ts.plot(diff(rw));acf(diff(rw))

#趋势平稳序列过程
set.seed(101) #设定随机种子
x=NULL;for (i in 1:200) {x=c(x,.5-.3*i+rnorm(1))
  par(mfrow=c(2,2));ts.plot(x);acf(x);ts.plot(diff(x));acf(diff(x))
}
#不同参数下的ARIMA中MA
set.seed(63010);par(mfrow=c(1 ,3)) 
plot(acf(arima.sim(n=200,list(ma=c (.5,.3))) ,plot=F),ylab='acf ', type='o',pch=16, main=expression(paste('MA(2):', theta[1]==0.5,'','', theta[2]==0.3))) 
plot(acf(arima.sim(n=200,list(ma=c(.5 ,.3))),plot=F),ylab='cf', type='o',pch=16, main=expression(paste('MA(2）：', theta[1]==0.5,'','', theta[2]==-0.3)))
plot(acf(arima.sim(n=200,list(ma=c(- .75,-.3))),plot=F),ylab='acf' ,type='o',pch=16, main=expression(paste ('MA(2):',theta[1]==-0.75,'','',theta[2]==-0.3))) 

#ARIMA模型
set.seed(6301)
par(mfrow=c(2,2))
plot(arima.sim(n=200,list(ar=.4,ma=.5)),ylab='x',type='o',pch=16)
title(expression(paste('ARMA(1,1):',phi==0.4,',',theta=0.5)))
plot(arima.sim(n=200,list(ar=-.4,ma=.5)),ylab='x',type='o',pch=16)
title(expression(paste('ARMA(1,1):',phi==-0.4,',',theta=0.5)))
plot(arima.sim(n=20,list(order=c(1,1,1),ar=.4,d=1,ma=.5)),ylab='x',type='o',pch=16)
title(expression(paste ('ARIMA(1 , 1,1):',phi==0.4, ',', tbeta==0.5)))
plot(arima.sim(n=20,list(order=c(1,1,1),ar=-.4,d=1,ma=.5)),ylab='x',type='o',pch=16)
title(expression(paste ('ARIMA(1 , 1,1):',phi==-0.4, ',', tbeta==0.5)))

w<-read.csv('BOGAMBNS.csv')  #读入数据
x<-ts(w[,2],start = c(1959,1),freq=12) #成为年度数据
plot(x,main = 'US Board of Governors Monetary Base')
#数据做差分
par(mfrow=c(3,1))
ts.plot(diff(x),ylab='difference')  #原图像
title(expression(paste(nabla,'(x)')))
ts.plot(diff(x,12),ylab='difference')  #差分一次
title(expression(paste(nabla[12],'(x)')))
ts.plot(diff(diff(x,12)),ylab='difference')
title(expression(paste(nabla,nable[12],'(x)')))  #差分两次
#做STL分解
bst1<-stl(x,'per')  #第一个图原始数据、第二个图季节成分、第三个趋势成分、第四个误差成分
plot(bst1,main = 'stl decompositon')

#进行holt-winter滤波分析
(b1<-HoltWinters(x,seasonal='multiplicative')) #第一个是虚拟序列、水平成分、趋势成分、季节成分
plot(b1$fit,main = 'holt-winters decomposition')
#求残差
e<-b1$x-b1$fitted[,1]
plot(e,main = 'Residuals')

x<-scan('darwin.slp.txt')  #读入数据
x<-ts(x,start = c(1882,1),freq=12)  #标以开始时间和周期
plot(x,ylab='sea level pressure')
title('sea level pressure at darwin') 
X<-window(x,start=c(1980,1),freq=12)  #取1980年之后的部分数据
par(mfrow=c(1,2))
plot(diff(X,12),ylab = 'difference');abline(h=0,lty=2) #画图
title(expression(paste(nabla[12],'(x)'))) #标题
plot(diff(diff(X,12)),ylab = 'difference')
title(expression(paste(nabla[1],nabla[12],'(x)')))
abline(h=0,lty=12)

#stl方法分解
x<-scan('darwin.slp.txt')
x<-ts(x,start = c(1882,1),freq=12)
(b0<-stl(x,'per'));plot(b0)

#用holt-winters波分解
(b<-HoltWinters(x))
plot(b$fit)

#看残差的分布
e<-b$x-b$fitted[,1]
par(mfrow=c(1,3))
hist(e,main = 'residuals',col=4,xlab = expression(x-hat(x)),probability = T)
lines(density(e),lwd=2);rug(e)
qqnorm(e);qqline(e);plot(e,ylab='residuals')

#混成检验
library(forecast);library(parallel);library(portes)
par(mfrow=c(1,2))
plot(gvtest(e,1:60)[,4],main='gneralized variance tests',ylab='p-value',xlab='lag',pch=16,ylim=c(0,1))
abline(h=0.05,lty=2)
Acf(e,main='ACF of residuals',lag.max = 60)
#模型预测
(p<-predict(b,72))
plot(x,xlim=(c(min(time(x)),max(time(x)))));lines(p,lty=2)

#数据读取
w<-read.csv('airports.csv')  #数据读取
v<-ts(w,start = c(1996,1),freq=12)
plot(v,plot.type='single',lty=1:ncol(w),ylab='number of passengers')
legend('topleft',names(w),lty=1:ncol(w),cex=.9)
title('number of passengers in 12 airports from 1995 to 2003')
#对数据做STL分解
plot(stl(x,'per'))
#对数据做喉holtwinter
w<-read.csv('airports.csv')
x<-ts(w,start=c(1995,1),freq=12)
X<-window(x,end=c(2003,1),freq=12)
a<-HoltWinters(X,seasonal = 'multiplicative')
Y<-predict(a,n.ahead=12,prediction.interval=FALSE)
plot(x,ylim=range(c(x,Y)),lty=3,lwd=2)
lines(Y,lwd=2)
title('holt winter model for passenger')
legend('topleft',c('original series','predicted series'),lty = c(2,1),lwd = 2)
plot(a,Y)

#根据acf和pacf图对ARMA模型  acf对应MA PACF对于AR
par(mfcol=c(2,3));set.seed(1010)
plot(ARMAacf(ma=c(.5,-.4),lag.max = 18,pacf = F),type='h',ylab='PACF')
title('Exact  ACF for MA(0.5,-0.4');abline(h=0)
acf(arima.sim(n=63,list(ma=c(.5,-.4)),sd=sqrt(0.2)),main='acf of simua;te ma(0.5,-0.4')

plot(ARMAacf(ma=c(-.5,.4),lag.max = 18,pacf = T),type='h',ylab='PACF')
title('Exact  ACF for MA(-0.5,0.4');abline(h=0)
pacf(arima.sim(n=63,list(ma=c(-.5,.4)),sd=sqrt(0.2)),main='acf of simua;te ma(-0.5,0.4')

plot(ARMAacf(ar=c(-.3,.4),ma=c(-.3,.25),lag.max = 18,pacf = F),type='h',ylab='ACF')
title('Exact  ACF for MA(-0.3,0.25');abline(h=0)
pacf(arima.sim(n=63,list(ar=c(-.3,.4),ma=c(-.3,0.4)),sd=sqrt(0.2)),main='acf of simua;te ma(-.3,0.4')

#尼罗河
plot(Nile,ylab='flow',main='annual flow of nile at ashwan 1871-1970')
par(mfrow=c(1,2));acf(Nile,lag.max = 60);pacf(Nile,lag.max = 60)
library(leaps);library(TSA);library(locfit);library(mgcv);library(nlme)
res<-armasubsets(y=Nile,nar = 15,nma = 15,y.name = 'test',ar.method = 'ols')
plot(res)
(fit<-arima(Nile,c(1,0,12)))
