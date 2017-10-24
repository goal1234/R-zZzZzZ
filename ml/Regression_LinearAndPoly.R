  #���Թ�ϵ
  p <- 0.5
  q <- seq(0, 100, 1)
  y <- p*q
  plot(q, y, type = 'l', col='red',main='Linear relationship')
  
  #�����Թ�ϵ
  y <- 450 + p*(q-10)^3
  plot(q,y,type='l',col='navy',main='Nonlinear relationship',lwd=3)
  
  #һ�������������
  set.seed(20)
  q <- seq(from=0, to=20, by=0.1)
  y <- 500 + 0.4 * (q-10)^3
  noise <- rnorm(length(q), mean=10, sd=80)
  noisy.y <- y + noise
  plot(q,noisy.y,col='deepskyblue4',xlab='q',main='Observed data')
  lines(q,y,col='firebrick1',lwd=3)
  
  #һ������ʽģ��
  model <- lm(noisy.y ~ poly(q,3))
  model <- lm(noisy.y ~ x + I(X^2) + I(X^3))
  summary(model)
  
  #��ȡ��������
  confint(model, level=0.95)
  
  #һ�����VS�в�ͼ
  plot(fitted(model), residuals(model))
  
  #Ԥ����������
  predicted.intervals <- predict(model,data.frame(x=q),interval='confidence',level=0.99)
  
  #���������
  lines(q,predicted.intervals[,1],col='green',lwd=3)
  lines(q,predicted.intervals[,2],col='black',lwd=1)
  lines(q,predicted.intervals[,3],col='black',lwd=1)
  
  #����ͼ��
  legend("bottomright",c("Observ.","Signal","Predicted"),col=c("deepskyblue4","red","green"), lwd=3)
  
  #==================�쳣�����===================#
  x<-rexp(100,0.2)  
  e<-rnorm(100)  
  y<-0.5+1.7*x+e 
  
  lm(y~x)  
  y[50]<-0.7+0.2*x[50]+e[50]  
  lm.reg1<-lm(y~x)  
  lm.reg1 
  
  library(car)
  #�쳣����
  outlierTest(lm.reg1)
  y[100]<-3+5.8*x[100]+2*e[100]  
  lm.reg<-lm(y~x)  
  outlierTest(lm.reg)  
  
  #cook distance
  cooks.distance(lm.reg)  
  result<-cooks.distance(lm.reg)  
  result[cooks.distance(lm.reg)>4/(100-1-1)]  
  
  #cook distance ��ͼ
  cook<-4/(100-1-1)  
  plot(lm.reg,which=4,cook.levels=cook)  
  abline(h=cook,lty=2,col=2) 
  
  #����Ӱ���
  influencePlot(lm.reg)  
  
  #ͨ��car���еĺ����������췽��ȼ���
  library(xlsx)  
  workbook<-"D:/R/data/3-15.xlsx"  
  mydataframe<-read.xlsx(workbook,1)  
  lm.reg2<-lm(Y~X,data=mydataframe)  
  spreadLevelPlot(lm.reg2) 
  
  library(xlsx)  
  workbook<-"D:/R/data/3-15.xlsx"  
  mydataframe<-read.xlsx(workbook,1)  
  lm.reg2<-lm(Y~X,data=mydataframe)  
  spreadLevelPlot(lm.reg2)
  
  library(MASS)  
  boxcox(lm.reg2,lambda= seq(-1, 1, length = 50))  
  which.max(box$y)  
  box$x[77]  
  summary(powerTransform(mydataframe$Y))  
  boxTidwell(Y~X,data=mydataframe)  
  
  which.max(box$y)
  box$x[77]
  
  #------ȫ���Լ��ع�-----#
  library(car)
  subsets(object,
          
          names=abbreviate(object$xnames, minlength = abbrev),
          
          abbrev=1, min.size=1, max.size=length(names), legend,
          
          statistic=c("bic", "cp", "adjr2","rsq", "rss"),
          
          las=par('las'), cex.subsets=1, ...)
  
  #car���е�scatterplot()�����������Ժ����ס�����ػ��ƶ�Ԫ��ϵͼ
  library(car)
  scatterplot(weight ~ height, data = women,spread = FALSE, lty.smooth = 2, pch = 19, 
              main ="30-39 ��Ů��", xlab = "����(Ӣ��)", ylab = "����(��)")
  scatterplotMatrix(states, spread = FALSE, main = "ScatterplotMatrix") 
  
  #---effects����������������ͬwt�£�mpg��hp֮������Թ�ϵ---#
  fit <- lm(mpg ~ hp + wt + hp:wt, data = mtcars)  
  summary(fit)  
  library(effects)
  plot(effect("hp:wt", fit, xlevels= list(wt = c(2.2, 3.2,4.2))), multiline = TRUE) 
  