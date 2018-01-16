  gzs <- read.delim('clipboard',header = T)
  samp <- sample(200,100)
  testdata <- gzs[samp,]
  traindata <- gzs[-samp,]

  lgst1 <- glm(testdata$ת��~testdata$������EPS+testdata$�����ֹɱ���+testdata$�ɼ�+0,family=binomial(link = 'logit'),data=testdata) #ȥ��ؾ���
  lgst2 <- glm(testdata$ת��~testdata$������EPS+testdata$�����ֹɱ���+testdata$�ɼ�+0,family=binomial(link = 'probit'),data=testdata)
  glm <- glm(gzs$ת��~gzs$������EPS+gzs$�����ֹɱ���+gzs$�ɼ�+0,family=binomial(link = 'logit'),data=gzs)
  summary(lgst1);summary(lgst2)

  glm.step=step(lgst1) #ģ�ͽ�һ���Ż�
  AIC(lgst1);AIC(lgst2)  #����EPS�������ֹɡ��ɼ�������������ֱ�ӻ�����
  #����ϸ�������ͼ
  p <- predict(lgst1,type = 'response')
  p <- predict(lgst1,traindata[,])
  plot(seq(-10,10,length=120),sort(p),col='blue')
  
  #�����µ�����
  new <- read.delim('clipboard',header = T)
  newdata<-new[,3:5]
  newdata1<-newdata[1:289,];newdata2 <- newdata[230:518,];newdata3<-newdata[519:807,]
  s<- data.frame(newdata1,newdata2,newdata3)
  pre1 <- predict(glm,newdata1,interval='predict',type='response',terms=NULL)
  pre2 <- predict(glm,newdata2,interval='predict',type='response',terms=NULL)
  pre3 <- predict(glm,newdata3,interval='predict',type='response',terms=NULL)
  p1 <-exp(pre1)/(1+exp(pre1));p2 <- exp(pre2)/(1+exp(pre2));p3 <- exp(pre3)/(1+exp(pre3))   #ת��Ϊ����
  p<-c(p1,p2,p3)
  p<-data.frame(p)
  new1 <- new[1:867,]
  new1$p <- p
  
  library(xlsx)
  write.xlsx(p,"D:/log.xlsx")
  