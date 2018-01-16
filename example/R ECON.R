data('Journals',package = 'AER')
dim(Journals)
names(Journals)
#����ģ��
j_lm<-lm(log(subs)~log(price/citations),data = Journals)
abline(j_lm)    #����
summary(j_lm)    #ģ��ͳ��

##�ڶ���
data('CPS1985',package = 'AER')
cps<-CPS1985    #��������
library('SparseM')
library('quantreg')
cps_lm<-lm(log(wage)~experience+I(experience^2)+education,data=cps)
cps_rq<-rq(log(wage)~experience+I(experience^2)+education,data=cps,tau = seq(0.2,0.8,by=0.15))
cps2<-data.frame(education=mean(cps$education),experience=min(cps$experience):max(cps$experience))
cps2<-cbind(cps2,predict(cps_lm,newdata=cps2,interval='prediction'))
cps2<-cbind(cps2,predict(cps_rq,newdata = cps2,type=''))

#��ͼ
plot(log(wage)~experience,data = cps)
for (i in 1:6) {
  lines(cps2[,i]~experience,data=cps2,col='red')
}

#KernSmooth��ͼ
library('KernSmooth')
cps_bkde<-bkde2D(cbind(cps$experience,log(cps$wage)),bandwidth = c(3.5,0.5),gridsize=c(200,200))
image(cps_bkde$x1,cps_bkde$x2,cps_bkde$fhat,col=rev(gray.colors(10,gamma = 1)),xlab='experience',ylab='log(wage)')
box()
lines(fit~experience,data=cps2)
lines(lwr~experience,data=cps2,lty=2)
lines(upr~experience,data = cps2,lty=2)
objects()
search()
#���ݲ���
log(16,2)
log(x=16,2)
log(16,base = 2)
log(base = 2,x=16)
one<-rep(1,10)
even<-seq(from=2,to=20,by=2)
trend<-1981:2005
c(one,even)
A<-matrix(1:6,nrow = 2)
t(A)
dim(A)
nrow(A)
A1<-A[1:2,c(1,3)]
solve(A1)
A1%*%solve(A1)
diag(4)
cbind(1,A1)
rbind(A1,diag(4,2))
x<-c(1.8,3.14,4,88.169,13)
x>3.5
names(x)<-c('a','b','c','d','e')
x[3:5]
x[c('c','d','e')]
x[x>3.5]
mylist<-list(sample=rnorm(5),family='normal distribution',parameters=list(mean=0,sd=1))

##random number generation
set.seed(123)
rnorm(2)
set.seed(123)
rnorm(2)
sample(1:5)
sample(c('male','female'),size=5,replace=TRUE,prob=c(0.2,0.8))

##�����ж�
x<-c(1.8,3.14,4,88.169,13)
if(rnorm(1)>0)sum(x) else mean(x)
ifelse(rnorm(1)>0,sum(x),mean(x))
ifelse(x>4,sqrt(x),x^2)
##ѭ��
for (i in 2:5) {
  x[i]<-x[i]-x[i-1]
}
x[-1]  #ɾ��
while (sum(x)<100) {
  x<-2*x  
}
#��ʽ
cmeans<-function(x){
  rval<-rep(0,ncol(x))
  for(j in 1:ncol(x)){
    mysum<-0
    for (i in 1:nrow(x)) {
      mysum<-mysum+x[i,j]
      rval[j]<-mysum/nrow(x)
    }
    return(rval)
  }
}
x<-matrix(1:20,ncol=2)
cmeans(x)
colMeans(x)
x<-seq(from=0,to=10,by=0.5)
y<-2+3*x+rnorm(21)
plot(y~x)
lm(y~x)
##���ݹ���
mydata<-data.frame(one=1:10,two=11:20,three=21:30)
mydata<-as.data.frame(matrix(1:30,ncol=3))
names(mydata)<-c('one','two','three')
mydata$two
mydata$[,'two']
mydata[,2]
attach(mydata)
mean(two)
detach(mydata)
with(mydata,mean(two))
subset(mydata,two<=16,select=-two)

#import and export
write.table(mydata,file='mydata.txt',col.names = TRUE)
newdata<-read.table('mydata.txt',header = T)
save(mydata,file = 'mydata.rda')
load(mydata.rda)
#�������
summary.normsample<-function(object,...){
  rval<-c(length(object),mean(object),sd(object))
  names(rval)<-c('sample size','mean','standard deviation')
  return(rval)
}
#��ͼ
data('Journals',package = 'AER')
Journals$citeprice<-Journals$price/Journals$citations
attach(Journals)
plot(log(subs),log(citeprice))
rug(log(sub))
rug(log(citeprice),side=2)
detach(Journals)

#��ͼ
attach(CPS1985)
hist(wage,freq = FALSE)
hist(log(wage),freq = FALSE)
lines(density(log(wage)),col=4)

#�߼ʱ�
summary(occupation)
tab<-table(occupation)
prop.table(tab)
barplot(tab)
pie(tab)
#˫��
xtabs(~gender+occupation,data=CPS1985)
plot(gender~occupation,data=CPS1985)  #�������������ݻ�ͼ

#�����
cor(log(wage),education)
cor(log(wage),education,method = 'spearman')
plot(log(wage)~education)
tapply(log(wage),gender, mean)
plot(log(wage)~gender)

#��ȡ����
mwage<-subset(CPS1985,gender=='male')$wage
fwage<-subset(CPS1985,gender=='famele')$wage
qqplot(mwage,fwage,xlab='male',ylab='female')
abline(0,1)
detach(CPS1985)

#���Իع�
data(Journals)
data('Journals',package = 'AER')
journals<-Journals[,c('subs','price')]
journals$citeprice<-Journals$price/Journals$citations
summary(journals)
plot(log(subs)~log(citeprice),data=journals)
jour_lm<-lm(log(subs)~log(citeprice),data = journals)
abline(jour_lm)
class(jour_lm)
names(jour_lm)
summary(jour_lm)
anova(jour_lm)
coef(jour_lm)  #��ȡϵ��
predict(jour_lm,newdata=data.frame(citeprice=2.11),interval='confidence')
predict(jout_lm,newdata=data.frame(cireprice=2.11),interval='prediction')
lciteprice<-seq(from=-6,to=4,by=0.25)
jour_pred<-predict(jour_lm,interval = 'prediction',newdata = data.frame(citeprice=exp(lciteprice)))
plot(log(subs)~log(citeprice),data=journals)
lines(jour_pred[,1]~lciteprice,col=1)
lines(jour_pred[,2]~lciteprice,col=1,lty=2)
lines(jour_pred[,3]~lciteprice,col=1,lty=2)
par(mfrow=c(2,2))
plot(jour_lm)
par(mfrow=c(1,1))
linear.hypothesis(jour_lm,'log(citeprice)=-0.5')
data("CPS1988")
summary(CPS1988)
#��Ԫ�ع�
cps_lm<-lm(log(wage)~experience+I(experience^2)+education+ethnicity,data = CPS1988)
cps_noeth<-lm(log(wage)~experience+I(experience^2)+education,data = CPS1988)
anova(cps_noeth,cps_lm) #�Աȷ���
anova(cps_lm)
cps_noeth<-update(cps_lm,formula=.~.-ethnicity)
waldtest(cps_lm,.~.-ethnicity)
#BICѡ��ع�
library(splines)
cps_lm<-lm(log(wage)~bs(experience,df=5)+education+ethnicity,data=CPS1988)
cps_bs<-lapply(3:10, function(i)lm(log(wage)~bs(experience,df=i)+education+ethnicity,data=CPS1988))
structure(sapply(cps_bs, AIC,k=log(nrow(CPS1988))),.Names=3:10)
cps<-data.frame(experience=-2:60,education=with(CPS1988,mean(education[ethnicity=='cauc'])),ethnicity='cauc')
cps$yhat1<-predict(cps_lm,newdata = cps)
cps$yhat2<-predict(cps_plm,newdata=cps)
plot(log(wage)~jitter(experience,factor = 3),pch=19,col=rgb(0.5,0.5,0.5,alpha=0.02),data=CPS1988)
lines(yhat1~experience,data=cps,lty=2)
lines(yhat2~experience,data=cps)
legend('topleft',c('quadratic','spline'),lty=c(2,1),bty='n')

#��������
data(CPS1988,package = 'AER')
cps_int<-lm(log(wage)~experience+I(experience^2)+education*ethnicity,data = CPS1988)
coeftest(cps_int)
cps_int<-lm(log(wage)~experience+I(experience^2)+education+ethnicity+education:ethnicity,data=CPS1988)
#ÿ��ˮƽ�µĻع�
cps_sep<-lm(log(wage)~ethnicity/(experience+I(experience^2)+education)-1,data=CPS1988)
cps_sep_cf<-matrix(coef(cps_sep),nrow=2)
rownames(cps_sep_cf)<-levels(CPS1988$ethnicity)
colnames(cps_sep_cf)<-names(coef(cps_lm)[1:4])
anova(cps_sep)
#change of reference catagory
CPS1988$region<-relevel(CPS1988$region,ref = 'south')
cps_region<-lm(log(wage)~ethnicity+education+experience+I(experience^2)+region,data=CPS1988)
coef(cps_region)

#��Ȩ�ع�
jour_wls1<-lm(log(subs)~log(citeprice),data=journals,weights = 1/citeprice^2)
jour_wls2<-lm(log(subs)~log(citeprice),data = journals,weights = 1/citeprice)
plot(log(subs)~log(citeprice),data=journals)
abline(jour_lm)
abline(jour_wls1,lwd=2,lty=2)
abline(jour_wls2,lwd=2,lty=2)
legend('bottomleft',c('OLS','WLS1','WLS2'),lty=1:3,lwd=2,bty='n')

#�в�ع�
auxreg<-lm(log(residuals(jour_lm)^2)~log(citeprice),data=journals)
jour_fgls<-lm(log(subs)~log(citeprice),weights = 1/exp(fitted(auxreg)),data=journals)
gamma2i<-coef(auxreg)[2]
gamma2<-0
while(abs(gamma2i-gamma2)/gamma>1e-7){
    gamm2<-gamma2i
    fglsi<-lm(log(subs)~log(citeprice),data=journals,weights = 1/citeprice^gamm2)
    gamm2i<-coef(lm(log(residuals(fglsi)^2)~log(citeprice),data = journals))[2]
}
jour_fgls2<-lm(log(subs)~log(citeprice),data=journals,weights = 1/citeprice^gamma2)
coef(jour_fgls2)

#ʱ���������Իع�
library(dynlm)
library(zoo)

data('USMacroG',package='AER')
plot(USMacroG[,c('dpi','consumption')],lty=c(3,1),plot.type = 'single',ylab='')
legend('topleft',legend = c('income','comsumption'),lty=c(3,1),bty='n')
library('dynlm')
cons_lm1<-dynlm(consumption~dpi+L(dpi),data = USMacroG)
cons_lm2<-dynlm(consumption~dpi+L(consumption),data=USMacroG)
summary(cons_lm1)
plot(merge(as.zoo(USMacroG[,"consumption"]), fitted(cons_lm1),
            fitted(cons_lm2), 0, residuals(cons_lm1),
            residuals(cons_lm2)), screens = rep(1:2, c(3, 3)),
     lty = rep(1:3, 2), ylab = c("Fitted values", "Residuals"),
     xlab = "Time", main = "")
legend(0.05, 0.95, c("observed", "cons_lm1", "cons_lm2"),lty = 1:3, bty = "n")
cons_lmE<-dynlm(consumption~dpi+L(dpi)+L(consumption),data=USMacroG)
anova(cons_lm1,cons_lmE,cons_lm2)
#lmtest��
library(lmtest)
encomptest(cons_lm1,cons_lm2)
data('Grunfeld',package = 'AER')
library(Formula)
library('plm')
gr<-subset(Grunfeld,firm %in% c('General Electric','Genernal Motors','IBM'))
pgr <- plm.data(gr, index = c("firm", "year"))
gr_pool <- plm(invest ~ value + capital, data = pgr,model = "pooling")
gr_fe <- plm(invest ~ value + capital, data = pgr,model = "within")
summary(gr_fe)
pFtest(gr_fe,gr_pool)
gr_re<-plm(invest~value+capital,data=pgr,model='random',random.method = 'walhus')
summary(gr_re)
plmtest(gr_pool)
phtest(gr_re,gr_fe)
data("EmplUK", package = "plm")
form<-log(emp)~log(wage)+log(capital)+log(output)
empl_ab <- pgmm(dynformula(form, list(2, 1, 0, 1)),
                data = EmplUK, index = c("firm", "year"),
                effect = "twoways", model = "twosteps",
                gmm.inst = ~ log(emp), lag.gmm = list(c(2, 99)))
summary(empl_ab)

#regression diagnostic
library(sandwich)
data('PublicSchools')
summary(PublicSchools)
ps<-na.omit(PublicSchools)
ps$Income<-ps$Income/1000
plot(Expenditure~Income,data=ps,ylim=c(230,830))
ps_lm<-lm(Expenditure~Income,data=ps)
abline(ps_lm)
id<-c(2,24,48)
text(ps[id,2:1],rownames(ps)[id],pos=1,xpd=TRUE)
plot(ps_lm,which=1:6)

#leverage and standardized residuals
ps_hat <- hatvalues(ps_lm)
plot(ps_hat)
abline(h = c(1, 3) * mean(ps_hat), col = 2)
id <- which(ps_hat > 3 * mean(ps_hat))
text(id, ps_hat[id], rownames(ps)[id], pos = 1, xpd = TRUE)
influence.measures(ps_lm)
which(ps_hat>3*mean(ps_hat))
summary(influence.measures(ps_lm))
plot(Expenditure ~ Income, data = ps, ylim = c(230, 830))
abline(ps_lm)
id <- which(apply(influence.measures(ps_lm)$is.inf, 1, any))
text(ps[id, 2:1], rownames(ps)[id], pos = 1, xpd = TRUE)
ps_noinf <- lm(Expenditure ~ Income, data = ps[-id,])
abline(ps_noinf, lty = 2)

#diagnostic tests
data("Journals")
journals <- Journals[, c("subs", "price")]
journals$citeprice <- Journals$price/Journals$citations
journals$age <- 2000 - Journals$foundingyear
#testing for heteroskedasticity
bptest(jour_lm)
bptest(jour_lm, ~ log(citeprice) + I(log(citeprice)^2),data = journals)
gqtest(jour_lm, order.by = ~ citeprice, data = journals)
resettest(jour_lm)
raintest(jour_lm, order.by = ~ age, data = journals)
harvtest(jour_lm, order.by = ~ age, data = journals)
#testing for autocorrelation
data("USMacroG")
consump1 <- dynlm(consumption ~ dpi + L(dpi),data = USMacroG)
dwtest(consump1)
Box.test(residuals(consump1),type='Ljung-Box')
bgtest(consump1)
#robust standard errors and tests
vcov(jour_lm)
coeftest(jour_lm,vcov=vcovHC)
t(sapply(c("const", "HC0", "HC1", "HC2", "HC3", "HC4"),function(x) sqrt(diag(vcovHC(jour_lm, type = x)))))
ps_lm<-lm(Expenditure~Income,data=ps)
ps_lm2<-lm(Expenditure~Income+I(Income^2),data=ps)
anova(ps_lm,ps_lm2)
waldtest(ps_lm,ps_lm2,vcov=vcovHC(ps_lm2,type='HC4'))
rbind(SE=sqrt(diag(vcov(consump1))),QS=sqrt(diag(kernHAC(consump1))),NW=sqrt(diag(NeweyWest(consump1))))
#resistent regression
data("OECDGrowth",package = 'AER')
solow_lm <- lm(log(gdp85/gdp60) ~ log(gdp60) +log(invest) + log(popgrowth + .05), data = OECDGrowth)
summary(solow_lm)
library(MASS)
solow_lts <- lqs(log(gdp85/gdp60) ~ log(gdp60) +log(invest) + log(popgrowth + .05), data = OECDGrowth,
                 psamp = 13, nsamp = "exact")
smallresid<-which(abs(residuals(solow_lts)/solow_lts$scale[2])<=2.5)
X<-model.matrix(solow_lm)[,-1]
Xcv<-cov.rob(X,nsamp = 'exact')
nohighlev<-which(sqrt(mahalanobis(X,Xcv$center,Xcv$cov))<=2.5)
goodobs<-unique(c(smallresid,nohighlev))
rownames(OECDGrowth)[-goodobs]
solow_rob<-update(solow_lm,subset=goodobs)
summary(solow_rob)
library(quantreg)
data(CPS1988)
cps_f<-log(wage)~experience+I(experience^2)+education
cps_lad<-rq(cps_f,data=CPS1988)
summary(cps_lad)
cps_rq<-rq(cps_f,tau = c(0.25,0.75),data=CPS1988)
summary(cps_rq)
cps_rq25<-rq(cps_f,tau=0.25,data = CPS1988)
cps_rq75<-rq(cps_f,tau=0.75,data = CPS1988)
anova(cps_rq25,cps_rq75)
anova(cps_rq25,cps_rq75,joint=FALSE)
cps_rqbig<-rq(cps_f,tau = seq(0.05,0.95,by=0.05))
cps_rqbigs<-summary(cps_rqbig)
plot(cps_rqbigs)


library(car)
library(survival)
library(AER)
data('SwissLabor',package = 'AER')
swiss_probit<-glm(participation~.+I(age^2),data = SwissLabor,family = binomial(link = 'probit'))
summary(swiss_probit)
plot(participation~age,data=SwissLabor,ylevels=2:1)
fav<-mean(dnorm(predict(swiss_probit,type='link')))
fav*coef(swiss_probit)
av<-colMeans(SwissLabor[,-c(1,7)])
av<-data.frame(rbind(swiss=av,foreign=av),foreign=factor(c('no','yes')))
av<-predict(swiss_probit,newdata = av,type='link')
av<-dnorm(av)
av['swiss']*coef(swiss_probit)[-7]
swiss_probit0<-update(swiss_probit,formula=.~1)
1-as.vector(logLik(swiss_probit)/logLik(swiss_probit0))
table(true=SwissLabor$participation,pred=round(fitted(swiss_probit)))
library('ROCR')
pred<-predict(fitted(swiss_probit),SwissLabor$participation)
plot(performance(pred,'acc'))
plot(performance(pred,'tpr','fpr'))
abline(0,1,lty=2)
deviance(swiss_probit)
sum(residuals(swiss_probit,type='deviance')^2)
sum(residuals(swiss_probit,type='pearson')^2)
coeftest(swiss_probit,vcov=sandwich)
data("MurderRates")
muder_logit<-glm(I(executions>0)~time+income+noncauc+lfp+southern,data=MurderRates,family=binomial)
coeftest(muder_logit)
murder_logit2<-glm(I(executions>0)~time+income+noncauc+lfp+southern,data=MurderRates,family = binomial,control = list(epsion=1e-15,maxit=50,trace=FALSE))
coeftest(murder_logit2)
table(I(MurderRates$executions>0),MurderRates%southern)
data('RecreationDemand')
rd_pois<-glm(trips~.,data=RecreationDemand,family = poisson)
coeftest(rd_pois)
logLik(rd_posis)
dispersiontest(rd_pois)
dispersiontest(rd_pois,trafo = 2)
library(MASS)
rd_nb<-glm.nb(trips~.data=RecreationDemand)
coeftest(rd_nb)
#robust standard errors
round(sqrt(rbind(diag(vcov(rd_pois)),diag(sandwich(rd_pois)))),digits = 3)
coeftest(rd_pois,vcov=sandwich)
round(sqrt(diag(vcovOPG(rd_pois))),3)
rbind(obs=table(RecreationDemand$trips)[1:10],exp=round(sapply(0:9,function(x)sum(x,fitted(rd_pois)))))
rd_zinb<-zeroinfl(trips~.|quality+income,data=RecreationDemand,dist='negbin')
summary(rd_zinb)
round(colSums(predict(rd_zinb,type='prob')[,1:10]))
rd_hurdle<-hurdle(trips~.|quality+income,data=RecreationDemand,dist='negbin')
summary(rd_hurdle)
round(colSums(predict(rd_hurdle,type='prob')[,1:10]))
#ɾ����ر���
data('Affairs',package = 'AER')
aff_tob<-tobit(Affairs~age+yearmarried+religiousness+occupation+rating,data=Affairs)
summary(aff_tob)
aff_tob2<-updata(aff_tob,right=4)
summary(aff_tob2)
linear.hypothesis(aff_tob,c('age=0','occupation=0'),vcov=sandwich)
#a semiparametric binary response model
SwissLabor$partnum<-as.numeric(SwissLabor$participation)-1
library('np')
swiss<-npindexbw(partnum~income+age+education+youngkids+oldkids+foreign+I(age^2),data=SwissLabor,method='kleinspady',nmulti=5)
summary(swiss_bw)
swiss_ks<-npindex(bws=swiss_bw,gradients=TRUE)
summary(swiss_ks)
table(ACtual=SwissLabor$participation,Predict=round(predict(swiss_probit,type='response')))
#multinomial responses
data('BankWages',package ='AER')
edcat<-factor(BankWages$education)
levels(edcat)[3:10]<-rep(c('14-15','16-18','19-21'),c(2,3,3))
tab<-xtabs(~edcat+job,data=BankWages)
prop.table(tab,1)
plot(job~edcat,data=BankWages,off=0)
library('nnet')
bank+mnl<-multinom(job~education+minority,data=BankWages,subset=gender=='male',trace=FALSE)
coeftest(bank_mnl)
#ordianal responses
library('MASS')
bank_polr<-polr(job~education+minority,data=BankWages,subset = gender=='male',Hess=TRUE)
coeftest(bank_polr)
AIC(bank_mnl)
data(UKNonDurables,package = 'AER')
plot(UKNonDurables)
tsp(UKNonDurables)
window(UKNonDurables,end=c(1956,4))
data(UKDriverDeaths)
plot(UKDriverDeaths)
lines(filter(UKDriverDeaths,c(1/2,rep(1,11),1/2)/12),col=2)
plot(rollapply(UKDriverDeaths,12,sd))
set.seed(1234)
x<-filter(rnorm(100),0.9,method = 'recursive')
#decomposition
dd_dec<-decompose(log(UKDriverDeaths))
dd_st1<-stl(log(UKDriverDeaths),s.window=13)
plot(dd_dec$trend,ylab = 'trend')
lines(dd_stl$time.series[,'trend'],lty=2,lwd=2)
dd_past<-window(UKDriverDeaths,end=c(1992,12))
dd_hw<-HoltWinters(dd_past)
dd_pred<-predict(dd_hw,n.ahead=24)
plot(dd_hw,dd_pred,ylim = range(UKDriverDeaths))
lines(UKDriverDeaths)
#classicail model based analysis
acf(x)
pacf(x)
ar(x)
nd<-window(log(UKNonDurables),end=c(1970,4))
acf(diff(nd),ylim=c(-1,1))
pacf(diff(nd),ylim=c(-1,1))
acf(diff(nd,4),ylim=c(-1,1))
pacf(diff(nd,4),ylim=c(-1,1))
