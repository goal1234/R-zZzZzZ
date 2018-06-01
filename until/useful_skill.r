
#Reduce()函数
v1 <- c("geneA","geneB",""...)
v2 <- c("geneA","geneC",""...)
v3 <- c("geneD","geneE",""...)
v4 <- c("geneA","geneE",""...)
v5 <- c("geneB","geneC",""...)

Reduce(intersect, list(a, b, c,d))

Reduce(`*`, x=list(5,4,3,2), accumulate=TRUE)

i2 <- seq(0,100,by=2)
i3 <- seq(0,100,by=3)
i5 <- seq(0,100,by=5)
Reduce(intersect, x=list(i2,i3,i5), accumulate=TRUE)

L <- list(a, b, c, d)
u <- unlist(lapply(L, unique))

tab <- table(u)
names(tab[tab>1])
sort(unique(u[duplicated(u)]))

Lst<-list(0)
for (i in (1:5)){
Lst[i]<-list(matrix(rep(i,8),nrow=2))

}
Reduce('+',Lst)

#从右向左运算
Reduce("-",a,right=T)

#mapReduce提供了按分组变量运行指定函数的方式。
dt <- data.frame(sex = rep(c('m','f'),5), age  = c(1:10),w=c(10:1))

mapReduce(sex, mean(age), mean(w), sd(age), data = dt)

#_____________________________something useful__________________________________#
str()
data(iris)
dput(iris$Petal.Length)  #变为R代码进行输出的格式
head()
tail()
paste()
with()
within()  #将会改变对象
df <- data.frame(A = runif(10), B= rnorm(10))
A <- 1:10
with(df, A+B)
within(df, C<-rpois(10, lambda = 2))
head(df)

#___________________________reshape____________________________#
library(reshape)
df$z <- c(1,1,2,2,2)
recast(df, z~., id.var = 'z')

#____________________new.env()________________#
zz <- new.env()
zz$foo <- c(1:5)
changer <- function(blah){
      blah$foo <-5
}
changer(zz)

#________________foreach()____________#
list_powers <- foreach(i = 1:100) %do% {
  lp <- x[i]^i
  return (lp)
}
#需要加载包
require(foreach)
require(doMC)

# 1-core
system.time(m <- foreach(i=1:100) %dopar%
  matrix(rnorm(1000*1000), ncol=5000) )

system.time(m <- foreach(i=1:100) %do%
    matrix(rnorm(1000*1000), ncol=5000) )

# 2-core
registerDoMC(cores=2)
system.time(m <- foreach(i=1:100) %dopar%
    matrix(rnorm(1000*1000), ncol=5000) )

system.time(m <- foreach(i=1:100) %do%
    matrix(rnorm(1000*1000), ncol=5000) )


#-------------------##########

sales <- expand.grid(country = c('USA', 'UK', 'FR'),
                     product = c(1, 2, 3))
sales$revenue <- rnorm(dim(sales)[1], mean=100, sd=10)

#transform()

## transform currency to euros
usd2eur <- 1.434
transform(sales, euro = revenue * usd2eur)

subset(sales,
       country == 'USA' & product %in% c(1, 2),
       select = c('product', 'revenue'))

       ##  recast the previous subset() expression in SQL
       sqldf('SELECT product, revenue FROM sales \
              WHERE country = "USA" \
              AND product IN (1,2)')

#两个有用的包plyr还有 Data Mainpulation in R

#____________easy loop_________#
for(i in 1:nrow(df)){
  if (df$column[i] == x) {
    df$column2[i] <- y
    or any other similiar code
  }
}

#效率很慢
#进行R的风格编码
df$column2[df$column1 == x] <- y

#__________________#
foo <- list()

foo[[1]] <- data.frame(a=1:5, b=11:15)
foo[[2]] <- data.frame(a=101:105, b=111:115)
foo[[3]] <- data.frame(a=200:210, b=300:310)

do.call(rbind, foo)

#_______________停止一个错误__________________________#
if (bad.condition) stop("message")
stopfifnot(!bad.condition)

traceback()
options(error=recover)

#________________sapply()___________________#
sapply(rnorm(100, 0, 1), round)
sapply(rnorm(100, 0, 1), function(x) {round(x, 2)})  #传入参数，有的时候是不能传参

#——————————use apply, sapply, lapply, tapply, and do.call! ————————#
N = 10000
l = numeric()
for (i in seq(1:N)) {
    sim <- rnorm(1, 0, 1)
    l <- rbind(l, sim)
}

#更好版本如下
N = 10000
l = numeric(N)
for (i in seq(1:N)) {
    sim <- rnorm(1, 0, 1)
    l[i] <- sim
}

#___________字符传入一个公式中____________#
a #一个有四个值的公式
(f = paste("y~", paste(names(a)[1:4], collapse = "+")))
lm(formula(f), data = a)

#______直接进行assign______#
condition <- runif(1) > 0.5
if(condition) x<- 1 else x<-2
x<- if(condition) 1 else 2

x<-ifelse(condition, 1, 2)
unclass()

#gmodel这个包里面的CrossTable()像SAS一帮扔东西

system()

#______将factor in to a numeric____#
#以前是factor 变成字符然后变成numeric

old.var <- as.numeric(levels(old.var))[as.numeric(old.var)]

#_____________cut______________#

#_____________expand.grid()______________#
interArray=function(X){
    n=ncol(X)
    ind=expand.grid(1:n,1:n)
    return(X[,ind[,1]]*X[,ind[,2]])
}

interArray(x)

#---eval()和parse()---#
NY.Capital <- 'Albany'
state <- 'NY'
parameter <- 'Capital'
eval(parse(text=paste(state, parameter, sep='.')))


#--------------------#
d = '~/R Code/Library/'

files = list.files(d,'.r$')

for (f in files){
      if (!(f == 'mysource.r' )) {
          print(paste('Sourcing',f)) source(paste(d,f,sep=''))
           }
}

#---aggregate()---#
newdf <- aggregate( cbind(rt, acc) ~ x + y + subj, olddf, mean )

ps <-readline(prompt="get the password in ")
mypass <- system("zenity --entry --hide-text",intern=TRUE)

#____tapply___#
df[df$col > something , c('col1', 'col3')]
tapply(df$col_value, df$col_factor, function)
