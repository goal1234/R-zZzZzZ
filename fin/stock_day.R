  setwd('E:/水果诱惑/stock data')
  library(magrittr)
  #---------------------------------------读入
  a<-dir('E:/水果诱惑/stock data')
  as.list(a)
  stock <-read.csv(a[[1]])
  as.data.frame(stock)
  stock <- stock[order(stock['date']),]
  
  stock_100<- list()
  for(i in 1:100){
    stock <-read.csv(a[[i]])
    as.data.frame(stock)
    stock_100[[i]] <- stock[order(stock['date']),]}
  model_1<-stock %>% lm(close~PE_TTM+PS_TTM+PC_TTM+PB+0,data=.)
  stock_1 <- sample(stock,1000,replace = FALSE)
  model_1.1 <- stock %>% lm(close~PE_TTM+PS_TTM+PC_TTM+0,data=.)
  
  result <-predict(model_1.1,stock_100[[2]]) #这个有问题
  rmse <- function(x,t){
    sqrt(mean(sum((x-t)^2)))
  }
  rmse(result,stock_100[[2]]$close)
  
  #-------------------------------------------------cor_list-------------------------
  cor_list <- vector()
  for(i in 1:length(stock_100)){
    cor_list[i] <-cor(stock_100[[i]]$close,stock_100[[i]]$volume,method = 'kendall')
  }
  
  cor_list_ps <- vector()
  for(i in 1:length(stock_100)){
    cor_list_ps[i] <-cor(stock_100[[i]]$close,stock_100[[i]]$PS_TTM,method ='kendall')
  }