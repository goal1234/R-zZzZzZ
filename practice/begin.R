
setwd('F:/催收代码/输出/data')

if(FALSE){
  # -没有读入字符串- #
  library(haven)
  filename <- 'b.sas7bdat'
  dataset <- read_sas(filename, encoding = 'UTF-8')
  View(dataset)
  
  # -在SAS中被压缩了出现报错- #
  library(sas7bdat)
  dataset1 <- read.sas7bdat(filename)
}

library(readr)

# ---还是正常点--- #
filename_a <- 'a.csv'
filename_b <- 'b.csv'
filename_c <- 'c.csv'
data_1 <- read_csv(filename)
data_a <- read.csv(filename_a, stringsAsFactors = F)
data_b <- read.csv(filename_b, stringsAsFactors = F)
data_c <- read.csv(filename_c, stringsAsFactors = F)

datasets <- rbind(data_a, data_b, data_c)


# ---输出一个透视表--- #

library(reshape2)

effect <- function(datasets){
  datavar <- datasets
  a <- aggregate(datavar$CLIENT_ID,by=list(datavar$effect, datavar$upprd),length)
  result <<- dcast(a,Group.1~Group.2)
}

#需要添加边后再输出

data_dpd1 <- subset(datasets, dpd == 1)
data_dpd2 <- subset(datasets, dpd %in% c(2,3))
data_dpd3 <- subset(datasets, dpd %in% c(4:6))
data_dpd4 <- subset(datasets, dpd %in% c(7:1000))

IsEffectAll <- effect(datasets = datasets)
IsEffectdpd1 <- effect(datasets = data_dpd1)
IsEffectdpd2 <- effect(datasets = data_dpd2)
IsEffectdpd3 <- effect(datasets = data_dpd3)
IsEffectdpd4 <- effect(datasets = data_dpd4)


# 对应全部有效这个表 #
AllEffect <- subset(datasets, effect == 'Y')


reason_noget <- subset(AllEffect, reason == "未提及逾期原因")
reason_incomereduce <- subset(AllEffect, reason == "收入减少")
reason_forget <- subset(AllEffect, reason == "忘记还款/还款?/td>")



#---forget匹配对策应该是一个透视是表，重复了很多次 ---#

# -匹配-#


  pattern = "忘"
  p1 <- grep(pattern,reason_forget$REMARK2)

  pattern = "代扣"
  p2 <- grep(pattern,reason_forget$REMARK2)


  pattern ="不在"
  pattern1 = "本地" 
  a <- grep(pattern,reason_forget$REMARK2)
  b <- grep(pattern1,reason_forget$REMARK2)
  p3 <- intersect(a,b)

  pattern ="时间"
  pattern1 = "还款" 
  a <- grep(pattern,reason_forget$REMARK2)
  b <- grep(pattern1,reason_forget$REMARK2)
  p4 <- intersect(a,b)

  
  pattern ="还款日"
  pattern1 = "存款不" 
  a <- grep(pattern,reason_forget$REMARK2)
  b <- grep(pattern1,reason_forget$REMARK2)
  p5 <- intersect(a,b)
  
  pattern ="存"
  pattern1 = "对公" 
  a <- grep(pattern,reason_forget$REMARK2)
  b <- grep(pattern1,reason_forget$REMARK2)
  p6 <- intersect(a,b)
  
  pattern ="存"
  pattern1 = "错"
  pattern2 = "卡"
  a <- grep(pattern,reason_forget$REMARK2)
  b <- grep(pattern1,reason_forget$REMARK2)
  c <- grep(pattern2, reason_forget$REMARK2)
  p7 <- intersect(a, b)
  p7 <- intersect(p7, c)
  
  
  pattern ="卡"
  pattern1 = "异常"
  pattern2 = "变更"
  pattern3 = "冻结"
  pattern4 = "丢"
  pattern5 = "挂失"
  pattern6 = "销户"
  pattern7 = "更换"
  
  a <- grep(pattern,reason_forget$REMARK2)
  
  b1<- grep(pattern1,reason_forget$REMARK2)
  b2 <- grep(pattern2, reason_forget$REMARK2)
  b3 <- grep(pattern3, reason_forget$REMARK2)
  b4 <- grep(pattern4, reason_forget$REMARK2)
  b5 <- grep(pattern5, reason_forget$REMARK2)
  b6 <- grep(pattern6, reason_forget$REMARK2)
  b7 <- grep(pattern7, reason_forget$REMARK2)
  
  b <- Reduce(union,list(b1,b2,b3,b4,b5,b6,b7))
  p8 <- intersect(a,b)
  
  
  result_match <- Reduce(union, list(p1, p2, p3, p4, p5, p6, p7, p8))

  # -1代表匹配到了，0为没有匹配到
  reason_forget$match <- 0
  reason_forget$match[result_match] <- 1

  #-进行一个聚合-#

  a <- aggregate(reason_forget$CLIENT_ID,by=list(reason_forget$math, 
                                                 reason_forget$upprd),length)
  result <- dcast(a, Group.1~Group.2)
  
  
  #-多维需要筛选之后进行拼接...-#
  a <- aggregate(reason_forget$CLIENT_ID,by=list(reason_forget$USER_NAME, 
                                                 reason_forget$upprd,reason_forget$math),length)
  
  # - 三维数据不是一个tidy数据,需要拼接
  library(tidyr)

  

  
  #----------------------未提及逾期原因----------------------#
  
  p <- c('工资', "奖金","绩效","提成",'年终奖','货款','报销','要账',
         '收款', '贷款','生意','经营','没钱','有钱','亏损','失业','破产','被骗',
         '被起诉','冻结','跑路','失联','被执行','公司倒闭','借钱','被开除',
         '离职','做生意赔钱','离异','离婚','分居','还贷款','家庭困难','承担家人债务',
         '绑家里还钱','高额负债','欠款很多','欠很多家','多家','高负债','忘',
         '代扣','投诉','帮别人借的','被拘留','被抓','毒品','抽大烟','局子',
         '去世','包装','伪冒','欺诈','欺骗')
  
  noget <- function(word){
    pattern = word
    a <<- grep(pattern, reason_noget$REMARK2)
  }
  
  get_p <- lapply(p,noget)
  
  get_p[[i]]
  
  for(i in 1:length(p)){
    eval(parse(text=paste(paste('p',i,sep=''), '= get_p[[',i,']]')))
  }
  result_match <- Reduce(union, get_p)
  reason_noget$match <- 0
  reason_noget$match[result_match] <- 1  
  
  
  # ---------------------收入减少------------------------------#
  p <- c('工资','奖金','绩效','提成','年终奖','货款','报销','要账','收账',
         '贷款','生意','经营','没钱','有钱','亏损','失业','破产','被骗',
         '被起诉','冻结','跑路','失联','被执行','公司倒闭','借钱','被开除','离职',
         '做生意赔钱')
  
  get_p <- lapply(p,noget)
  result_match <- Reduce(union, get_p)
  
  pattern ="工程款"
  pattern1 = "未"
  pattern2 = "没"
  a <- grep(pattern,reason_incomereduce$REMARK2)
  b <- grep(pattern1,reason_incomereduce$REMARK2)
  c <- grep(pattern2, reason_incomereduce$REMARK2)
  p <- union(b, c)
  p <- intersect(a, p)
  
  result_match <- union(result_match, p)
  
  reason_incomereduce$match <- 0
  reason_incomereduce$match[result_match] <- 1  
  
  
  
  
  
  
  