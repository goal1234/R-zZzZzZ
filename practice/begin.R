
setwd('F:/���մ���/���/data')

if(FALSE){
  # -û�ж����ַ���- #
  library(haven)
  filename <- 'b.sas7bdat'
  dataset <- read_sas(filename, encoding = 'UTF-8')
  View(dataset)
  
  # -��SAS�б�ѹ���˳��ֱ���- #
  library(sas7bdat)
  dataset1 <- read.sas7bdat(filename)
}

library(readr)

# ---����������--- #
filename_a <- 'a.csv'
filename_b <- 'b.csv'
filename_c <- 'c.csv'
data_1 <- read_csv(filename)
data_a <- read.csv(filename_a, stringsAsFactors = F)
data_b <- read.csv(filename_b, stringsAsFactors = F)
data_c <- read.csv(filename_c, stringsAsFactors = F)

datasets <- rbind(data_a, data_b, data_c)


# ---���һ��͸�ӱ�--- #

library(reshape2)

effect <- function(datasets){
  datavar <- datasets
  a <- aggregate(datavar$CLIENT_ID,by=list(datavar$effect, datavar$upprd),length)
  result <<- dcast(a,Group.1~Group.2)
}

#��Ҫ���ӱߺ������

data_dpd1 <- subset(datasets, dpd == 1)
data_dpd2 <- subset(datasets, dpd %in% c(2,3))
data_dpd3 <- subset(datasets, dpd %in% c(4:6))
data_dpd4 <- subset(datasets, dpd %in% c(7:1000))

IsEffectAll <- effect(datasets = datasets)
IsEffectdpd1 <- effect(datasets = data_dpd1)
IsEffectdpd2 <- effect(datasets = data_dpd2)
IsEffectdpd3 <- effect(datasets = data_dpd3)
IsEffectdpd4 <- effect(datasets = data_dpd4)


# ��Ӧȫ����Ч����� #
AllEffect <- subset(datasets, effect == 'Y')


reason_noget <- subset(AllEffect, reason == "δ�ἰ����ԭ��")
reason_incomereduce <- subset(AllEffect, reason == "�������")
reason_forget <- subset(AllEffect, reason == "���ǻ���/����?/td>")



#---forgetƥ��Բ�Ӧ����һ��͸���Ǳ����ظ��˺ܶ�� ---#

# -ƥ��-#


  pattern = "��"
  p1 <- grep(pattern,reason_forget$REMARK2)

  pattern = "����"
  p2 <- grep(pattern,reason_forget$REMARK2)


  pattern ="����"
  pattern1 = "����" 
  a <- grep(pattern,reason_forget$REMARK2)
  b <- grep(pattern1,reason_forget$REMARK2)
  p3 <- intersect(a,b)

  pattern ="ʱ��"
  pattern1 = "����" 
  a <- grep(pattern,reason_forget$REMARK2)
  b <- grep(pattern1,reason_forget$REMARK2)
  p4 <- intersect(a,b)

  
  pattern ="������"
  pattern1 = "��" 
  a <- grep(pattern,reason_forget$REMARK2)
  b <- grep(pattern1,reason_forget$REMARK2)
  p5 <- intersect(a,b)
  
  pattern ="��"
  pattern1 = "�Թ�" 
  a <- grep(pattern,reason_forget$REMARK2)
  b <- grep(pattern1,reason_forget$REMARK2)
  p6 <- intersect(a,b)
  
  pattern ="��"
  pattern1 = "��"
  pattern2 = "��"
  a <- grep(pattern,reason_forget$REMARK2)
  b <- grep(pattern1,reason_forget$REMARK2)
  c <- grep(pattern2, reason_forget$REMARK2)
  p7 <- intersect(a, b)
  p7 <- intersect(p7, c)
  
  
  pattern ="��"
  pattern1 = "�쳣"
  pattern2 = "���"
  pattern3 = "����"
  pattern4 = "��"
  pattern5 = "��ʧ"
  pattern6 = "����"
  pattern7 = "����"
  
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

  # -1����ƥ�䵽�ˣ�0Ϊû��ƥ�䵽
  reason_forget$match <- 0
  reason_forget$match[result_match] <- 1

  #-����һ���ۺ�-#

  a <- aggregate(reason_forget$CLIENT_ID,by=list(reason_forget$math, 
                                                 reason_forget$upprd),length)
  result <- dcast(a, Group.1~Group.2)
  
  
  #-��ά��Ҫɸѡ֮�����ƴ��...-#
  a <- aggregate(reason_forget$CLIENT_ID,by=list(reason_forget$USER_NAME, 
                                                 reason_forget$upprd,reason_forget$math),length)
  
  # - ��ά���ݲ���һ��tidy����,��Ҫƴ��
  library(tidyr)

  

  
  #----------------------δ�ἰ����ԭ��----------------------#
  
  p <- c('����', "����","��Ч","���",'���ս�','����','����','Ҫ��',
         '�տ�', '����','����','��Ӫ','ûǮ','��Ǯ','����','ʧҵ','�Ʋ�','��ƭ',
         '������','����','��·','ʧ��','��ִ��','��˾����','��Ǯ','������',
         '��ְ','��������Ǯ','����','���','�־�','������','��ͥ����','�е�����ծ��',
         '����ﻹǮ','�߶ծ','Ƿ��ܶ�','Ƿ�ܶ��','���','�߸�ծ','��',
         '����','Ͷ��','����˽��','������','��ץ','��Ʒ','�����','����',
         'ȥ��','��װ','αð','��թ','��ƭ')
  
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
  
  
  # ---------------------�������------------------------------#
  p <- c('����','����','��Ч','���','���ս�','����','����','Ҫ��','����',
         '����','����','��Ӫ','ûǮ','��Ǯ','����','ʧҵ','�Ʋ�','��ƭ',
         '������','����','��·','ʧ��','��ִ��','��˾����','��Ǯ','������','��ְ',
         '��������Ǯ')
  
  get_p <- lapply(p,noget)
  result_match <- Reduce(union, get_p)
  
  pattern ="���̿�"
  pattern1 = "δ"
  pattern2 = "û"
  a <- grep(pattern,reason_incomereduce$REMARK2)
  b <- grep(pattern1,reason_incomereduce$REMARK2)
  c <- grep(pattern2, reason_incomereduce$REMARK2)
  p <- union(b, c)
  p <- intersect(a, p)
  
  result_match <- union(result_match, p)
  
  reason_incomereduce$match <- 0
  reason_incomereduce$match[result_match] <- 1  
  
  
  
  
  
  
  