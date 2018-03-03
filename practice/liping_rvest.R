# rvest抓取猎聘网
# ---

url <- "https://www.liepin.com/zhaopin/?pubTime=&ckid=2db88e5512a1eb5d&fromSearchBtn=2&compkind=&isAnalysis=&init=-1&searchType=1&flushckid=1&dqs=010010030&industryType=&jobKind=&sortFlag=15&industries=&salary=&compscale=&clean_condition=&key=%E6%95%B0%E6%8D%AE%E5%88%86%E6%9E%90&headckid=2a5c16f2da2e0d7a&d_pageSize=40&siTag=ZFDYQyfloRvvhTxLnVV_Qg%7EF5FSJAXvyHmQyODXqGxdVw&d_headId=450b7655c931bbd70769a2e56a4c4cf8&d_ckId=fb322235aa6913eab3b6eea4cdae7e93&d_sfrom=search_prime&d_curPage=0"
page <- read_html(url)

# 先写父节点，再写子节点，中间用空格隔开，或者 >隔开
#节点是标题时，需要用“，”逗号与前面隔开
position<-page%>%html_nodes('ul.sojob_list div.sojob-item-main div.job-info,h3 a')%>%html_text(trim =TRUE)
position
position<- position[-41] #删除第41个


# --提取职位连接
link<- page %>% html_nodes('ul.sojob_list div.job-info,h3 a')%>%html_attrs()

#也可以写成'ul.sojob_list div.sojob-item-main div.job-info,h3 a
link[1]#读取数据，规定编码
# position<-page %>% html_nodes("div.pages_content") %>% html_text()
link1<-c(1:length(link))#初始化一个和link长度相等的link1

for(i in 1:length(link)) link1[i]<-link[[i]][1]

link1#查看link1
link2<-link1[-41]#删除最后一行

link2#查看link2
link<-link2#将link2重新赋值给link


# ---提取薪水
salary <- page %>% html_nodes('span.text-warning') %>% html_text()
salary

# --里面的<p class ="condition"> <span>
#---提取工作地点
experience <- page %>% html_nodes('p.condition span') %>% html_text()
experience

# --- 提取教育背景
edu <- page %>% html_nodes('span.edu') %>% html_text()
edu

# ---工作地址
place <- page %>% html_nodes('a.area') %>% html_text()
place

# --工作经验
experience <- page %>% html_nodes('p.condition span') %>% html_text()
experience


dt<-matrix(,length(experience)/3,3)
colnames(dt)<-c("sal","ed","exp")

for(n in 1:3) #n代表第n列
{
  j <- 1
  i <- n
  while(i<=length(experience))
  {
    dt[j,n]=experience[i]
    j<-j+1
    i <- i+3
  }
}
dt


# --合并数据
Alldata<-matrix(,40,6)
Alldata[,1]<-position
Alldata[,2]<-dt[,1]
Alldata[,3]<-dt[,2]
Alldata[,4]<-dt[,3]
Alldata[,5]<-place
Alldata[,6]<-link
colnames(Alldata)<-c("职位","薪水","学历","经验","工作地点","链接") #给列命名

head(Alldata) #查看Alldata数据前6行
result_liping <- Alldata
