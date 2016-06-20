#getURL函数用于构造所需要的检索表达式
getUrl<-function(databsename,year,prov,item,type){
urlT=paste0("http://202.127.42.157/moazzys/",databsename,".aspx?year=",year,#input year
"&prov=",prov,#input province code (sep by ",")
"&item=",item,#input item code (sep by ",")
"&type=",type,
"&radio=1&order1=year_code&order2=prov_code&order3=item_code")
return(urlT)}
#这里用于定义所需要检索的一系列数据
databsename="nongqing_chanzhi"#数据库名称
year=paste0(2014:2015,collapse=",")#查询年份
prov="00,11,12,13,14,15,21,22,23,31,32,33,34,35,36,37,41,42,43,44,45,46,49,51,52,53,54,61,62,63,64,65"#查询的省份代码
item="1,2,3,4,5,6,7,8,9,10,11,12,13,14"#查询的项目代码
type="1,2"#查询的类型代码
##下面的代码用于抓取数据
urlT=getUrl(databsename,year,prov,item,type)#运行函数得到查询所需要的url
library(rvest)#载入所需要的rvest包
google <- read_html(urlT) %>%#读取url，下载原始数据
html_nodes(xpath = "//span[@id='ContentPlaceHolder1_lbldata']") #定位表格数据所在
years<-html_nodes(google,"b")%>%html_nodes("font")%>%html_text()#提取年份
charts=html_nodes(google,"tr")%>%html_nodes("table")%>%html_nodes("table")%>%html_table(fill=T,header=T)#提取表格主体
names<-html_nodes(google,"tr")%>%html_nodes("table")%>%html_nodes("b")%>%html_text()#提取省份名称
names(charts)<-names
#下面的代码用于整理抓到的数据
temp=data.frame()#初始化
res=data.frame()#初始化
for (j in 1:length(charts)){#将抓到的表格list转化成data.frame，并将地理位置信息加入
    temp=cbind(charts[[j]],names(charts[j]))
    res<-rbind(temp,res)}
colnames(res)<-c("type","quantity","none","place")
res<-res[,-3]#删除无用的一列
res<-subset(res,res$type!="")#删除空白行
a=length(unique(res$type))*length(unique(res$place))#这一步比较奇妙，我们还需要把时间变量加入
#这一步是用于加入时间变量的
res$year=rev(rep(years[1]:years[length(years)],1,each = a))#加入年份变量
