#prepare for data--生产力投入
year=paste0(1949:2015,collapse=",")#select time period
prov=as.character(c(00,11,12,13,14,15,21,22,23,31,32,33,34,35,36,37,41,42,43,44,45,46,49,51,52,53,54,61,62,63,64,65))
prov[1]<-"00"#constuct province code
item="1,2,3,4,5,6,7,8,9,10,11,12,13,14"#item code construction
type="1"
#产值
item="1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21"#item code construction
type="1,2"
get.agridata<-function(x){
urlT=getUrl(databsename,year,prov[x],item,type)
google <- read_html(urlT) %>%
  html_nodes(xpath = "//span[@id='ContentPlaceHolder1_lbldata']") 
years<-html_nodes(google,"b")%>%html_nodes("font")%>%html_text()#years
charts=html_nodes(google,"tr")%>%html_nodes("table")%>%html_nodes("table")%>%html_table(fill=T,header=T)
names<-html_nodes(google,"tr")%>%html_nodes("table")%>%html_nodes("b")%>%html_text()
names(charts)<-names
temp=data.frame()
res=data.frame()
for (j in 1:length(charts)){
  temp=cbind(charts[[j]],names(charts[j]))
  res<-rbind(temp,res)}
colnames(res)<-c("type","quantity","none","place")
res<-res[,-3]
res<-subset(res,res$type!="")
a=length(unique(res$type))*length(unique(res$place))
res$year=rev(rep(years[1]:years[length(years)],1,each = a))
return(res)}
nongqing_chanzhi <- data.frame()
for (m in 1:length(prov)){
  cat("R is crawling data in", prov[m], 
      as.character(as.POSIXlt(Sys.time(), "Asia/Shanghai")),"\n")  
  temp=get.agridata(m)
  
  nongqing_chanzhi<-rbind(temp,nongqing_chanzhi)}
colnames(nongqing_chanzhi)<-c("type","quantity","na","place","year")
write.csv(nongqing_chanzhi,"chanzhi.csv")
huafei<-subset(agridatabase,type=="农药(吨)" )
chanzhi<-subset(nongqing_chanzhi,type=="农林牧渔业" )

ggplot(huafei,aes(x=year,y=quantity))+geom_line()+facet_wrap(~place,scales="free")+ ggtitle('各省级行政区化肥施用量')
ggplot(chanzhi,aes(x=year,y=quantity))+geom_line()+facet_wrap(~place,scales="free")+ ggtitle('各省级行政区农林牧渔产值')
