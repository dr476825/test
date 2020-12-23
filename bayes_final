library(dplyr)
library(rgdal)
library(ggplot2)
library(tidyr)
library(spdep)

aa<-readOGR(dsn="C:/Users/dr476/Desktop/haopinn/109_1bayes/mapdata202010230156",layer="VILLAGE_MOI_1091016",encoding = "UTF-8",use_iconv=TRUE)
aa<-aa[aa@data$COUNTYNAME=="臺北市"&(aa$TOWNNAME=="大同區"|aa$TOWNNAME=="中正區"|aa$TOWNNAME=="中山區"|aa$TOWNNAME=="大安區"),]
aa$li<-paste(aa$COUNTYNAME,aa$TOWNNAME,aa@data$VILLNAME,sep="")
rb<-read.table("C:/Users/dr476/Desktop/haopinn/109_1bayes/rob_coord.csv",sep=",",header=TRUE)
rb$li<-substr(rb$address,1,9)
rb$month<-substr(rb$date,1,5)

fqq<-rb %>% mutate(qui=substr(li,4,6)) %>% filter(qui%in%c("大同區","中正區","大安區","中山區")) %>% group_by(li,month) %>% count %>% spread(month,n) 
fqq[is.na(fqq)]<-0
# fqq will be the working data frame

# get neighbor of aa
aa.nb<-poly2nb(aa)
# create adj. table of aa
aa_value<-paste(aa$COUNTYNAME,aa$TOWNNAME,aa$VILLNAME,sep="") %>% data.frame(li=.) %>% left_join(.,fqq,by="li")
aa_value[is.na(aa_value)]<-0
aa_value$adj<-aa.nb %>% lapply(.,function(x){paste(x,collapse = ",")}) %>% unlist
aa_value$num<-aa.nb %>% lapply(.,function(x){length(x)}) %>% unlist
adj<-c(aa_value$adj[1],paste(paste("\n",aa_value$adj[2:151],sep=""),sep=""))
adj<-aa_value$adj
adj<-unlist(aa.nb)

num<-aa_value$num
li_index<-c(1:151)
O<-aa_value[,c(2:61)] %>% rowSums()
N<-151
sumNumNeigh<-sum(num)
parameters<-c("b")
data<-list(O = as.numeric(O),
           N = as.numeric(N),
           num = as.numeric(num),
           sumNumNeigh = as.numeric(sumNumNeigh),
           adj=as.character(adj),
           li_index=as.numeric(li_index))

M<-rep(10000,151)
data2<-list("N","O","num","sumNumNeigh","adj","li_index")
results<-R2WinBUGS::bugs(data2,inits=NULL,model.file = "C:/Users/dr476/Desktop/model_car.txt",parameters,n.chains = 1,n.iter = 60000,n.burnin = 10000,n.thin = 15,debug=TRUE,bugs.directory = "C:/Users/dr476/Desktop/haopinn/109_1bayes/winbugs14_full_patched/WinBUGS14")
