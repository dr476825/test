library(rgdal)
library(splancs)
library(GISTools)
library(spdep)
library(ggplot2)
library(dbscan)
sch<-readOGR("C:/Users/dr476825/Desktop/final",layer="school",encoding="utf8")
tpebond<-read.csv("C:/Users/dr476825/Desktop/final/tpe_sqr_bnd.csv")
tpebond2<-tpebond[,c(2,3)]
colnames(tpebond2)<-c("x","y")
tpebondp<-as.points(tpebond2)
attach(sch@data)
jh<-(sch[TYPE=="國民中學"|TYPE=="附設國民中學",])
jhes<-(sch[TYPE=="國民中學"|TYPE=="附設國民中學"|TYPE=="國民小學"|TYPE=="附設國民小學",])
sh<-(sch[TYPE=="高級中等學校",])
dseq<-seq(0,3000,100)
jhcoords<-jh@coords
shcoords<-sh@coords
jhsh<-Fhat(jhcoords,shcoords,dseq)
plot(jhsh)
nsim=999
res<-c()
for(i in 1:nsim) {
  tpebondran<-csr(as.points(tpebondp), nrow(shcoords))
  tpe.F.CSR<-Fhat(jhcoords,tpebondran, dseq)
  res<-cbind(res,tpe.F.CSR)
}
upper<-c()
lower<-c()
for(i in 1:nrow(res)){
upper[i]<-quantile(res[i,],.975)
lower[i]<-quantile(res[i,],.025)
}
plot(jhsh,xlab="距離",ylab="Bivariate F function",main="以國中為中心搜尋最近高中的Bivariate F function")
lines(upper, col="red", lty=3)
lines(lower, col="blue", lty=3)
legend("bottomright", legend = c("模擬平均距離信賴區間上界(Clustering)", "模擬平均距離信賴區間下界(Dispersion)", "obs    信賴係數=0.95"), bty = "n",lwd = 2, cex = 1.2, col = c("red", "blue", "black"), lty = c(3, 3, 1), pch = c(NA, NA, 1))

jhp<-as.points(jh@coords)
a<-kNNdist(jhp,3)
kNNdistplot(jhp, k = 3)
dbsres <- dbscan(jhp, eps = 1300, minPts = 3)
polymap(tpebond2, col="lightgray")
pointmap(jhp, col = dbsres$cluster +1 , pch = dbsres$cluster +1 , add=T)

subt<-readOGR("C:/Users/dr476825/Desktop/final",layer="tpe_subtown",encoding="utf8")
summary(subt)
subtcoords<-cbind(subt$XCOORD,subt$YCOORD)
jhessubt.count<-poly.counts(jhes,subt)
subt$jhessubt.count<-jhessubt.count
subt$jhessubt.count.den<-jhessubt.count/subt$Shape_Area
tpe_n<-dnearneigh(subtcoords, d1=0, d2=5000)
tpe_nb_w<- nb2listw(tpe_n)
LISA.jhessubt.count <- localmoran(jhessubt.count,tpe_nb_w)
LISA.jhessubt.count.den <- localmoran(subt$jhessubt.count.den,tpe_nb_w)
chk<-jhessubt.count - mean(jhessubt.count)
zi<- LISA.jhessubt.count[,4]
quadrant <- vector(mode="numeric",length=nrow(LISA.jhessubt.count))
quadrant[chk>0 & zi>0] <- 1 # H-H
quadrant[chk<0 & zi>0] <- 2 # L-L
quadrant[chk>0 & zi<0] <- 3 # H-L
quadrant[chk<0 & zi<0] <- 4 # L-H
signif <- 0.1
quadrant[LISA.jhessubt.count[,5]> signif] <- 5
colors <- c("red", "blue", "lightpink", "skyblue2", rgb(.95, .95, .95))
plot(subt, border="grey", col=colors[quadrant], main = "LISA Cluster Map: # Junior high school and Elementary school in Taipei subtowns")
legend("bottomright",legend=c("High-High","Low-Low","High-Low","Low-High"), fill=colors,bty="n",cex=0.7,y.intersp=1,x.intersp=1)

chk<-subt$jhessubt.count.den - mean(subt$jhessubt.count.den)
zi<- LISA.jhessubt.count.den[,4]
quadrant <- vector(mode="numeric",length=nrow(LISA.jhessubt.count.den))
quadrant[chk>0 & zi>0] <- 1 # H-H
quadrant[chk<0 & zi>0] <- 2 # L-L
quadrant[chk>0 & zi<0] <- 3 # H-L
quadrant[chk<0 & zi<0] <- 4 # L-H
quadrant[LISA.jhessubt.count.den[,5]> signif] <- 5
colors <- c("red", "blue", "lightpink", "skyblue2", rgb(.95, .95, .95))
plot(subt, border="grey", col=colors[quadrant], main = "LISA Cluster Map: Density of Junior school and Elementary school in subtowns of Taipei")
legend("bottomright",legend=c("High-High","Low-Low","High-Low","Low-High"), fill=colors,bty="n",cex=0.7,y.intersp=1,x.intersp=1)
	






subtnb<-poly2nb(subt,queen=TRUE)
subtnbm <-nb2mat(subtnb, style="W")
subtnbw <- nb2listw(subtnb, style = "W")
subtnbw.mat <-nb2mat(subtnb, style="W")
census<-subt$Sum_CENSUS/10000
slm <- lagsarlm(subt$jhessubt.count~census,listw=subtnbw)
summary(slm)
names(slm)
cvec <- rep(0,dim(subt)[1])
censusgrowth<-census/10
cvec <- censusgrowth 
eye <- matrix(0,nrow=dim(subt)[1],ncol=dim(subt)[1])
diag(eye) <- 1
rho<-coef(slm)[1]  
beta<-coef(slm)[3]
subt@data$rus.est <- solve(eye - rho *subtnbw.mat ) %*% cvec * beta
solve(eye - rho *subtnbw.mat )[c(1:5),c(1:5)]
censuschange<-c()
censuschange[subt$SEC_NAME=="公館地區"|subt$SEC_NAME=="學府地區"|subt$SEC_NAME=="興隆地區"|subt$SEC_NAME=="東門地區"]<-1
censuschange[subt$SEC_NAME=="大直地區"|subt$SEC_NAME=="西湖地區"|subt$SEC_NAME=="金龍地區"|subt$SEC_NAME=="灣仔地區"]<--1
censuschange[is.na(censuschange)]<-0
subt@data$rus.est2 <- solve(eye - rho *subtnbw.mat ) %*% censuschange * beta
test<-fortify(subt,region = "SEC_NAME")
head(test2)
test2 <- merge (test, subt@data, by.x = "id", by.y = "SEC_NAME")
ggplot() + geom_polygon(data = test2, aes(x=long,y=lat,group=group, fill =rus.est)) +
coord_equal() + scale_fill_gradient(name = "平均國中小改變數量(單位:間)",low="#132B43",high="#56B1F7") +
ggtitle("台北市次分區人口成長 10%的情況下以SLM估計的次分區國民中小學的增長趨勢圖") + 
theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_blank())
temp<-data.frame(cbind(subt$rus.est,as.character(subt$SEC_NAME)))
(temp[rev(order(temp$X1)),])[c(1:10),]


ggplot() + geom_polygon(data = test2, aes(x=long,y=lat,group=group, fill =rus.est2)) +
coord_equal() + scale_fill_gradient(name = "平均國中小改變數量(單位:間)",low="green",high="red") +
ggtitle("事件發生後台北市次分區國民中小學的增長趨勢圖") + 
theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_blank())
temp2<-data.frame(cbind(subt$rus.est2,as.character(subt$SEC_NAME)))
(temp2[rev(order(temp2$X1)),])[c(1:5),]
(temp2[order(temp2$X1),])[c(1:5),]