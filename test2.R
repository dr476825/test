tmp<-c()
k1<-strsplit("A.B",split="")[[1]]
for (i in seq(length(k1))){
tmp<-paste(tmp,k1[length(k1)+1-i],sep="")
}

tmp<-c()
tmp2<-c()
k2<-strsplit("Ac B",split=" ",fixed=T)[[1]]
for (j in c(1:length(k2))){
for (i in seq(length(k2[j]))){
tmp2<-paste(tmp,k2[nchar(k2[j])+1-i],sep="")
}
tmp<-paste(tmp,tmp2,sep=" ")
}

k=15
count<-0
for (i in seq(1,k)){
if (i%%3!=0&i%%5!=0){
count=count+1}
}
count=count+round(k/15,1)
}
