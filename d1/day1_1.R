library(data.table)
data <-read.delim("input",sep="",header=F)
setDT(data)


for (k in 1:2){ 
temp<-data[,get(colnames(data)[k])]
temp<-setDT(as.data.frame(temp))
setorder(temp,temp)
assign(paste0("temp",k),temp)
}

result=cbind(temp1,temp2)
result$dist=abs(temp1-temp2)
dist_tot=sum(result$dist)
