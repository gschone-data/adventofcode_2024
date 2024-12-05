library(data.table)
library(dplyr)
data <-read.delim("input.data",sep="",header=F)
setDT(data)


for (k in 1:2){ 
temp<-data[,get(colnames(data)[k])]
temp<-setDT(as.data.frame(temp))
setorder(temp,temp)
assign(paste0("temp",k),temp)
}

temp2 |> group_by(temp) |> summarise(n())->temp2_count
head(temp2_count)

temp1 |> inner_join(temp2_count,by="temp")->base_12
base_12$similarity=base_12$temp*base_12$`n()`
result_12=sum(base_12$similarity)
