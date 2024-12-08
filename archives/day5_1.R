library(data.table)
library(stringr)
data <-read.delim("input5.data",sep="\n",header=F)
setDT(data)

data |> dplyr::filter(str_detect(data$V1,"\\|"))->obligations
fread(text=obligations$V1,sep="|",header=F)->obligations

data |> dplyr::filter(str_detect(data$V1,","))->print_seq

# vec=vector()
# for (seq in print_seq$V1){
#   print_seq_split<-str_split(seq,",")
# res=0  
# for(i in (1:(length(print_seq_split[[1]])-1))){
# res=res+(nrow(obligations[V2==as.integer(print_test[[1]][i])&V1%in%print_test[[1]][i+1:length(print_test[[1]])]]))
# }
# vec[n]=res
# n=n+1
# }

test_sequence<-function(sequence){
  res=0
  temp<-unlist(str_split(sequence,","))
  for (i in (1:length(temp))){
    before<-temp[1:i-1]
    before<-before[!is.na(before)]
    after<-temp[i+1:length(temp)]
    after<-after[!is.na(after)]
    
    temp[i]
    res=res+nrow(obligations[V2==temp[i]][V1%in%after])
    res=res+nrow(obligations[V1==temp[i]][V2%in%before])
  }
  if (res==0){
    return(as.integer(temp[(length(temp)-1)/2+1]))
  }else{
    return(0)
    }
}  
res=vector()
for (i in 1:nrow(print_seq)){
res[i]=test_sequence(print_seq$V1[i])
  }
sum(res)

as.data.frame(cbind(print_seq$V1,res))->day5_2
day5_2 |> dplyr::filter(res=="0")->day5_2
saveRDS(day5_2,"day5_2.data")
