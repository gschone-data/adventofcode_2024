readRDS("day5_2.data")->day5_2
data <-read.delim("input5.data",sep="\n",header=F)
setDT(data)

data |> dplyr::filter(str_detect(data$V1,"\\|"))->obligations
fread(text=obligations$V1,sep="|",header=F)->obligations

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


test_sequence_l<-function(sequence_l){
  res=0
  for (i in (1:length(sequence_l))){
    before<-sequence_l[1:i-1]
    before<-before[!is.na(before)]
    after<-sequence_l[i+1:length(sequence_l)]
    after<-after[!is.na(after)]
    
    sequence_l[i]
    res=res+nrow(obligations[V2==sequence_l[i]][V1%in%after])
    res=res+nrow(obligations[V1==sequence_l[i]][V2%in%before])
  }
  if (res==0){
    return(as.integer(sequence_l[(length(sequence_l)-1)/2+1]))
  }else{
    return(0)
  }
}  






res=vector()
for (k in 1:nrow(day5_2)){
  temp<-unlist(str_split(day5_2$V1[k],","))
  vec=vector()
  for (i in 1:length(temp)){
    vec[i]=nrow(obligations[V1==temp[i]][V2 %in% temp[-i]])
  }
  res[k]=test_sequence_l(temp[order(vec,decreasing = T)])
}

sum(res)
