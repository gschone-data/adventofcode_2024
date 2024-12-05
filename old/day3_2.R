library(stringr)
input<-readLines("input3.data")
res=0
for (i in 1:6) {
  input_temp=str_extract_all(string=input[i],pattern = "mul\\(\\d{1,3}\\,\\d{1,3}\\)")
  input_temp=str_extract_all(string=input_temp[[1]],pattern="\\d{1,3}")
  for(item in input_temp){res=res+as.numeric(item[1])*as.numeric(item[2])    }
}

#simplification de la chaine
for (i in 1:6) {
  for (item in c( "from\\(\\)","where\\(\\)", "how\\(\\)","when\\(\\)","why\\(\\)","how\\(\\)","select\\(\\)", "who\\(\\)", "what\\(\\)")){
  input[i]<-str_replace_all(string=input[i],pattern = item,replacement = "")
}
}
input_all=""
for (i in 1:6){
  print(nchar(input[i]))
  input_all=paste0(input_all,input[i])
}

test<-str_split(string = input_all,pattern = "do\\(\\)")
minus=0

for (item in test[[1]]){
  
  test2=str_split(string=item,pattern="don\\'t\\(\\)")
  k=0
  for (dontdo in test2[[1]]){
    if (k!=0){
      temp<-str_extract_all(string=dontdo,pattern = "mul\\(\\d{1,3}\\,\\d{1,3}\\)")
      temp<-str_extract_all(string=temp[[1]],pattern="\\d{1,3}")
      for(item in temp){minus=minus+as.numeric(item[1])*as.numeric(item[2])    }
    }
    k=k+1 
      
    }
  }
  
final =res-minus
final
