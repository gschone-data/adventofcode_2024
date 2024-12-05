library(stringr)
input<-readLines("input3.data")
res=0
for (i in 1:6) {
  input_temp=str_extract_all(string=input[i],pattern = "mul\\(\\d{1,3}\\,\\d{1,3}\\)")
  input_temp=str_extract_all(string=input_temp[[1]],pattern="\\d{1,3}")
  for(item in input_temp){res=res+as.numeric(item[1])*as.numeric(item[2])    }
}
