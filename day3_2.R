library(stringr)
input<-readLines("input3.data")
res=0
for (i in 1:6) {
  input_temp=str_extract_all(string=input[i],pattern = "mul\\(\\d{1,3}\\,\\d{1,3}\\)")
  input_temp=str_extract_all(string=input_temp[[1]],pattern="\\d{1,3}")
  for(item in input_temp){res=res+as.numeric(item[1])*as.numeric(item[2])    }
}
minus=0
for (i in 1:6) {
  input_temp_min=str_extract_all(string=input[i],pattern="(?<=don\\'t\\(\\)).*?(?=do\\(\\))")
  input_temp_min=str_extract_all(string=unlist(input_temp_min),pattern = "mul\\(\\d{1,3}\\,\\d{1,3}\\)")
  input_temp_min=str_extract_all(string=unlist(input_temp_min),pattern="\\d{1,3}")
  for(item in input_temp_min){minus=minus+as.numeric(item[1])*as.numeric(item[2])    }
  }
final=res-minus
minus0=minus
for(i in 1:6){print(nchar(input[i]))}


res=0
min=0
for (i in 1:6) {
  input_temp=str_extract_all(string=input[i],pattern = "(?<=do\\(\\)).*?mul\\(\\d{1,3}\\,\\d{1,3}\\)")
  input_temp=str_extract_all(string=unlist(input_temp),pattern="\\d{1,3}")
  for(item in input_temp){res=res+as.numeric(item[1])*as.numeric(item[2])    }
}
