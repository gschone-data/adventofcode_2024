w<-read.delim("/data/schoneg/advent/input13",sep="\n",header = F)
setDT(w)
w[,index:=1:.N]
w[index%%3==1]->buttonA
w[index%%3==2]->buttonB
w[index%%3==0]->price
library(stringr)


x<-data.table(str_extract_all(buttonA$V1,"(\\d+)", simplify = TRUE))
buttonA[,':='(aa=x$V1,ab=x$V2)]
buttonA$V1=NULL
buttonA$index=NULL


x<-data.table(str_extract_all(buttonB$V1,"(\\d+)", simplify = TRUE))
buttonB[,':='(ba=x$V1,bb=x$V2)]
buttonB$V1=NULL
buttonB$index=NULL


x<-data.table(str_extract_all(price$V1,"(\\d+)", simplify = TRUE))
price[,':='(pa=x$V1,pb=x$V2)]
price$V1=NULL
price$index=NULL


puzzle<-cbind(buttonA,buttonB,price)
puzzle |>
  mutate(aa=as.integer(aa),
         ab=as.integer(ab),
         ba=as.integer(ba),
         bb=as.integer(bb),
         pa=as.integer(pa),
         pb=as.integer(pb))->puzzle

puzzle$pcm=pcm(puzzle$aa,puzzle$ab)
puzzle$ma=puzzle$pcm/puzzle$aa
puzzle$mb=puzzle$pcm/puzzle$ab
puzzle |> mutate(res_b=(ma*pa-mb*pb)/(ma*ba-mb*bb))->puzzle
puzzle |> mutate(res_a=(pa-ba*res_b)/aa)->puzzle
puzzle |> mutate(test=(res_b==floor(res_b)&res_b<=100&res_a<=100))->puzzle

puzzle$res_fin=puzzle$test*(puzzle$res_b+puzzle$res_a*3)
sum(puzzle$res_fin)
