library(bit64)

options(scipen=999)
w<-read.delim("/data/schoneg/advent/input13",sep="\n",header = F)
#w<-read.delim("/data/schoneg/advent/test.data",sep="\n",header = F)

setDT(w)
w[,index:=1:.N]
w[index%%3==1]->buttonA
w[index%%3==2]->buttonB
w[index%%3==0]->price
library(stringr)


x<-data.table(str_extract_all(buttonA$V1,"(\\d+)", simplify = TRUE))
buttonA[,':='(xa=x$V1,ya=x$V2)]
buttonA$V1=NULL
buttonA$index=NULL


x<-data.table(str_extract_all(buttonB$V1,"(\\d+)", simplify = TRUE))
buttonB[,':='(xb=x$V1,yb=x$V2)]
buttonB$V1=NULL
buttonB$index=NULL


x<-data.table(str_extract_all(price$V1,"(\\d+)", simplify = TRUE))
price[,':='(px=x$V1,py=x$V2)]
price$V1=NULL
price$index=NULL
price$px=as.integer64(price$px)+10000000000000 
price$py=as.integer64(price$py)+10000000000000 
                              
puzzle<-cbind(buttonA,buttonB,price)
puzzle |>
  mutate(xa=as.numeric(xa),
         xb=as.numeric(xb),
         ya=as.numeric(ya),
         yb=as.numeric(yb),
         px=as.numeric(px),
         py=as.numeric(py))->puzzle

puzzle |> mutate(coefxa=pcm(xa,ya)/xa,coefya=pcm(xa,ya)/ya)->puzzle
puzzle |> mutate(res_b1=(coefxa*px-coefya*py)/(coefxa*xb-coefya*yb))->puzzle
puzzle |> mutate(res_a1=(px-xb*res_b1)/xa)->puzzle

puzzle |> mutate(coefxb=pcm(xb,yb)/xb,coefyb=pcm(xb,yb)/yb)->puzzle
puzzle |> mutate(res_a2=(coefxb*px-coefyb*py)/(coefxb*xa-coefyb*ya))->puzzle
puzzle |> mutate(res_b2=(px-xa*res_a2)/xb)->puzzle


puzzle |> mutate(test1=(abs(res_b1-floor(res_b1))<0.001)&(abs(res_a1-floor(res_a1))<0.001))->puzzle
puzzle |> mutate(test2=(abs(res_b2-floor(res_b2))<0.001)&(abs(res_a2-floor(res_a2))<0.001))->puzzle
puzzle

puzzle |> mutate(res1=ifelse(test1==F,NA,3*res_a1+res_b1),
                 res2=ifelse(test2==F,NA,3*res_a2+res_b2))->puzzle

puzzle |> mutate(verif1=res_a1*xa+res_b1*xb-px,
                 verif2=res_a1*ya+res_b1*yb-py)->puzzle
puzzle |> filter(verif1!=0|verif2!=0)
puzzle |> mutate(res=pmin(res1,res2,na.rm=T))->puzzle
sum(puzzle,na.rm=T)
puzzle[,.N,test1&test2]
puzzle[test1==T&test2==T]->puzzlev1
