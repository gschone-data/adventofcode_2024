calcul_point<-function(antenne1,antenne2){
  if (antenne2$x==antenne1$x){
    coef=1
    }else{
    coef<-(antenne2$y-antenne1$y)/(antenne2$x-antenne1$x)
    }
  distx<-abs(antenne1$x-antenne2$x)
  disty<-abs(antenne1$y-antenne2$y)
  
  x0<-min(antenne1$x,antenne2$x)-distx
  y0<-min(antenne1$y,antenne2$y)-disty
  x3<-max(antenne1$x,antenne2$x)+distx
  y3<-max(antenne1$y,antenne2$y)+disty
  
  if (coef>0) {
    point1=list(x=x0,y=y0)
    point2=list(x=x3,y=y3)
  }else{
    point1=list(x=x0,y=y3)
    point2=list(x=x3,y=y0)
  }
  return(list(point1,point2))
}

test_point<-function(point){
  point$x<=50&point$x>0&point$y<=50&point$y>0
}

calcul_droite<-function(antenne1,antenne2){
  if (antenne2$x==antenne1$x){
    a=0
    b=antenne1$y
  }else{
    a<-(antenne2$y-antenne1$y)/(antenne2$x-antenne1$x)
    }
  b=antenne1$y-a*antenne1$x
return(equation=list(a=a,b=b))
  
  
  
}

