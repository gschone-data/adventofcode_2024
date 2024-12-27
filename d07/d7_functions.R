calc<-function(i,nombres){
  tmp<-""
  tmp<-paste0(rep("(",length(nombres)),collapse = '')
  
  for(k in 1:(length(nombres)-1)){
    tmp<-paste0(tmp,nombres[k],")",comb[i,k])
    
  }
  
  tmp<-paste0(tmp,nombres[k+1],")")
  res_calc<-eval(parse(text=tmp))
  return(res_calc)
}

`||` <- function(x, y) {
  return(as.numeric(paste0(x,y)))
}

create_combs<-function(nb_num,base){
  
  
  gtools::permutations(length(base),nb_num-1,v=base,repeats.allowed = T)->comb
  return(comb)
  
}

