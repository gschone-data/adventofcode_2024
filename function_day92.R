library(dplyr)

taille_bloc_vide<-function(list){
  cherche<- which(is.na(list))
  ecart_liste<-lead(cherche)-cherche-1
  tmp<-rle(ecart_liste)
  liste_taille<-tmp$lengths[tmp$values==0]
  return(liste_taille)
}
list<-test

cherche<-which

ead(ecart_liste,50)
