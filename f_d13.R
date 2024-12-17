gcd <- function(x,y) {
  r <- x%%y;
  return(ifelse(r, gcd(y, r), y))
}
pcm <- function(x,y){
  return(x*y/gcd(x,y))
}