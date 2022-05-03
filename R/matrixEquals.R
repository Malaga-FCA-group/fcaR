.matrixEquals <- function(x, y){
  return ( dim(x) == dim(y) && all(x == y) )
}
