.intersection_xy <- function(x,Y) {
  
  .replicate_col(x, ncol(Y))* Y
}