library(fcaR)
library(bench)
library(profvis)
library(jointprof)

#PRUEBAS CON cobre32
fc_cobre32 <- FormalContext$new(cobre32)
fc_cobre32

var <- Matrix::t(fc_cobre32$I)
var
var2 <-compute_grades(var)
var2
str(var2)

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "dual"
######################################################################################


fc_dual1 <- fc_cobre32$dual()
fc_dual1
test1 <- function() {
  for(i in seq(100)) fc_cobre32$dual()
}
joint_pprof(test1())

bench::mark(
  test1(S)
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

bench::mark(
  fc_cobre32$dual()
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]


######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "dual"
######################################################################################
