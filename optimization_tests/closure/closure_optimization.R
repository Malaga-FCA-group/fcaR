library(fcaR)
library(bench)
library(profvis)
library(jointprof)

#PRUEBAS CON cobre32
fc_cobre32 <- FormalContext$new(cobre32)
fc_cobre32

# CLOSURES
S <- Set$new(attributes = fc_cobre32$attributes)
S$assign(FICAL_1 = 1, COSAS_1 = 1, COSAS_2 = 1, COSAS_3 = 1)
S

fc_cobre32$extent(S)

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "closure"
######################################################################################


Sc1 <- fc_cobre32$closure(S)
Sc1
test1 <- function(set) {
  for(i in seq(1000)) fc_cobre32$closure(set)
}
joint_pprof(test1(S))

Sc2 <- fc_cobre32$closure_fast(S)
Sc2
test2 <- function(set) {
  for(i in seq(1000)) fc_cobre32$closure_fast(set)
}
joint_pprof(test2(S))

bench::mark(
  test1(S),
  test2(S)
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

bench::mark(
  fc_cobre32$closure(S),
  fc_cobre32$closure_fast(S)
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]


######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "closure"
######################################################################################
