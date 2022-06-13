library(fcaR)
library(bench)
library(profvis)
library(jointprof)
library(arules)

fc_planets <- FormalContext$new(planets)
fc_planets


######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "reduce"
######################################################################################


S1 <- fc_planets$reduce(TRUE)
S1
test1 <- function() {
  for(i in seq(10)) fc_planets$reduce(TRUE)
}
joint_pprof(test1())

bench::mark(
  fc_planets$reduce(TRUE)
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

bench::mark(
  test1()
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]


######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "reduce"
######################################################################################
