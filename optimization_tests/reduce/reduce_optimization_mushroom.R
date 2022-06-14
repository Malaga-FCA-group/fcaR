library(fcaR)
library(bench)
library(profvis)
library(jointprof)
library(arules)
data("Mushroom", package = "arules")

fc_mushroom <- FormalContext$new(Mushroom)
fc_mushroom

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "reduce"
######################################################################################


S1 <- fc_mushroom$reduce(TRUE)
S1
test1 <- function() {
  fc_mushroom$reduce(TRUE)
}
joint_pprof(test1())

S2 <- fc_mushroom$reduce_fast(TRUE)
S2
test2 <- function() {
  fc_mushroom$reduce_fast(TRUE)
}
joint_pprof(test2())

bench::mark(
  fc_mushroom$reduce(TRUE),
  fc_mushroom$reduce_fast(TRUE)
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

bench::mark(
  test1(),
  test2()
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]


######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "reduce"
######################################################################################
