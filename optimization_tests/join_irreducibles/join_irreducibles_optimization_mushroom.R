library(fcaR)
library(bench)
library(profvis)
library(jointprof)
library(arules)
# data("Mushroom", package = "arules")

# fc_mushroom <- FormalContext$new(Mushroom)
# fc_mushroom

fc_planets <- FormalContext$new(planets)
fc_planets

S1 <- fc_planets$find_implications()
S1
fc_planets$concepts
fc_planets$implications

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "join_irreducibles"
######################################################################################


test1 <- function() {
  for(i in seq(10000)) fc_planets$concepts$join_irreducibles()
}
joint_pprof(test1())

bench::mark(
  fc_planets$concepts$join_irreducibles()
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

bench::mark(
  test1()
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]


######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "join_irreducibles"
######################################################################################
