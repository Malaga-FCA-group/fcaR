library(fcaR)
library(bench)
library(profvis)
library(jointprof)
library(arules)
#data("Mushroom")

fc_planets <- FormalContext$new(planets)
fc_planets

#fc_mushroom <- FormalContext$new(Mushroom)
#fc_mushroom

S1 <- fc_planets$find_implications()
S1
fc_planets$concepts
fc_planets$implications

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "find_implications"
######################################################################################

S2 <- fc_planets$standardize()
S2
test1 <- function() {
  for(i in seq(100)) fc_planets$standardize()
}
joint_pprof(test1())

bench::mark(
  fc_planets$standardize()
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

bench::mark(
  test1()
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]


######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "find_implications"
######################################################################################
