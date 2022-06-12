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

S2 <- fc_planets$concepts$sub(5)
S2
C <- fc_planets$concepts[2:4]
######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "supremum"
######################################################################################


test1 <- function() {
  for(i in seq(1000)) fc_planets$concepts$supremum(C)
}
joint_pprof(test1())

bench::mark(
  fc_planets$concepts$supremum(C)
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

bench::mark(
  test1()
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]


######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "supremum"
######################################################################################


######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "infimum"
######################################################################################


test2 <- function() {
  for(i in seq(1000)) fc_planets$concepts$infimum(C)
}
joint_pprof(test2())

bench::mark(
  fc_planets$concepts$infimum(C)
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

bench::mark(
  test2()
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]


######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "infimum"
######################################################################################

