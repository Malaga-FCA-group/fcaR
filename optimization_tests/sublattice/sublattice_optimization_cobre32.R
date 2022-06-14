library(fcaR)
library(bench)
library(profvis)
library(jointprof)
library(arules)

fc_cobre32 <- FormalContext$new(cobre32)
fc_cobre32

fc_cobre32$find_implications()


######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "join_irreducibles"
######################################################################################


idx <- which(fc_cobre32$concepts$support() > 0.2)
test1 <- function() {
  fc_cobre32$concepts$sublattice(idx)
}
joint_pprof(test1())

bench::mark(
  fc_cobre32$concepts$join_irreducibles()
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

bench::mark(
  test1()
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]


######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "join_irreducibles"
######################################################################################
