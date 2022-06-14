library(fcaR)
library(bench)
library(profvis)
library(jointprof)
library(arules)
# data("Mushroom", package = "arules")

# fc_mushroom <- FormalContext$new(Mushroom)
# fc_mushroom

fc_cobre32 <- FormalContext$new(cobre32)
fc_cobre32

S1 <- fc_cobre32$find_implications()
S1
fc_cobre32$concepts
fc_cobre32$implications

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "join_irreducibles"
######################################################################################


test1 <- function() {
  fc_cobre32$concepts$meet_irreducibles()
}
joint_pprof(test1())

bench::mark(
  fc_cobre32$concepts$meet_irreducibles()
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

bench::mark(
  test1()
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]


######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "join_irreducibles"
######################################################################################
