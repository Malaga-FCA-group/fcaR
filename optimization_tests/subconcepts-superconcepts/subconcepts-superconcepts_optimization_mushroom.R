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

fc_cobre32_2 <- FormalContext$new(cobre32)
fc_cobre32_2

S1 <- fc_cobre32$find_implications()
S1

S3 <- fc_cobre32_2$find_implications()
S3
# fc_cobre32$concepts
# fc_cobre32$implications

S2 <- fc_cobre32$concepts$sub(10)
S2

S4 <- fc_cobre32_2$concepts$sub(10)
S4

fc_planets <- FormalContext$new(planets)
fc_planets$find_implications()

x <- fc_planets$implications$get_LHS_matrix()
x

y <- fc_planets$implications$get_RHS_matrix()
y

z <- fc_planets$concepts$extents()
z

f######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "subconcepts"
######################################################################################


test1 <- function() {
  fc_cobre32$concepts$subconcepts(S2)
}
joint_pprof(test1())

bench::mark(
  fc_cobre32$concepts$superconcepts(S2),
  fc_cobre32_2$concepts$superconcepts_fast(S4)
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

bench::mark(
  test1()
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]


######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "subconcepts"
######################################################################################


######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "superconcepts"
######################################################################################


test2 <- function() {
  fc_cobre32$concepts$superconcepts(S2)
}
joint_pprof(test2())

bench::mark(
  fc_cobre32$concepts$superconcepts(S2)
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

bench::mark(
  test2()
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]


######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "superconcepts"
######################################################################################
