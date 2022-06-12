library(fcaR)
library(bench)
library(profvis)
library(jointprof)
library(arules)
data("Mushroom", package = "arules")

fc_mushroom <- FormalContext$new(Mushroom)
fc_mushroom

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "dual"
######################################################################################


fc_dual<- fc_mushroom$dual()
fc_dual
test1 <- function() {
  for(i in seq(2)) fc_mushroom$dual()
}
joint_pprof(test1())

bench::mark(
  fc_mushroom$dual()
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

bench::mark(
  test1()
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]


######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "dual"
######################################################################################
