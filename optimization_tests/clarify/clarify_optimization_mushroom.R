library(fcaR)
library(bench)
library(profvis)
library(jointprof)
library(arules)
data("Mushroom", package = "arules")

fc_mushroom <- FormalContext$new(Mushroom)
fc_mushroom

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "clarify"
######################################################################################


S1<- fc_mushroom$clarify(TRUE)
S1
test1 <- function() {
  for(i in seq(10)) fc_mushroom$clarify(TRUE)
}
joint_pprof(test1())

bench::mark(
  fc_mushroom$clarify(TRUE)
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

bench::mark(
  test1()
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]


######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "clarify"
######################################################################################
