library(fcaR)
library(bench)
library(profvis)
library(jointprof)
library(arules)
data("Mushroom", package = "arules")

#PRUEBAS CON cobre32
fc_mushroom <- FormalContext$new(Mushroom)
fc_mushroom

# CLOSURES
S <- Set$new(attributes = fc_mushroom$attributes)
S$assign("class=edible" = 1, "CapShape=flat" = 1, "CapSurf=grooves" = 1)
S

fc_mushroom$extent(S)

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "closure"
######################################################################################


Sc1 <- fc_mushroom$closure(S)
Sc1
test1 <- function(set) {
  for(i in seq(1000)) fc_mushroom$closure(set)
}
joint_pprof(test1(S))

Sc2 <- fc_mushroom$closure_fast(S)
Sc2
test2 <- function(set) {
  for(i in seq(1000)) fc_mushroom$closure_fast(set)
}
joint_pprof(test2(S))

Sc3 <- fc_mushroom$closure_fastest_vector(S)
Sc3
test3 <- function(set) {
  for(i in seq(1000)) fc_mushroom$closure_fastest_vector(set)
}
joint_pprof(test3(S))

Sc4 <- fc_mushroom$closure_fastest_matrix(S)
Sc4
test4 <- function(set) {
  for(i in seq(1000)) fc_mushroom$closure_fastest_matrix(set)
}
joint_pprof(test4(S))

bench::mark(
  fc_mushroom$closure(S),
  fc_mushroom$closure_fast(S),
  fc_mushroom$closure_fastest_vector(S),
  fc_mushroom$closure_fastest_matrix(S)
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

bench::mark(
  test1(S),
  test2(S),
  test3(S),
  test4(S)
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "closure"
######################################################################################
