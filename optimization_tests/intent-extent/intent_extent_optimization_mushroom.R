library(fcaR)
library(bench)
library(profvis)
library(jointprof)
library(arules)
data("Mushroom", package = "arules")

fc_mushroom <- FormalContext$new(Mushroom)
fc_mushroom
fc_mushroom$objects
fc_mushroom$attributes

S1 <- Set$new(attributes = fc_mushroom$objects)
S1$assign(attributes = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), values = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))
S1

S2 <- Set$new(attributes = fc_mushroom$attributes)
S2$assign("class=edible" = 1, "CapShape=flat" = 1, "CapSurf=grooves" = 1)
S2

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "intent"
######################################################################################


Sintent1 <- fc_mushroom$intent(S1)
Sintent1
test1 <- function(set) {
  for(i in seq(100)) fc_mushroom$intent(set)
}
joint_pprof(test1(S1))

Sintent2 <- fc_mushroom$intent_fast(S1)
Sintent2
test2 <- function(set) {
  for(i in seq(100)) fc_mushroom$intent_fast(set)
}
joint_pprof(test2(S1))

bench::mark(
  test1(S1),
  test2(S1)
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

bench::mark(
  fc_mushroom$intent(S1),
  fc_mushroom$intent_fast(S1)
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]


######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "intent"
######################################################################################



######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "extent"
######################################################################################


Sextent1 <- fc_mushroom$extent(S2)
Sextent1
test3 <- function(set) {
  for(i in seq(1000)) fc_mushroom$extent(set)
}
joint_pprof(test3(S2))

Sextent2<- fc_mushroom$extent_fast(S2)
Sextent2
test4 <- function(set) {
  for(i in seq(1000)) fc_mushroom$extent_fast(set)
}
joint_pprof(test4(S2))

bench::mark(
  test3(S2),
  test4(S2)
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

bench::mark(
  fc_mushroom$extent(S2),
  fc_mushroom$extent_fast(S2)
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]


######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "extent"
######################################################################################
