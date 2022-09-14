library(fcaR)
library(bench)
library(profvis)
library(jointprof)
library(knitr)
#library(ggplot2)

fc_planets <- FormalContext$new(planets)
#fc_I <- FormalContext$new(I)


###########################################
#TIPOS DE LA ESTRUCTURA DE UN FORMALCONTEXT
###########################################

# class(fc_planets$I)
# str(fc_planets$I)
#
# class(fc_planets$attributes)
# str(fc_planets$attributes)
#
# class(fc_planets$objects)
# str(fc_planets$objects)
#
# class(fc_planets$grades_set)
# str(fc_planets$grades_set)
#
# class(fc_planets$expanded_grades_set)
# str(fc_planets$expanded_grades_set)
#
# class(fc_planets$concepts)
# str(fc_planets$concepts)
#
# class(fc_planets$implications)
# str(fc_planets$implications)

###########################################
#TIPOS DE LA ESTRUCTURA DE UN FORMALCONTEXT
###########################################

  dframe %>%
    kable(format = 'latex', booktabs = TRUE)

S1 <- Set$new(attributes = fc_planets$objects)
S1$assign(Earth = 1, Mars = 1)
S1

S2 <- Set$new(attributes = fc_planets$attributes)
S2$assign(moon = 1, large = 1)
S2

Matrix::as.matrix(fc_planets$I)
fc_planets$I
str(Matrix::as.matrix(fc_planets$I))
str(fc_planets$I)

# print(fc_planets)
# #print(fc_I)
#
# fc_planets$plot()
# #fc_I$plot()
#
# fc_planets$to_latex()

# EXTRAER CONTEXTO FORMAL DE FICHERO
# filename <- system.file("contexts", "airlines.csv", package = "fcaR")
# fc1 <- FormalContext$new(filename)
# fc1
#
# filename <- system.file("contexts", "lives_in_water.cxt", package = "fcaR")
# fc2 <- FormalContext$new(filename)
# fc2

# # TRASPONER UN FORMAL CONTEXT
# fc_dual <- fc_planets$dual()
# fc_dual




# #ANÁLISIS DE RENDIMIENTO ----->      "intent"
# Sintent1 <- fc_planets$intent(S1)
# Sintent2
#
# test5 <- function(set) {
#   for(i in seq(1000)) fc_planets$intent(set)
# }
# joint_pprof(test5(S1))
#
# Sintent2 <- fc_planets$intent_fast(S1)
# Sintent2
# test6 <- function(set) {
#   for(i in seq(1000)) fc_planets$intent_fast(set)
# }
# joint_pprof(test6(S1))
#
# bench::mark(
#   test5(S1),
#   test6(S1)
# )[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]
#
# bench::mark(
#   fc_planets$intent(S1),
#   fc_planets$intent_fast(S1)
# )[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]
#
#
#
#
# #ANÁLISIS DE RENDIMIENTO ----->      "extent"
# Sextent1 <- fc_planets$extent(S2)
# Sextent1
#
# test3 <- function(set) {
#   for(i in seq(1000)) fc_planets$extent(set)
# }
# joint_pprof(test3(S2))
#
# Sextent2<- fc_planets$extent_fast(S2)
# Sextent2
# test4 <- function(set) {
#   for(i in seq(1000)) fc_planets$extent_fast(set)
# }
# joint_pprof(test4(S2))
#
# bench::mark(
#   test3(S2),
#   test4(S2)
# )[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]
#
# bench::mark(
#   fc_planets$extent(S2),
#   fc_planets$extent_fast(S2)
# )[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]




#ANÁLISIS DE RENDIMIENTO ----->     "closure"
Sc <- fc_planets$closure(S2)
Sc
test1 <- function(set) {
  for(i in seq(1000)) fc_planets$closure(set)
}
joint_pprof(test1(S2))

Sc2 <- fc_planets$closure_fast(S2)
Sc2
test2 <- function(set) {
  for(i in seq(1000)) fc_planets$closure_fast(set)
}
joint_pprof(test2(S2))

Sc3 <- fc_planets$closure_fastest_vector(S2)
Sc3
test3 <- function(set) {
  for(i in seq(1000)) fc_planets$closure_fastest_vector(set)
}
joint_pprof(test3(S2))

Sc4 <- fc_planets$closure_fastest_matrix(S2)
Sc4
test4 <- function(set) {
  for(i in seq(1000)) fc_planets$closure_fastest_matrix(set)
}
joint_pprof(test4(S2))

dframe <- bench::mark(
  test1(S2)
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

bench::mark(
  test1(S2),
  test2(S2),
  test3(S2),
  test4(S2)
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

bench::mark(
  fc_planets$closure(S2),
  fc_planets$closure_fast(S2),
  fc_planets$closure_fastest_vector(S2),
  fc_planets$closure_fastest_matrix(S2)
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

joint_pprof(test(S2))

fc_planets$is_closed(S2)
fc_planets$is_closed(Sc)



out_file <- tempfile("jointprof", fileext = ".out")
Rprof(out_file, gc.profiling = TRUE)
fc_planets$closure(S2)
Rprof(NULL)

summary <- summaryRprof(out_file)
summary$by.self

out_file <- tempfile("jointprof", fileext = ".out")
start_profiler(out_file)
for (i in 1:1000) fc_planets$closure(S2)
stop_profiler()

summary <- summaryRprof(out_file)
summary$by.self


#x <- runif(10000)
#(lb <- bench::mark(
#  fc_planets$closure(S2)
#))[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

# test <- function() {
#   identical(fc_planets$closure(S2), fc_planets$closure2(S2))
#   }
# test()

fc_planets$reduce(TRUE)
fc_planets$clarify(TRUE)
fc_planets

fc_planets$find_implications()
fc_planets$concepts
fc_planets$implications







