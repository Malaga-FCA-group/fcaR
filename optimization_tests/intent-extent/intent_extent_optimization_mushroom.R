library(fcaR)
library(bench)
library(profvis)
library(jointprof)
library(arules)
data("Mushroom", package = "arules")

fc_mushroom <- FormalContext$new(Mushroom)

fc_mushroom_opt <- FormalContext_opt$new(Mushroom)

S1 <- Set$new(attributes = fc_mushroom$objects)
S1$assign(attributes = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), values = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))

S2 <- Set$new(attributes = fc_mushroom$attributes)
S2$assign("class=edible" = 1, "CapShape=flat" = 1, "CapSurf=grooves" = 1)

test1 <- function() {
  for(i in seq(1000)) fc_mushroom$intent(S1)
}

test2 <- function() {
  for(i in seq(1000)) fc_mushroom$extent(S2)
}

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "intent"
######################################################################################


out_file <- tempfile("jointprof", fileext = ".out")
start_profiler(out_file)
test1(S1)
profile_data <- stop_profiler()

pprof_file <- tempfile("jointprof", fileext = ".pb.gz")
profile::write_pprof(profile_data, pprof_file)
system2(
  find_pprof(),
  c(
    "-http",
    "localhost:8080",
    shQuote(pprof_file)
  )
)

test3 <- function() {
  fc_mushroom$intent(S1)
}

test4 <- function() {
  fc_mushroom_opt$intent(S1)
}

test5 <- function() {
  fc_mushroom_opt$intent_fast(S1)
}

bench::mark(
  test3(),
  test4(),
  test5(),
  iterations = 1000
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]


######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "intent"
######################################################################################



######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "extent"
######################################################################################


out_file <- tempfile("jointprof", fileext = ".out")
start_profiler(out_file)
test2(S2)
profile_data <- stop_profiler()

pprof_file <- tempfile("jointprof", fileext = ".pb.gz")
profile::write_pprof(profile_data, pprof_file)
system2(
  find_pprof(),
  c(
    "-http",
    "localhost:8080",
    shQuote(pprof_file)
  )
)

test6 <- function() {
  fc_mushroom$extent(S2)
}

test7 <- function() {
  fc_mushroom_opt$extent(S2)
}

test8 <- function() {
  fc_mushroom_opt$extent_fast(S2)
}

bench::mark(
  test6(),
  test7(),
  test8(),
  iterations = 1000
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]


######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "extent"
######################################################################################
