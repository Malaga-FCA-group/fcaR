library(fcaR)
library(bench)
library(profvis)
library(jointprof)
library(arules)
data("Mushroom", package = "arules")

fc_mushroom <- FormalContext$new(Mushroom)

fc_mushroom_opt <- FormalContext_opt$new(Mushroom)

S <- Set$new(attributes = fc_mushroom$attributes)
S$assign("class=edible" = 1, "CapShape=flat" = 1, "CapSurf=grooves" = 1)

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "closure"
######################################################################################


test1 <- function(set) {
  fc_mushroom$closure(set)
}

out_file <- tempfile("jointprof", fileext = ".out")
start_profiler(out_file)
test1(S)
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

test2 <- function(set) {
  fc_mushroom_opt$closure(set)
}

test3 <- function(set) {
  fc_mushroom_opt$closure_fast(set)
}

test4 <- function(set) {
  fc_mushroom_opt$closure_fastest_vector(set)
}

test5 <- function(set) {
  fc_mushroom_opt$closure_fastest_matrix(set)
}

bench::mark(
  test1(S),
  test2(S),
  test3(S),
  test4(S),
  test5(S),
  iterations = 1000
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "closure"
######################################################################################
