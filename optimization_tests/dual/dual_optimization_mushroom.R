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
  for(i in seq(4)) fc_mushroom$dual()
}

out_file <- tempfile("jointprof", fileext = ".out")
start_profiler(out_file)
test1()
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

bench::mark(
  fc_mushroom$dual()
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

bench::mark(
  test1()
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]


######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "dual"
######################################################################################
