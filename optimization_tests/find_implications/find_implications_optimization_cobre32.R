library(fcaR)
library(bench)
library(profvis)
library(jointprof)
library(arules)

fc_cobre32 <- FormalContext$new(cobre32)

fc_cobre32_opt <- FormalContext_opt$new(cobre32)

# fc_mushroom <- FormalContext$new(Mushroom)

test1 <- function() {
  fc_cobre32$find_implications()
}

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "find_implications"
######################################################################################


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

test2 <- function() {
  fc_cobre32_opt$find_implications()
}

bench::mark(
  test1(),
  iterations = 1
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

bench::mark(
  test2(),
  iterations = 1
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]


######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "find_implications"
######################################################################################
