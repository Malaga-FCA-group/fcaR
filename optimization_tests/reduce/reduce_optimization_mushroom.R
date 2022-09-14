library(fcaR)
library(bench)
library(profvis)
library(jointprof)
library(arules)
library(knitr)
data("Mushroom", package = "arules")

fc_mushroom <- FormalContext$new(Mushroom)

fc_mushroom_opt <- FormalContext_opt$new(Mushroom)

test1 <- function() {
  fc_mushroom$reduce(TRUE)
}

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "reduce"
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
  fc_mushroom_opt$reduce(TRUE)
}

test3 <- function() {
  fc_mushroom_opt$reduce_fast(TRUE)
}

reduce_results <- bench::mark(
  test3(),
  test2(),
  test1(),
  check = FALSE
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

reduce_results

reduce_results %>% kable(format = 'latex', booktabs = TRUE)

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "reduce"
######################################################################################
