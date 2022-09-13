library(fcaR)
library(bench)
library(profvis)
library(jointprof)
library(arules)
library(microbenchmark)

fc_cobre32 <- FormalContext$new(cobre32)

fc_cobre32_opt <- FormalContext_opt$new(cobre32)

test1 <- function() {
  for(i in seq(100)) fc_cobre32$clarify(TRUE)
}

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "clarify"
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
  fc_cobre32$clarify(TRUE)
}

test3 <- function() {
  fc_cobre32_opt$clarify(TRUE)
}

clarify_results <- bench::mark(
  test2(),
  test3(),
  iterations = 1000,
  check = FALSE
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

clarify_results

clarify_results %>% kable(format = 'latex', booktabs = TRUE)

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "clarify"
######################################################################################
