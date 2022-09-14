library(fcaR)
library(bench)
library(profvis)
library(jointprof)
library(arules)
library(knitr)
# data("Mushroom", package = "arules")

# fc_mushroom <- FormalContext$new(Mushroom)
# fc_mushroom

fc_cobre32 <- FormalContext$new(cobre32)

fc_cobre32_opt <- FormalContext_opt$new(cobre32)

fc_cobre32$find_implications()

fc_cobre32_opt$find_implications()

test1 <- function() {
  fc_cobre32$concepts$join_irreducibles()
}

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "join_irreducibles"
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
  fc_cobre32$concepts$join_irreducibles()
}

test3 <- function() {
  fc_cobre32_opt$concepts$join_irreducibles()
}

join_irreducibles_results <- bench::mark(
  test2(),
  test3(),
  iterations = 1
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

join_irreducibles_results

join_irreducibles_results %>% kable(format = 'latex', booktabs = TRUE)

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "join_irreducibles"
######################################################################################
