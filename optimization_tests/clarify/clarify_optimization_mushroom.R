library(fcaR)
library(bench)
library(profvis)
library(jointprof)
library(arules)
library(microbenchmark)
library(knitr)
data("Mushroom", package = "arules")

fc_mushroom <- FormalContext$new(Mushroom)

fc_mushroom_opt <- FormalContext_opt$new(Mushroom)

fc_mushroom_opt_binary <- FormalContext_opt$new(Mushroom)

test1 <- function() {
  for(i in seq(10)) fc_mushroom$clarify(TRUE)
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
  fc_mushroom$clarify(TRUE)
}

test3 <- function() {
  fc_mushroom_opt$clarify2(TRUE)
}

test4 <- function() {
  fc_mushroom_opt_binary$clarify_binary(TRUE)
}

clarify_results <- bench::mark(
  test2(),
  test3(),
  test4(),
  iterations = 10,
  check = FALSE
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

clarify_results

clarify_results %>% kable(format = 'latex', booktabs = TRUE)


######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "clarify"
######################################################################################
