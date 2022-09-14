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

S <- fc_cobre32$concepts$sub(6)

C <- fc_cobre32$concepts[1:10]

test1 <- function() {
  fc_cobre32$concepts$supremum(C)
}

test2 <- function() {
  fc_cobre32$concepts$infimum(C)
}

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "supremum"
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

test3 <- function() {
  fc_cobre32_opt$concepts$supremum(S)
}

supremum_results <- bench::mark(
  test1(),
  test3(),
  iterations = 1
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

supremum_results

supremum_results %>% kable(format = 'latex', booktabs = TRUE)

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "supremum"
######################################################################################


######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "infimum"
######################################################################################


out_file <- tempfile("jointprof", fileext = ".out")
start_profiler(out_file)
test2()
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

test4 <- function() {
  fc_cobre32_opt$concepts$infimum(S)
}

infimum_results <- bench::mark(
  test2(),
  test4(),
  iterations = 1
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

infimum_results

infimum_results %>% kable(format = 'latex', booktabs = TRUE)

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "infimum"
######################################################################################
