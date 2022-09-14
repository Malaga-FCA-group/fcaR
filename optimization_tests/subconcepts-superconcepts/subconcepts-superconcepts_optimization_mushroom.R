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

test1 <- function() {
  fc_cobre32$concepts$subconcepts(S)
}

test2 <- function() {
  fc_cobre32$concepts$superconcepts(S)
}

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "subconcepts"
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
  fc_cobre32_opt$concepts$subconcepts(S)
}

subconcepts_results <- bench::mark(
  test1(),
  test3(),
  iterations = 1,
  check = FALSE
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

subconcepts_results

subconcepts_results %>% kable(format = 'latex', booktabs = TRUE)

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "subconcepts"
######################################################################################


######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "superconcepts"
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
  fc_cobre32_opt$concepts$superconcepts(S)
}

superconcepts_results <- bench::mark(
  test2(),
  test4(),
  iterations = 1,
  check = FALSE
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

superconcepts_results

superconcepts_results %>% kable(format = 'latex', booktabs = TRUE)

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "superconcepts"
######################################################################################

fc_planets <- FormalContext$new(planets)
fc_planets$find_implications()
prueba <- fc_planets$concepts$extents()
prueba
.subset(prueba)
