library(fcaR)
library(bench)
library(profvis)
library(jointprof)
library(arules)
library(knitr)
# data("Mushroom", package = "arules")

# fc_mushroom <- FormalContext$new(Mushroom)
# fc_mushroom_opt <- FormalContext_opt$new(Mushroom)

fc_cobre32 <- FormalContext$new(cobre32)

fc_cobre32_opt <- FormalContext_opt$new(cobre32)

fc_cobre32$find_concepts()

fc_cobre32_opt$find_concepts()

idx <- which(fc_cobre32$concepts$support() > 0.5)

test1 <- function() {
  fc_cobre32$concepts$sublattice(idx)
}

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "sublattice"
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
  fc_cobre32_opt$concepts$sublattice2(idx)
}

sublattice_results1 <- system.time(test1())
sublattice_results2 <- system.time(test2())

sublattice_results1
sublattice_results2

# sublattice_results <- bench::mark(
#   test1(),
#   test2(),
#   iterations = 1
# )[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]
#
# sublattice_results
#
# sublattice_results %>% kable(format = 'latex', booktabs = TRUE)

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "sublattice"
######################################################################################
