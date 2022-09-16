library(fcaR)
library(bench)
library(profvis)
library(jointprof)
library(arules)
library(knitr)

fc_cobre32 <- FormalContext$new(cobre32)

fc_cobre32_opt <- FormalContext_opt$new(cobre32)

fc_cobre32$find_concepts()

fc_cobre32_opt$find_concepts()

test1 <- function() {
  fc_cobre32$standardize()
}

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "standardize"
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
  fc_cobre32_opt$standardize2()
}

standardize_results1 <- system.time(test1())
standardize_results2 <- system.time(test2())

standardize_results1
standardize_results2

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "standardize"
######################################################################################
