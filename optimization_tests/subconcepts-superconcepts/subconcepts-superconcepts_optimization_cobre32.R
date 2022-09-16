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

S <- fc_cobre32$concepts$sub(6)

C <- fc_cobre32$concepts[1:10]

test1 <- function() {
  fc_cobre32$concepts$subconcepts(C)
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
  fc_cobre32_opt$concepts$subconcepts2(C)
}

subconcepts_results1 <- system.time(test1())
subconcepts_results2 <- system.time(test3())

subconcepts_results1
subconcepts_results2

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
  fc_cobre32_opt$concepts$superconcepts2(S)
}

superconcepts_results1 <- system.time(test2())
superconcepts_results2 <- system.time(test4())

superconcepts_results1
superconcepts_results2

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "superconcepts"
######################################################################################
