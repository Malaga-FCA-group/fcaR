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
  fc_cobre32_opt$concepts$supremum2(C)
}

supremum_results1 <- system.time(test1())
supremum_results2 <- system.time(test3())

supremum_results1
supremum_results2

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
  fc_cobre32_opt$concepts$infimum2(C)
}

supremum_results1 <- system.time(test2())
supremum_results2 <- system.time(test4())

supremum_results1
supremum_results2

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "infimum"
######################################################################################
