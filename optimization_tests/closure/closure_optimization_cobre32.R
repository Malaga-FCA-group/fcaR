library(fcaR)
library(bench)
library(profvis)
library(jointprof)
library(arules)

fc_cobre32 <- FormalContext$new(cobre32)

fc_cobre32_opt <- FormalContext_opt$new(cobre32)

S <- Set$new(attributes = fc_cobre32$attributes)
S$assign("COSAS_1" = 1, "COSAS_2" = 1, "FICAL_2" = 1)

test1 <- function() {
  for(i in seq(100)) fc_cobre32$closure(S)
}

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "closure"
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
  fc_cobre32$closure(S)
}

test3 <- function() {
  fc_cobre32_opt$closure(S)
}

test4 <- function() {
  fc_cobre32_opt$closure_fast(S)
}

test5 <- function() {
  fc_cobre32_opt$closure_fastest_vector(S)
}

test6 <- function() {
  fc_cobre32_opt$closure_fastest_matrix(S)
}

bench::mark(
  test2(),
  test3(),
  test4(),
  test5(),
  test6(),
  iterations = 10000
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]


######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "closure"
######################################################################################

# y <- function(n) {
#   z <- sample(fc_cobre32_opt$attributes, sample(1:n, 1))
#   S <- Set$new(attributes = fc_cobre32_opt$attributes)
#   S$assign(attributes = z, values = 1)
#   return(S)
# }
#
# x <- function(S) {
#   bench::mark(
#   test1(S),
#   test2(S),
#   test3(S),
#   test4(S),
#   test5(S),
#   iterations = 10
# )[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]
# }
#
# sink("prueba.txt", append = TRUE)
# for(i in seq(2)) {
#   z <- sample(fc_cobre32_opt$attributes, sample(1:10, 1))
#   S <- Set$new(attributes = fc_cobre32_opt$attributes)
#   S$assign(attributes = z, values = 1)
#   x(S)
# }
# #x(y(2))
# sink()
# unlink("sink-examp.txt")
