library(fcaR)
library(bench)
library(profvis)
library(jointprof)
library(arules)
library(knitr)
data("Mushroom", package = "arules")

fc_mushroom <- FormalContext$new(Mushroom)

fc_mushroom_opt <- FormalContext_opt$new(Mushroom)

S <- Set$new(attributes = fc_mushroom$attributes)
S$assign("class=edible" = 1, "CapShape=flat" = 1, "CapSurf=grooves" = 1)

test1 <- function() {
  fc_mushroom$closure(S)
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
  fc_mushroom_opt$closure(S)
}

test3 <- function() {
  fc_mushroom_opt$closure_fast(S)
}

test4 <- function() {
  fc_mushroom_opt$closure_fastest_vector(S)
}

test5 <- function() {
  fc_mushroom_opt$closure_fastest_matrix(S)
}

closure_results <- bench::mark(
  test1(),
  test2(),
  test3(),
  test4(),
  test5(),
  iterations = 1000
)[c("expression", "min", "median", "itr/sec", "n_gc", "total_time", "mem_alloc")]

closure_results

closure_results %>% kable(format = 'latex', booktabs = TRUE)

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "closure"
######################################################################################

# y <- function(n) {
#   z <- sample(fc_mushroom_opt$attributes, sample(1:n, 1))
#   S <- Set$new(attributes = fc_mushroom_opt$attributes)
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
#   z <- sample(fc_mushroom_opt$attributes, sample(1:10, 1))
#   S <- Set$new(attributes = fc_mushroom_opt$attributes)
#   S$assign(attributes = z, values = 1)
#   x(S)
# }
# #x(y(2))
# sink()
# unlink("sink-examp.txt")
