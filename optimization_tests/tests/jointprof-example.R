library(fcaR)
library(profvis)
library(bench)

fib_r <- function(x) {
  if (x <= 2) return(rep(1, x))
  fib <- fib_r(x - 1)
  c(fib, fib[x - 1] + fib[x - 2])
}

test <- function(i) {
  for (i in seq_len(i)) {
    stopifnot(identical(fib_r(i), fib_cpp(i)))
  }
}

library(jointprof)
out_file <- tempfile("jointprof", fileext = ".out")
start_profiler(out_file)
for (i in 1:10000) fc_planets$closure(S2)
stop_profiler()

summary <- summaryRprof(out_file)
summary$by.self


library(jointprof)
out_file <- tempfile("jointprof", fileext = ".out")
start_profiler(out_file)
for (i in 1:10) test(700)
profile_data <- stop_profiler()

profile_data

profile_data$samples
profile_data$locations
profile_data$functions
