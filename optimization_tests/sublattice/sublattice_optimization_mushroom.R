library(fcaR)
library(bench)
library(profvis)
library(jointprof)
library(arules)
library(knitr)
data("Mushroom", package = "arules")

fc_mushroom <- FormalContext$new(Mushroom)

fc_mushroom_opt <- FormalContext_opt$new(Mushroom)

fc_mushroom_opt_binary <- FormalContext_opt$new(Mushroom)

fc_mushroom$find_concepts()

fc_mushroom_opt$find_concepts()

fc_mushroom_opt_binary$find_concepts()

idx <- which(fc_mushroom$concepts$support() > 0.5)

test1 <- function() {
  fc_mushroom$concepts$sublattice(idx)
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
  fc_mushroom_opt$concepts$sublattice2(idx)
}

test3 <- function() {
  fc_mushroom_opt_binary$concepts$sublattice_binary(idx)
}

sublattice_results1 <- system.time(test1())
sublattice_results2 <- system.time(test2())
sublattice_results3 <- system.time(test3())

sublattice_results1
sublattice_results2
sublattice_results3



####################################################
# PRUEBAS CON CONJUNTO DE ENTRADA BINARIO ALEATORIO
####################################################

conjuntoPruebas <- generate_context()

fc_prueba <- FormalContext$new(conjuntoPruebas)

fc_prueba_opt <- FormalContext_opt$new(conjuntoPruebas)

fc_prueba_opt_binary <- FormalContext_opt$new(conjuntoPruebas)

fc_prueba$find_concepts()

fc_prueba_opt$find_concepts()

fc_prueba_opt_binary$find_concepts()

idx <- which(fc_prueba$concepts$support() > 0.5)

test1 <- function() {
  fc_prueba$concepts$sublattice(idx)
}

test2 <- function() {
  fc_prueba_opt$concepts$sublattice2(idx)
}

test3 <- function() {
  fc_prueba_opt_binary$concepts$sublattice_binary(idx)
}

sublattice_results1 <- system.time(test1())
sublattice_results2 <- system.time(test2())
sublattice_results3 <- system.time(test3())

sublattice_results1
sublattice_results2
sublattice_results3

####################################################
# PRUEBAS CON CONJUNTO DE ENTRADA BINARIO ALEATORIO
####################################################

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "sublattice"
######################################################################################
