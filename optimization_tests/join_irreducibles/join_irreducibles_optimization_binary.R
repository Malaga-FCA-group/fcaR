library(fcaR)
library(bench)
library(profvis)
library(jointprof)
library(arules)
library(knitr)

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "join_irreducibles"
######################################################################################

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

test1 <- function() {
  fc_prueba$concepts$join_irreducibles()
}

test2 <- function() {
  fc_prueba_opt$concepts$join_irreducibles2()
}

test3 <- function() {
  fc_prueba_opt_binary$concepts$join_irreducibles_binary()
}

join_irreducibles_results1 <- system.time(test1())
join_irreducibles_results2 <- system.time(test2())
join_irreducibles_results3 <- system.time(test3())

join_irreducibles_results1
join_irreducibles_results2
join_irreducibles_results3

####################################################
# PRUEBAS CON CONJUNTO DE ENTRADA BINARIO ALEATORIO
####################################################

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "join_irreducibles"
######################################################################################
