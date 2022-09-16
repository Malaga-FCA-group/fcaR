library(fcaR)
library(bench)
library(profvis)
library(jointprof)
library(arules)
library(knitr)

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "supremum"
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

S <- fc_prueba$concepts$sub(6)

C <- fc_prueba$concepts[1:10]

test1 <- function() {
  fc_prueba$concepts$subconcepts(C)
}

test2 <- function() {
  fc_prueba_opt$concepts$subconcepts2(C)
}

test3 <- function() {
  fc_prueba_opt_binary$concepts$subconcepts_binary(C)
}

subconcepts_results1 <- system.time(test1())
subconcepts_results2 <- system.time(test2())
subconcepts_results3 <- system.time(test3())

subconcepts_results1
subconcepts_results2
subconcepts_results3

####################################################
# PRUEBAS CON CONJUNTO DE ENTRADA BINARIO ALEATORIO
####################################################

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "subconcepts"
######################################################################################


######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "superconcepts"
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

S <- fc_prueba$concepts$sub(6)

C <- fc_prueba$concepts[1:10]

test1 <- function() {
  fc_prueba$concepts$superconcepts(S)
}

test2 <- function() {
  fc_prueba_opt$concepts$superconcepts2(S)
}

test3 <- function() {
  fc_prueba_opt_binary$concepts$superconcepts_binary(S)
}

superconcepts_results1 <- system.time(test1())
superconcepts_results2 <- system.time(test2())
superconcepts_results3 <- system.time(test3())

superconcepts_results1
superconcepts_results2
superconcepts_results3

####################################################
# PRUEBAS CON CONJUNTO DE ENTRADA BINARIO ALEATORIO
####################################################


######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "superconcepts"
######################################################################################
