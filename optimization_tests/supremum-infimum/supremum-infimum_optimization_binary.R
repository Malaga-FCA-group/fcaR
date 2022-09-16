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
  fc_prueba$concepts$supremum(C)
}

test2 <- function() {
  fc_prueba_opt$concepts$supremum2(C)
}

test3 <- function() {
  fc_prueba_opt_binary$concepts$supremum_binary(C)
}

supremum_results1 <- system.time(test1())
supremum_results2 <- system.time(test2())
supremum_results3 <- system.time(test3())

supremum_results1
supremum_results2
supremum_results3

####################################################
# PRUEBAS CON CONJUNTO DE ENTRADA BINARIO ALEATORIO
####################################################

######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "supremum"
######################################################################################


######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "infimum"
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
  fc_prueba$concepts$infimum(C)
}

test2 <- function() {
  fc_prueba_opt$concepts$infimum2(C)
}

test3 <- function() {
  fc_prueba_opt_binary$concepts$infimum_binary(C)
}

supremum_results1 <- system.time(test1())
supremum_results2 <- system.time(test2())
supremum_results3 <- system.time(test3())

supremum_results1
supremum_results2
supremum_results3

####################################################
# PRUEBAS CON CONJUNTO DE ENTRADA BINARIO ALEATORIO
####################################################


######################################################################################
#                   ANÁLISIS DE RENDIMIENTO ----->     "infimum"
######################################################################################
