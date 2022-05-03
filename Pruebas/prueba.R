## Prueba Inicial
#install.packages("usethis")
library(usethis)
#install.packages("devtools")
library(devtools)
#install.packages("fcaR", dependencies = TRUE)
library(fcaR)
library(ggplot2)
#install.packages("useful", dependencies = TRUE)
library(useful)
library(Matrix)
library(R6)


# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("Rgraphviz")

# Te mete en memoria todo lo del paquete, no tienes que estar instalando
# y reiniciando sesion. IMPORTANTE HACERLO DESDE LA CONSOLA.
# load_all()



A <- planets
A

# Creamos matriz dispersa
A_Dispersa <- Matrix(A, sparse = TRUE)

dim(A_Dispersa) == dim(A_Dispersa) | all(A_Dispersa == A_Dispersa)
X <- list(A_Dispersa)
Y <- list(A_Dispersa)
all(X==Y)
library(useful)
useful::compare.list(X,Y)


for (ind in 1:0){
  print(ind)
}

length(NULL)

# STR = func que muestra de una variable todos los campos internos
# str(A_Dispersa)

# small medium large near far moon no_moon
# Mercury     1      .     .    1   .    .       1
# Venus       1      .     .    1   .    .       1
# Earth       1      .     .    1   .    1       .
# Mars        1      .     .    1   .    1       .
# Jupiter     .      .     1    .   1    1       .
# Saturn      .      .     1    .   1    1       .
# Uranus      .      1     .    .   1    1       .
# Neptune     .      1     .    .   1    1       .
# Pluto       1      .     .    .   1    1       .


# Formal class 'dgCMatrix' [package "Matrix"] with 6 slots
# ..@ i       : int [1:27] 0 1 2 3 8 6 7 4 5 0 ...
# ..@ p       : int [1:8] 0 5 7 9 13 18 25 27
# ..@ Dim     : int [1:2] 9 7
# ..@ Dimnames:List of 2
# .. ..$ : chr [1:9] "Mercury" "Venus" "Earth" "Mars" ...
# .. ..$ : chr [1:7] "small" "medium" "large" "near" ...
# ..@ x       : num [1:27] 1 1 1 1 1 1 1 1 1 1 ...
# ..@ factors : list()

# Acceso a Datos A_Dispersa@i
# Atributo i: Te dice "vectores de donde estan los 1" ¿Como se cuando acaba un
# vector y empieza el siguiente? Se sabe gracias a p.
# Atributo p: Te dice en que posicion del vector i comienza y acaba la columna.
# Atributo x: me dice que valores están en las posiciones de la i (En este caso 1s)

# Ejemplo: En la columna small que seria el vector [1,1,1,1,.,.,.,.,1],
# i me dice: en la posicion 0,1,2,3,8 tienes un 1 (cualquier término no solo 1, aparecera en x)
# p me dice: desde la posicion 0 hasta la posicion 5-1, tienes el primer vector.
# x me dice: 1s

# Acceso a elementos
# Posicion directa (Gracias a paquete implementado en c) A_Dispersa[5,4]
# Acceso a fila A_Dispersa[2,]
# Acceso a columna A_Dispersa[,3]

# Gran Problema a tener en cuenta: si obtenemos una fila o una columna, el resultado
# es una matriz normal, no una sparse, por lo que hay que forzar los cambios.
Matrix(A_Dispersa[,3], sparse = TRUE)

# Inicializar un conjunto
# Inicializar un set es indicar los elementos que contienen, pero cada uno inicializado
# a valor 0
A <- Set$new(attributes = LETTERS[1:3])

# Para asignar un valor realizamos assign
A$assign(A=0.75,B=1,C=0.5)

B <- Set$new(attributes = LETTERS[1:2])
B$assign(A=0.5,B=0.3)

C <- Set$new(attributes = LETTERS[1:3])
C$assign(A=0.5,B=0.3)

# Subconjunto de otro (IMPORTANTE: Tienen que tener el mismo tamaño)
# Compara elemento a elemento, con que una sea mayor ya es falso
# C$assign(A=0.5,B=1.5)
C %<=% A

## Repaso de Conceptos Formales
fc <- FormalContext$new(planets)

# Matriz transpuesta de la matriz de los planetas
fc$I

# Encontrar el conjunto de implicaciones
fc$find_implications()
implicaciones <- fc$implications
implicaciones$is_empty()
A <-implicaciones[1]$get_RHS_matrix()
B <-implicaciones[3]$get_LHS_matrix()
implicaciones$add(A,A)
C <- append(C,B )
str(C)

Delocos <- cbind(implicaciones$get_LHS_matrix(), A)
Delocos <- Matrix(Delocos[,12],sparse=TRUE)
.matrixEquals(Delocos, A)

sum((A == B)@x) == length((A == B)@x)
#.matrixEquals(A,B)
library(useful)
.matrixEquals(implicaciones$get_LHS_matrix(), implicaciones$get_RHS_matrix())

A * B
sum(A*B)



# Obtener la matriz de las partes izquierdas
lh <- implicaciones$get_LHS_matrix()
# small   . . . . . . 1 1 1 1
# medium  . . . . 1 1 . . . 1
# large   . . . 1 . 1 . . 1 .
# near    . . 1 . . . 1 1 . .
# far     . 1 . . . 1 . 1 1 1
# moon    . . . . . 1 1 1 1 1
# no_moon 1 . . . . . 1 . . .

# La regla 1 (parte izquierda) se representa en la matriz como
# la columna 1 donde la posicion 1 es la que hay y el . no

## Aplicacion
S <- Set$new(attributes = fc$attributes)
S$assign(large=1)
S

# Cierre de fc
fc$implications$closure(S)

.subset(fc$implications$get_LHS_matrix(), S$get_vector())

.equal_sets(lh,lh)
