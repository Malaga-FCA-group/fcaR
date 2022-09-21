.sampleImplicationSet <- function(nAttr, nImplications, sizeMaxLeft, sizeMaxRight){

  sample_lhs <- c()
  sample_rhs <- c()
  zeros_col <- rep(0,nAttr)

  for (index in 1:nImplications) {
    # Left Part
    # We pick a random number for the size between 1 and sizeMaxLeft
    szLeft <- sample(1:sizeMaxLeft,1)

    # Right Part
    # We pick a random number for the size between 1 and sizeMaxRight
    szRight <- sample(1:sizeMaxRight,1)

    # We generate the index where we are going to fill with 1s
    idx_Left  <- sample(1:nAttr,szLeft,  replace = FALSE) # No replacements
    idx_Right <- sample(1:nAttr,szRight, replace = FALSE)

    # We add 1s in the index
    zeros_aux <- zeros_col
    zeros_aux[idx_Left] <- rep(1,szLeft)
    sample_lhs <- c(sample_lhs, zeros_aux)

    zeros_aux <- zeros_col
    zeros_aux[idx_Right] <- rep(1,szRight)
    sample_rhs <- c(sample_rhs, zeros_aux)

  }

  sigma_lhs <- Matrix(as.numeric(sample_lhs), nrow=nAttr, ncol = nImplications, sparse = TRUE)
  sigma_rhs <- Matrix(as.numeric(sample_rhs), nrow=nAttr, ncol = nImplications, sparse = TRUE)
  attr <- LETTERS[1:nAttr]

  res <- ImplicationSet$new(lhs=sigma_lhs, rhs=sigma_rhs, attributes = attr)

}


.executionTime <- function(nAttr, nImplications, szLeftMax, szRightMax, nTimes){

  # We create the Sampled Implication Set
  ex1 <- .sampleImplicationSet(nAttr, nImplications, szLeftMax, szRightMax)

  timeList <- list()

  for (iter in 1:nTimes) {
    tictoc::tic()
    .slGetDo(sigma_lhs = ex1$get_LHS_matrix(), sigma_rhs = ex1$get_RHS_matrix(),
             attr = ex1$get_attributes() )
    time <- tictoc::toc()
    timeList <- append(timeList, time$toc - time$tic)
  }

  timeMin_DOB <- min(unlist(timeList))

  timeList <- list()

  for (iter in 1:nTimes) {
    tictoc::tic()
    .algorithm_FDB(sigma_lhs = ex1$get_LHS_matrix(), sigma_rhs = ex1$get_RHS_matrix(),
             attr = ex1$get_attributes() )
    time <- tictoc::toc()
    timeList <- append(timeList, time$toc - time$tic)
  }

  timeMin_FDOB <- min(unlist(timeList))

  return(list(timeMin_DOB, timeMin_FDOB))

}

# We import the libraries
library(usethis)
library(devtools)
# Command Line = devtools::load_all()
library(Matrix)
library("xlsx")
# A <- .sampleImplicationSet(6, 15, 3, 2)

########################################################
# Example 1 (Number of Attributes = 10, Number of
# Implications = 50, Max size Left Part = 6, Max size
# Right Part = 4)
########################################################

nAttr <- 10
nImplications <- 50
szLeftMax <- 6
szRightMax <- 4
nTimes <- 1

times <- .executionTime(nAttr,nImplications,szLeftMax,szRightMax,nTimes)

df <- data.frame(
  "Number of Attributes"              = nAttr,
  "Number of Implications"            = nImplications,
  "Max Size Left Part"                = szLeftMax,
  "Max size Right Part"               = szRightMax,
  "Time with DOB Algorithm (in seg)"  = times[[1]],
  "Time with FDOB Algorithm (in seg)" = times[[2]]
)

########################################################
# Example 2 (Number of Attributes = 10, Number
# of Implications = 150)
########################################################

nImplications <- 150

times <- .executionTime(nAttr,nImplications,szLeftMax,szRightMax,nTimes)

df[nrow(df) + 1,] <- c(nAttr, nImplications, szLeftMax, szRightMax, times[[1]], times[[2]])

########################################################
# Example 3 (Number of Attributes = 10, Number of
# Implications = 500)
########################################################

nImplications <- 500

times <- .executionTime(nAttr,nImplications,szLeftMax,szRightMax,nTimes)

df[nrow(df) + 1,] <- c(nAttr, nImplications, szLeftMax, szRightMax, times[[1]], times[[2]])

########################################################
# Example 4 (Number of Attributes = 10, Number of
# Implications = 5000)
########################################################

nImplications <- 5000

times <- .executionTime(nAttr,nImplications,szLeftMax,szRightMax,nTimes)

df[nrow(df) + 1,] <- c(nAttr, nImplications, szLeftMax, szRightMax, times[[1]], times[[2]])

########################################################
# Example 5 (Number of Attributes = 10, Number of
# Implications = 10000)
########################################################

nImplications <- 10000

times <- .executionTime(nAttr,nImplications,szLeftMax,szRightMax,nTimes)

df[nrow(df) + 1,] <- c(nAttr, nImplications, szLeftMax, szRightMax, times[[1]], times[[2]])

dfTotal <- df

View(dfTotal)
