#' Compute Closure of a (Fuzzy) Set
#'
#' This function is used to compute the closure...
#'
#' @param S      (a fuzzy set "dgCMatrix") Set to compute the closure.
#' @param LHS    (dGCMatrix) Matrix of lhs of implications
#' @param RHS
#' @param attributes   (character vector) Names of the attributes.
#' @param reduce
#' @param verbose
#'
#' @return
.compute_closure <- function(S, LHS, RHS, attributes, reduce = FALSE, verbose = FALSE) {

  if (is.null(LHS) || (ncol(LHS) == 0)) {

    if (!reduce) {

      return(S)

    } else {

      return(list(closure = S,
                  implications = list(lhs = LHS,
                                      rhs = RHS)))

    }

  }

  # Which are the rules applicable to the set S?
  S_subsets <- .subset(LHS, S)

  # idx_subsets <- which(S_subsets)
  idx_subsets <- S_subsets@i + 1

  do_not_use <- rep(FALSE, ncol(LHS))

  # While there are applicable rules, apply!!
  while (length(idx_subsets) > 0) {

    if (length(idx_subsets) == 1) {

      A <- Matrix(RHS[, idx_subsets], sparse = TRUE)

    } else {

      A <- RHS[, idx_subsets]

    }

    S <- .multiunion(add_col(A, S))

    if (verbose) {

      # cat("Reducing", length(idx_subsets), " rules\n")
      cat("Using rules:\n")
      imp <- implication_set$new(attributes = attributes,
                                 lhs = Matrix(LHS[, idx_subsets],
                                              sparse = TRUE),
                                 rhs = A)
      cat(imp$print())

    }

    do_not_use[idx_subsets] <- TRUE


    if (is.null(LHS) || (ncol(LHS) == 0)) {

      if (!reduce) {

        return(S)

      } else {

        return(list(closure = S,
                    implications = list(lhs = LHS,
                                        rhs = RHS)))
      }

    }

    if (reduce) {

      if (verbose) {

        # cat("Simplification stage\n")

      }

      C <- LHS
      D <- RHS

      CD <- .union(LHS, RHS)

      intersections <- .intersection(x = S, y = CD)
      idx_not_empty <- which(colSums(intersections) > 0)


      if (length(idx_not_empty) > 0) {

        C_B <- .difference(C[, idx_not_empty], S)

        D_B <- .difference(D[, idx_not_empty], S)

        idx_zeros <- which(colSums(D_B) == 0)

        if (verbose) {

          # cat("Reducing", length(idx_zeros), " rules:\n")
          # cat(" ** ", idx_not_empty, "\n")

        }

        if (length(idx_zeros) > 0) {

          C_B <- Matrix(C_B[, -idx_zeros], sparse = TRUE)
          D_B <- Matrix(D_B[, -idx_zeros], sparse = TRUE)

        }

        LHS <- cbind(C_B,
                     Matrix(C[, -idx_not_empty], sparse = TRUE))
        RHS <- cbind(D_B,
                     Matrix(D[, -idx_not_empty], sparse = TRUE))

      }

    }

    S_subsets <- .subset(LHS, S)

    idx_subsets <- S_subsets@i + 1
    idx_subsets <- setdiff(idx_subsets, which(do_not_use))

  }

  if (reduce) return(list(closure = S,
                          implications = list(lhs = LHS,
                                              rhs = RHS)))
  return(S)

}