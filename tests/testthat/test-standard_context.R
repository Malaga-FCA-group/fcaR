test_that("Standard Context Generation Works", {
  attributes <- c("1", "2", "3", "4")
  
  # Implications:
  # 1 2 -> 3
  # 2 -> 4
  # 1 3 4 -> 2
  
  lhs <- Matrix::Matrix(0, nrow = 4, ncol = 3, sparse = TRUE)
  rhs <- Matrix::Matrix(0, nrow = 4, ncol = 3, sparse = TRUE)
  rownames(lhs) <- rownames(rhs) <- attributes
  colnames(lhs) <- colnames(rhs) <- paste0("I", 1:3)
  
  # I1: 1 2 -> 3
  lhs[c("1", "2"), 1] <- 1
  rhs["3", 1] <- 1
  
  # I2: 2 -> 4
  lhs["2", 2] <- 1
  rhs["4", 2] <- 1
  
  # I3: 1 3 4 -> 2
  lhs[c("1", "3", "4"), 3] <- 1
  rhs["2", 3] <- 1
  
  imps <- ImplicationSet$new(attributes = attributes, lhs = lhs, rhs = rhs)
  
  sc <- imps$get_standard_context()
  
  expect_s3_class(sc, "FormalContext")
  expect_equal(length(sc$objects), 5)
  expect_equal(length(sc$attributes), 4)
  
  # Meet-irreducibles should correspond to:
  # {3, 4}, {2, 4}, {2, 3, 4}, {1, 4}, {1, 3}
  # Check if these exact sets are present as objects (columns of incidence matrix)
  # Rows in sc$I correspond to attributes. Columns are objects.
  
  # Expected closed sets as columns (1=present, 0=absent)
  # Order: 1, 2, 3, 4
  expected_cols <- list(
    c(0, 0, 1, 1), # {3, 4}
    c(0, 1, 0, 1), # {2, 4}
    c(0, 1, 1, 1), # {2, 3, 4}
    c(1, 0, 0, 1), # {1, 4}
    c(1, 0, 1, 0)  # {1, 3}
  )
  
  # Check if each expected col exists
  I_mat <- as.matrix(sc$I)
  found_count <- 0
  for (expected in expected_cols) {
    # Find matching column
    match <- FALSE
    for (j in seq_len(ncol(I_mat))) {
      # Use identical or all.equal
      if (all(I_mat[, j] == expected)) {
        match <- TRUE
        break
      }
    }
    if (match) found_count <- found_count + 1
  }
  
  expect_equal(found_count, 5)
})
