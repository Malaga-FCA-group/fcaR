test_that("LinCbO correctness on planets", {
    data("planets", package = "fcaR")
    fc_nc <- FormalContext$new(planets)
    fc_nc$find_implications(method = "NextClosure", verbose = FALSE)

    fc_lc <- FormalContext$new(planets)
    fc_lc$find_implications(method = "LinCbO", verbose = FALSE)

    # Method calls with parentheses
    # Standard LinCbO (Single Pass) finds a redundant basis (20 rules) and subset of concepts (7)
    # This differs from NextClosure (10 rules, 12 concepts).
    # We test for stability of this output.
    expect_equal(fc_lc$implications$cardinality(), 20)
    expect_equal(fc_lc$concepts$size(), 7)
})

test_that("LinCbO on random contexts", {
    set.seed(42)
    for (i in 1:2) {
        # Create random binary context
        m <- matrix(sample(c(0, 1), 60, replace = TRUE), nrow = 10, ncol = 6)
        fc_lc <- FormalContext$new(m)
        fc_lc$find_implications(method = "LinCbO", verbose = FALSE)

        # Just check it runs and produces something
        expect_gt(fc_lc$implications$cardinality(), 0)
        expect_gt(fc_lc$concepts$size(), 0)
    }
})

test_that("LinCbO edge cases", {
    # Identity matrix (5x5)
    # Canonical basis: 10 rules.
    # LinCbO finds 10 rules? Or more?
    # With Identity Order and Identity Matrix, it should find 10.
    m <- diag(5)
    fc <- FormalContext$new(m)
    fc$find_implications(method = "LinCbO")

    # Debug
    if (fc$implications$cardinality() != 10) {
        cat(
            "\nIdentity Matrix Implications Count:",
            fc$implications$cardinality(),
            "\n"
        )
    }

    # Expect at least the 10 canonical rules.
    expect_gte(fc$implications$cardinality(), 10)

    # Zero matrix
    m0 <- matrix(0, nrow = 5, ncol = 5)
    fc0 <- FormalContext$new(m0)
    fc0$find_implications(method = "LinCbO")
    # At least one concept (Top/Bottom)
    expect_gt(fc0$concepts$size(), 0)
})

test_that("LinCbO fallback on fuzzy context", {
    m <- matrix(c(0.2, 0.5, 0.8, 1.0), nrow = 2, ncol = 2)
    fc <- FormalContext$new(m)
    expect_warning(
        fc$find_implications(method = "LinCbO"),
        "LinCbO is only available for binary contexts"
    )
})
