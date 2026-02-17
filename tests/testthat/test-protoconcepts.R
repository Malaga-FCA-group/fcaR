context("protoconcepts")

test_that("find_protoconcepts works for a small context", {
  # Diagonal context 3x3
  I <- matrix(c(1, 0, 0,
                0, 1, 0,
                0, 0, 1), nrow = 3, byrow = TRUE)
  fc <- FormalContext$new(I)
  
  # Compute protoconcepts
  P <- fc$find_protoconcepts()
  
  # Check count
  expect_equal(length(P), 11)
  
  # Verify one specific non-concept: ({1,2}, {})
  # A={1,2} -> A'={} -> A''={1,2,3}. 
  # B={} -> B'={1,2,3} -> B''={} (if we consider attribute closure)
  # Wait, logic check:
  # A={1,2}. A'={}.
  # B={}. B''={}.
  # So A'=B'' match.
  # Pair is ({1,2}, {}).
  # Extent: {1,1,0} (objects 1,2). Intent: {0,0,0} (empty).
  
  found <- FALSE
  for (i in seq_along(P)) {
    p <- P[[i]]
    
    # helper to safely get vector from Set
    # get_vector() returns sparse matrix. as.vector might fail if matrix is weird.
    # explicit conversion to matrix first
    v_ext <- as.matrix(p$extent$get_vector())
    v_int <- as.matrix(p$intent$get_vector())
    
    ext <- as.vector(v_ext)
    int <- as.vector(v_int)
    
    # Check ext={1,1,0}, int={0,0,0}
    if (length(ext) == 3 && length(int) == 3 &&
        all(ext == c(1,1,0)) && all(int == c(0,0,0))) {
      found <- TRUE
      break
    }
  }
  
  expect_true(found)
})
