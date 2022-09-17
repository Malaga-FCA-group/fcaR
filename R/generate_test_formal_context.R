generate_context <- function(n_objects = 500,
                             n_attributes = 20,
                             grades = c(0, 1),
                             density = 0.4) {



  grades <- grades[grades > 0]
  v <- vector(mode = "numeric",
              length = n_objects * n_attributes)
  idx <- sample.int(n = n_objects * n_attributes,
                    size = round(density * n_objects * n_attributes))



  vals <- sample(grades,
                 size = round(density * n_objects * n_attributes),
                 replace = TRUE)



  v[idx] <- vals
  I <- matrix(v, nrow = n_objects, ncol = n_attributes)
  colnames(I) <- paste0("A", seq(n_attributes))
  rownames(I) <- paste0("O", seq(n_objects))
  return(I)

}

