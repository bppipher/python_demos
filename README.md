generate_log_linear_model <- function(systems, var_matrix) {
  # Step 1: Within-system interactions
  within_system_terms <- apply(var_matrix, 1, function(vars) {
    vars <- vars[vars != ""]  # Remove empty slots (if any)
    if (length(vars) > 1) paste(vars, collapse = "*") else vars
  })
  
  # Step 2: Cross-system interactions
  all_vars <- split(var_matrix, row(var_matrix))  # Convert matrix to list
  cross_system_terms <- c()
  
  for (i in seq_along(systems)) {
    for (j in seq_along(systems)) {
      if (i < j) {  # Ensure unique cross-system pairs
        cross_terms <- outer(all_vars[[i]], all_vars[[j]], paste, sep="*")
        cross_system_terms <- c(cross_system_terms, cross_terms)
      }
    }
  }

  # Step 3: System presence interactions
  system_presence_terms <- sapply(seq_along(systems), function(i) {
    sys <- systems[i]
    vars <- var_matrix[i, var_matrix[i, ] != ""]
    if (length(vars) > 0) paste(sys, "*", vars, collapse = "+") else NA
  })
  system_presence_terms <- system_presence_terms[!is.na(system_presence_terms)]  # Remove NA

  # Step 4: Construct formula string
  all_terms <- c(within_system_terms, cross_system_terms, system_presence_terms)
  formula_str <- paste("~", paste(all_terms, collapse = " + "))

  return(as.formula(formula_str))
}

# Example usage with four systems, varying numbers of variables
systems <- c("A", "B", "C", "D")
var_matrix <- matrix(c(
  "X1", "X2", "X3",  # System A: Three variables
  "Y1", "Y2", "",    # System B: Two variables
  "Z1", "", "",     # System C: One variable
  "", "", "",      # System D: No variables
), nrow=4, byrow=TRUE)

generate_log_linear_model(systems, var_matrix)
