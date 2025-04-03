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





generate_log_linear_model <- function(systems, var_matrix) {
  # Step 1: Within-system interactions - only the highest-order term
  within_system_terms <- apply(var_matrix, 1, function(vars) {
    vars <- vars[vars != ""]  # Remove empty slots (if any)
    if (length(vars) > 1) {
      paste(vars, collapse = "*")  # Keep only the highest-order term (interaction of all variables)
    } else if (length(vars) == 1) {
      return(vars)  # Just the variable if there's only one
    }
    return(NULL)  # No variables, no terms
  })
  
  # Remove NULLs (empty terms for systems without variables)
  within_system_terms <- within_system_terms[!sapply(within_system_terms, is.null)]

  # Step 2: Cross-system interactions - only if both systems have variables
  cross_system_terms <- c()
  for (i in seq_along(systems)) {
    for (j in seq_along(systems)) {
      if (i < j) {  # Ensure unique cross-system pairs
        vars_i <- var_matrix[i, var_matrix[i, ] != ""]
        vars_j <- var_matrix[j, var_matrix[j, ] != ""]
        
        if (length(vars_i) > 0 && length(vars_j) > 0) {
          cross_terms <- outer(vars_i, vars_j, paste, sep="*")
          cross_system_terms <- c(cross_system_terms, cross_terms)
        }
      }
    }
  }

  # Step 3: System presence interactions - for systems with variables
  system_presence_terms <- sapply(seq_along(systems), function(i) {
    sys <- systems[i]
    vars <- var_matrix[i, var_matrix[i, ] != ""]
    if (length(vars) > 0) paste(sys, "*", vars, collapse = "+") else NULL
  })
  
  # Remove NULLs (empty presence terms for systems without variables)
  system_presence_terms <- system_presence_terms[!sapply(system_presence_terms, is.null)]

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
