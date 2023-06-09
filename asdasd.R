rm(list = ls())
suppressPackageStartupMessages(library(dplyr, quietly = TRUE)) 
suppressPackageStartupMessages(library(ROI))
library(ompr)
library(ompr.roi)
library(ROI.plugin.gurobi)

# Define the input data
L <- matrix(c(7, 10, 6, 9, 5, 1, 1, 2, 8, 6, 
              2, 1, 10, 10, 7, 8, 1, 6, 1, 8), nrow = 2, byrow = TRUE)
d <- c(13, 7, 14, 10, 16, 16, 14, 11, 7, 20)
w <- c(2, 4, 1, 3, 4, 5, 1, 3, 4, 1)
n <- 10
m <- 2

# Define the MIP model
model <- MIPModel() %>%
  # Add decision variables
  add_variable(x[j,i], i = 1:m, j = 1:n, type = "binary") %>%
  add_variable(t[j], j = 1:n, lb = 0) %>%
  add_variable(z[j], j = 1:n, type = "binary") %>%
  # Set objective function
  set_objective(sum_expr(w[j] * z[j], j = 1:n), "min") %>%
  # Add constraints
  add_constraint(sum_expr(x[j,i], i = 1:m) == 1, j = 1:n) %>%
  add_constraint(t[j] >= sum_expr(L[i,j] * x[j,i], i = 1:m), j = 1:n) %>%
  add_constraint(t[k] >= t[j] + L[i,j] * x[k,i], i = 1:m, j = 1:(n-1), k = 2:n) %>%
  add_constraint(t[j] - d[j] <= z[j] * 1, j = 1:n)

model



params <- list(OutputFlag=1)    #set solver options
result <- gurobi(model, params)



library(gurobi)

# Set Gurobi solver and parameters
# model <- model %>% 
#   set_solver("gurobi") %>% 
#   add_solver_opts("OutputFlag" = 1)

#solve model with Gurobi
library("gurobi")
params <- list(OutputFlag=1)    #set solver options
result <- solve_model(model, with_ROI(solver = "gurobi", params))

# Solve the model
result <- solve_model(model)

# Print the results
if (result$status == "optimal") {
  cat("Optimal objective value:", result$objective_value, "\n")
  for (j in 1:n) {
    for (i in 1:m) {
      if (result$solution(x[j,i]) > 0.5) {
        cat("Project", j, "assigned to employee", i, "\n")
      }
    }
  }
  for (j in 1:n) {
    cat("Finishing time for project", j, ":", result$solution(t[j]), "\n")
  }
} else {
  cat("No solution found.")
}
