rm(list = ls())
suppressPackageStartupMessages(library(dplyr, quietly = TRUE)) 
suppressPackageStartupMessages(library(ROI))
library(ompr)
library(ompr.roi)
library(ROI)
library(ROI.plugin.gurobi)

# Data
 i <- as.numeric()
# j <- as.numeric()
n <- 4
L1 <- c(5, 8, 3, 6)
L2 <- c(6, 7, 2, 5)
d <- c(6, 8, 8, 9)
w <- c(4, 5, 1, 0.2)

#create MIP model
model <- MIPModel() %>%
  # Decision variables
  add_variable(x[i,j], i = 1:n, j = 1:2, type = "binary") %>%
  # Objective function
  set_objective(sum_over(w[j]*(sum_over(L1[i]*sum_over(x[i,1], i = 1:j)) 
                             + sum_over(L2[i]*sum_over(x[i,2], i = 1:j)) - d[j]), 
                       j = 1:2)) %>%
  # Constraints
  add_constraint(sum_expr(x[i,1] + x[i,2], i = 1:n) == 1) %>%
  add_constraint(sum_expr(L1[i]*x[i,1], i = 1:n) + sum_expr(L2[i]*x[i,2], i = 1:n) <= sum_expr(d[i], i = 1:n)) 

library(gurobi)
# Solve the model using Gurobi
params <- list(OutputFlag=1)   
result <- solve_model(model, with_ROI(solver = "gurobi", params))

# Print the optimal solution
cat("Optimal objective value:", result$objective_value, "\n")
cat("Optimal solution:")
for (j in 1:2) {
  cat("\nEmployee", j, ":")
  for (i in 1:n) {
    if (result$solution_value(x[i,j]) > 0.5) {
      cat(" Project", i)
    }
  }
}
