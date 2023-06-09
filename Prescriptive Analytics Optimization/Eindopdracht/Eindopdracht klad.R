# inladen data.
# Specify the file to read
#fileName = "example.txt";
fileName = "small.txt";
# fileName = "large.txt";

# Read the number of jobs
nProjects = scan(fileName, skip = 1, nlines =  1);

# Read the properties of the jobs
input = scan(fileName, skip = 3, nlines = nProjects);
durations = matrix(-1, 2, nProjects);
weights = rep(-1, nProjects);
deadlines = rep(-1, nProjects)
index = 1;
for(j in 1:nProjects)
{
  durations[1, j]= input[index];
  durations[2, j]= input[index + 1];
  deadlines[j] = input[index + 2];
  weights[j] = input[index + 3];
  index = index + 4;
}

rm(index, input, j, fileName)

##########################################################


# Load Gurobi library
#rm(list = ls())
suppressPackageStartupMessages(library(dplyr, quietly = TRUE)) 
suppressPackageStartupMessages(library(ROI))
library(magrittr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.gurobi)


#create MIP model
model <- MIPModel() %>%
  # define variables
  add_variable(x[i,j], i=1:n, j=1:n, type = "binary") %>% # Als de ene werknemer aan het project zit, dan kan de andere hier niet aan zitten
  add_variable(t[j], j=1:n, type = "continuous") %>% # de tijd dat het kost om een project af te ronden.
  
  # minimize total duration
  set_objective(sum_over(duration[i,j]*x[i,j], i=1:m, j=1:n) + 
                  sum_over(setupTime[i]*y[i], i=1:m) +
                  extraTime12*z, "min") %>%

  
  



# Create Gurobi model
model <- list()

# Add variables
for (i in 1:2) {
  for (j in 1:n) {
    model[[paste0("x", i, j)]] <- gurobi(model, lb = 0, ub = 1, vtype = "B", name = paste0("x", i, j))
  }
}
for (j in 1:n) {
  model[[paste0("t", j)]] <- gurobi(model, lb = 0, name = paste0("t", j))
  model[[paste0("y", j)]] <- gurobi(model, vtype = "B", name = paste0("y", j))
}

# Set objective
obj <- quicksum(w[j] * model[["y" (j)]] * (model[["t" (j)]] - d[j]) for j in 1:n)
model$obj <- obj

# Add constraints
for (j in 1:n) {
  model <- addConstr(model, quicksum(model[["x1" (j)]], model[["x2" (j)]]) == 1, name = paste0("project", j))
  if (j > 1) {
    model <- addConstr(model, model[["t" (j)]] - model[["t" (j-1)]] >= L1[j-1] * model[["x1" (j-1)]] + L2[j-1] * model[["x2" (j-1)]], name = paste0("finish_time", j))
  }
  model <- addConstr(model, model[["t" (j)]] - d[j] <= M * model[["y" (j)]], name = paste0("deadline", j))
}

# Set Gurobi parameters
params <- list(OutputFlag = 1)

# Optimize model
result <- gurobi(model, params)

# Print solution
cat("Optimal objective value:", result$objval, "\n")
cat("Optimal assignment of projects to employees:\n")
for (j in 1:n) {
  for (i in 1:2) {
    if (result[[paste0("x", i, j)]]$x > 0.5) {
      cat("Project", j, "assigned to employee", i, "\n")
      break
    }
  }
}
cat("Optimal order of projects:\n")
for (j in 1:n) {
  cat("Project", which(result$x[paste0("t", 1:n)] == j), "\n")
}
