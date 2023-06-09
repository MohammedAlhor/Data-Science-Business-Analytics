suppressPackageStartupMessages(library(dplyr, quietly = TRUE)) 
suppressPackageStartupMessages(library(ROI))
library(magrittr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.gurobi)

#2A: create Bi-objective MIP model for small instance dataset 
#--------------------------------

#Data (sourced)
nProjects
durations
deadlines
weights

#define parameters
n=nProjects;   #number of projects
p=2    #number of persons


#BigM
max_length <- max(sum(t(durations)[,1]),sum(t(durations)[,2]))
min_length <- min(t(durations))

bigM<-max_length-min_length

#create MIP model: Epsilon constraint is added!
model <- MIPModel() %>%
  
  # define variables
  add_variable(x[i,j], i=1:n, j=1:n, type = "binary") %>%
  add_variable(y[j,k], j=1:n, k=1:p, type = "binary") %>%
  add_variable(e[j], j=1:n, type = "continuous") %>%
  add_variable(t[j], j=1:n, type = "continuous") %>%
  add_variable(f, type = "continuous") %>%
  
  # Set Bi-objective function: minimization of total fine and makespan
  set_objective(lambda[1]*sum_over(weights[j]*e[j], j=1:n) + lambda[2] * f, "min") %>%
                
  #Set constraints
  
  #Impose epsilon constraint on second objective (makespan)
  add_constraint(f<=eps) %>%
  
  # Tardiness must be positive
  add_constraint(e[j]>=t[j]-deadlines[j], j = 1:n) %>%
  add_constraint(e[j]>=0, j=1:n) %>%
  
  #The makespan f is the largest completion time of all projects
  add_constraint(f>=t[j], j=1:n) %>%
  
  #Finishing time of project j must be at least equal to the duration of project j that is assigned to person k
  add_constraint(t[j]>=t(durations)[j,k]*y[j,k], j=1:n,k=1:p) %>%
  
  # Finishing time of a project j must be at least equal to the finishing time of project i that is directly before j plus the duration of project j,
  #if the project is assigned to the same person
  add_constraint(t[j]>=t[i] + t(durations)[j,k] - bigM * (3-x[i,j]-(y[i,k]+y[j,k])),i = 1:n, j = 1:n, i!=j, k=1:p)  %>%
  
  #If project i is before j, then j cannot be before i
  add_constraint(1==x[i,j]+x[j,i],i = 1:n, j = 1:n, i<j) %>%
  
  #Transitivity, if i is before j, and j is before k, then i must be before k
  add_constraint(x[i,l]>=x[i,j]+ x[j,l] - 1, i=1:n,j=1:n, l=1:n, i!=j, i!=l,j!=l) %>%
  
  #A project j can be assigned to at most 1 person
  add_constraint(1==sum_over(y[j,k],k=1:p), j = 1:n) %>%
  
  #set lower bounds
  set_bounds(e[j], j=1:n, lb=0) %>%
  set_bounds(f, lb=0) %>%
  set_bounds(t[j], j=1:n, lb=0)

model

#solve model with Gurobi
library("gurobi")
params <- list(OutputFlag=1)    #set solver options
result <- solve_model(model, with_ROI(solver = "gurobi", params))
sol_f=get_solution(result, f)
sol_t=get_solution(result, t[j])
sol_e=get_solution(result, e[j])
sol_x=get_solution(result, x[i,j])
sol_y=get_solution(result, y[j,k])


#2B: Non-dominated points for lambda1 (0.95,0.05) and lambda2 (0.05,0.95)
#--------------------------------

#g1: Objective - minimization of total fines (according to lambda)

#Initialize
g1 <- 0

for (i in 1:n) {
g1 <- g1 + weights[i]*sol_e$value[i]
}

#f1: Objective - minimization of makespan (according to lambda1)
f1<-as.numeric(sol_f)

#print output
cat("The objective values are: ", g1,"and", f1, '\n')
#------

#g2: Objective - minimization of total fines (according to lambda)

#Initialize
g2 <- 0

for (i in 1:n) {
  g2 <- g2 + weights[i]*sol_e$value[i]
}

#2C: Set of non-dominated points betweeen points found in 2C using epsilon constraint method
#--------------------------------

#store points found in 2c
y1 <- c(g1,f1)
y2 <- c(g2,f2)



