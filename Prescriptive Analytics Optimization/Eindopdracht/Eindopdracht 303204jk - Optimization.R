rm(list = ls())
suppressPackageStartupMessages(library(dplyr, quietly = TRUE)) 
suppressPackageStartupMessages(library(ROI))
library(magrittr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.gurobi)
source("readInstance.R")
source("printVector.R")

#1ABC: create MIP model for small and large instance dataset
#-------------------------


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

#MIP model
model <- MIPModel() %>%
  
  # define variables
  add_variable(x[i,j], i=1:n, j=1:n, type = "binary") %>%
  add_variable(y[j,k], j=1:n, k=1:p, type = "binary") %>%
  add_variable(e[j], j=1:n, type = "continuous") %>%
  add_variable(t[j], j=1:n, type = "continuous") %>%
  
  # Set objective function: minimize penalties
  set_objective(sum_over(weights[j]*e[j], j=1:n) 
                  , "min") %>%
  
  #Set constraints
  # Tardiness must be positive
  add_constraint(e[j]>=t[j]-deadlines[j], j = 1:n) %>%
  add_constraint(e[j]>=0, j=1:n) %>%
  
  #Finishing time of project j must be at least equal to the duration of project j that is assigned to person k
  add_constraint(t[j]>=t(durations)[j,k]*y[j,k], j=1:n,k=1:p) %>%
  
  # Finishing time of a project j must be at least equal to the finishing time of project i plus the duration of project j 
  #if project i is directly before j and the project is assigned to the same person.
  add_constraint(t[j]>=t[i] + t(durations)[j,k] - bigM * (3-x[i,j]-(y[i,k]+y[j,k])),i = 1:n, j = 1:n, i!=j, k=1:p)  %>%
  
  #If project i is before j, then j cannot be before i
  add_constraint(1==x[i,j]+x[j,i],i = 1:n, j = 1:n, i<j) %>%
  
  #Transitivity, if i is before j, and j is before l, then i must be before l
  add_constraint(x[i,l]>=x[i,j]+ x[j,l] - 1, i=1:n,j=1:n, l=1:n, i!=j, i!=l,j!=l) %>%
  
  #A project j can be assigned to at most 1 person
  add_constraint(1==sum_over(y[j,k],k=1:p), j = 1:n) %>%
  
  #set lower bounds
  set_bounds(e[j], j=1:n, lb=0) %>%
  set_bounds(t[j], j=1:n, lb=0)

model

#solve model with Gurobi
library("gurobi")
params <- list(OutputFlag=1) #Timelimit = 10*60)    #set solver options
result <- solve_model(model, with_ROI(solver = "gurobi", params))
sol_t=get_solution(result, t[j])
sol_e=get_solution(result, e[j])
sol_x=get_solution(result, x[i,j])
sol_y=get_solution(result, y[j,k])


#print output
cat("\nObjective value:",objective_value(result),'\n')
cat("Optimal solution:",'\n')
printVector("Finishing time projects: ",sol_t$value,1,n)
printVector("Tardiness: ",sol_e$value,1,n)
printVector("Order of projects: ",sol_x$value,1,n)
printVector("Assignment of projects: ",sol_y$value,1,n)


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

lambda1 <-c(0.95,0.05) #change in model objective to lambda1
lambda2 <-c(0.05,0.95) #change in model objective to lambda2


#create MIP model: # No epsilon constraint is added here!
model <- MIPModel() %>%
  
  # define variables
  add_variable(x[i,j], i=1:n, j=1:n, type = "binary") %>%
  add_variable(y[j,k], j=1:n, k=1:p, type = "binary") %>%
  add_variable(e[j], j=1:n, type = "continuous") %>%
  add_variable(t[j], j=1:n, type = "continuous") %>%
  add_variable(f, type = "continuous") %>%
  
  # Set Bi-objective function: minimization of total fine and makespan
  set_objective(lambda2[1]*sum_over(weights[j]*e[j], j=1:n) + lambda2[2] * f, "min") %>%
                
  #Set constraints
  
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
sol_f=get_solution(result, C)
sol_t=get_solution(result, t[j])
sol_e=get_solution(result, e[j])
sol_x=get_solution(result, x[i,j])
sol_y=get_solution(result, y[j,k])


#print output
cat("\nObjective value:",objective_value(result),'\n')
cat("Optimal solution:",'\n')
printVector("Finishing time projects: ",sol_t$value,1,n)
printVector("Tardiness: ",sol_e$value,1,n)
printVector("Order of projects: ",sol_x$value,1,n)
printVector("Assignment of projects: ",sol_y$value,1,n)

#2B: Non-dominated points for lambda1 (0.95,0.05) and lambda2 (0.05,0.95)
#--------------------------------

#g1: Objective - minimization of total fines (according to lambda 1)

#Initialize
g1 <- 0

for (i in 1:n) {
g1 <- g1 + weights[i]*sol_e$value[i]
}

#f1: Objective - minimization of makespan (according to lambda1)
f1<-as.numeric(sol_f)

#print output
cat("The objective values are: ", g1,"and", f1, '\n')

#------ second point

#g2: Objective - minimization of total fines (according to lambda 2)

#Initialize
g2 <- 0

for (i in 1:n) {
  g2 <- g2 + weights[i]*sol_e$value[i]
}

#f2: Objective - minimization of makespan (according to lambda2)
f2<-as.numeric(sol_f)

#print output
cat("The objective values are: ", g2,"and", f2, '\n')

#2C: Set of non-dominated points between points found in 2B using epsilon constraint method
#--------------------------------

#store points found in 2c
y1 <- c(g1,f1) #(19,30)
y2 <- c(g2,f2) #(31,25)

#Hardcopy solution from 2b
y1<-c(19,30)
y2 <-c(31,25)

# Try out different values for epsilon
nSteps<-y1[2] - y2[2]- 1
allResults <- matrix(0,nSteps,2);
iteration <- 0;

for(i in 1:nSteps) {
  eps<- y2[2]+ i
  lambda <- c(1,0) # Optimize the first objective while bounding the second to epsilon
  source('BiObjectiveModel.R') #Source model where epsilon constraint is added on second objective!
  iteration <- iteration + 1
  allResults[iteration, 1] <- g1;
  allResults[iteration, 2] <- f1;
} 


plot(allResults, type='o',xlab="Objective 1: Minimum Fine", ylab="Objective 2:Minimum makespan") 

#So, given that the objective values are integer values, the set of non-dominated points between the two points (y1 and y2) from 2B is 
#c(21,28) and c(22,26)

#Plot the set of non-dominated points including the points found in 2b. Add begin and end points.

non_dominated_points <- matrix(c(19,30,21,28,22,26,31,25),nrow=4, ncol=2, byrow=T)
plot(non_dominated_points,type='o',xlab="Objective 1: Minimum Fine", ylab="Objective 2:Minimum makespan")                     

#3A GRASP heuristic

#Create a table with per project j, the average completion time; its weights; and the ratio weights over the average completion time. 
#Then sort the projects based on this ratio with a decreasing order.

#Higher weights contribute more to the total fine.
#Early completion of projects is beneficial for subsequent projects (and meeting the corresponding deadlines) as it contributes many times.
#However, it could be the case that always choosing the best ratio at each iteration does not yield the overall best objective value as
#each project has different deadline (ie when two ratios are fairly close to each other, but the deadline of the slightly smaller ratio is way earlier and
#could in the end give a total fine that is larger). Therefore at a random component (GRASP).

#Start with project j which has the highest ratio. Then iteratively choose the next project based on a random component (grasp), 
#ie choose one of the two next best options randomly.

#3B Implementation of GRASP heuristic 

set.seed(112)

#Create table with average duration per project and ratio weight/average duration
ratio_table <- matrix(0,nProjects,6)

for (i in 1:nProjects) {
ratio_table[i,1] <- i #project i
ratio_table[i,2] <- t(durations)[i,1] #duration project i by first employee
ratio_table[i,3] <- t(durations)[i,2] #duration project i by second employee
ratio_table[i,4] <- (ratio_table[i,2] + ratio_table[i,3])/4 #average duration project i
ratio_table[i,5] <- weights[i] #weights project i
ratio_table[i,6] <- weights[i] / ratio_table[i,4] #ratios
}

#Unordered ratio table
ratio_table_orig <-ratio_table

#order table based on ratio (decreasing)
ratio_table <- ratio_table[order(ratio_table[,6],decreasing =T),]

#Initialize
nRepetitions <- 1000
repetition <-1
iter <- 1
bestOrder <- -1;
bestMinFine <- 999999

for(repetition in 1:nRepetitions)
{
  #Start with project that has the highest ratio
  completed <- rep(FALSE, nProjects) #projects 1-N sequential
  currentProject <- ratio_table[1,1]
  orderofProjects <- c(currentProject)
  completed[currentProject] <- TRUE
  
  for(iter in 1:(nProjects-1))
  {
    
    nOptions <- nProjects - iter #Projects remaining
    j <- 0  #initialize temp table index
    temp <- matrix(0, sum(completed==FALSE),2) # Create temporary table of all remaining options
    
    for(i in 1:nProjects) #Check if projects has already been completed
    {
      if( completed[i] == F) #if not, then make temporary table of all remaining projects with the corresponding ratio
      {
        
      #temp tables does not have same nr of indices as nProjects, so make it flexible with j
        j <- j+1;
        temp[j, 1] <- ratio_table_orig[i,1] #project nr (unordered)
        temp[j, 2] <- ratio_table_orig[i,6] #ratio
        
        # order temp table
        ordertemp <- temp[order(temp[,2],decreasing =T),] 
      }
         
    } 
    
        if (nOptions==1) {
          bestOption <- ordertemp[1] #only one project nr, (is now a vector instead of matrix)
          
        }
        
        else {
          
          #Determine number of options to choose from with a maximum of three
          
          nOptions <-0
          while(nOptions < nProjects - iter && nOptions <2)
           {nOptions <- nOptions + 1}
          
          # Create restricted candidate list of best next options (that have not been chosen yet with a maximum of three)
           rcl <- matrix(0, nOptions, 2)
           
           for (i in 1:nOptions) {
            rcl[i, 1] <- ordertemp[i,1] #project nr
            rcl[i, 2] <- ordertemp[i,2]
            }; #ratio
         
        
         #sample one project out of three in the rcl
          index <- sample.int(nOptions, 1, replace=TRUE)
          bestOption <- rcl[index,1] #project nr
          }
    
      # Add the selected project to the order
      completed[bestOption] <- TRUE
      orderofProjects[iter+1] <- bestOption
      
      # Update the current project
      currentProject <- bestOption 
    
  }
    
   
  TotalFine <- 0 # initialization total fine
  sumtime <- 0 #initialization cumulative sum of finishing times
  
  for(i in 1:nProjects) {
    
    #cumulative finishing time corresponding to iteration i (ie project i in the best order)
    sumtime <- sumtime + ratio_table_orig[orderofProjects[i],4]
    
    #If deadline is met, then no fine and total fine remain unchanged
    if ((sumtime - deadlines[orderofProjects[i]]) * weights[orderofProjects[i]]<0) {
      TotalFine <- TotalFine
      }
  else {
    #if deadline is not met, then add this fine to total fine
    TotalFine <- TotalFine + ((sumtime - deadlines[orderofProjects[i]]) * weights[orderofProjects[i]])
      }
    
    ###insert local search - See 3D 
    
    }

  if(bestOrder == -1 || TotalFine < bestMinFine)
  {
  bestMinFine <- TotalFine;
  bestOrder <- orderofProjects;
  
  }
}


# Print the objective
cat("Heuristic objective (NN - extension - randomized): ",bestMinFine,'\n',"Heuristic solution (NN - extension - randomized): ",bestOrder,'\n' )

###-----

##3c Neighborhood is every solution where any two projects are swapped in the initial solution. Look into the entire neighborhood



##3d Implementation local search heuristic

#Local search in each repetition of the grasp heuristic

#Same static data tables used as in 3B

set.seed(112)

#Initialize
nRepetitions <- 1000
repetition <-1
iter <- 1
bestOrder <- -1;
bestMinFine <- 999999

for(repetition in 1:nRepetitions)
{
  #Start with project that has the highest ratio
  completed <- rep(FALSE, nProjects) #projects 1-N sequential
  currentProject <- ratio_table[1,1]
  orderofProjects <- c(currentProject)
  completed[currentProject] <- TRUE
  
  for(iter in 1:(nProjects-1))
  {
    
    nOptions <- nProjects - iter #Projects remaining
    j <- 0  #initialize temp table index
    temp <- matrix(0, sum(completed==FALSE),2) # Create temporary table of all remaining options
    
    for(i in 1:nProjects) #Check if projects has already been completed
    {
      if( completed[i] == F) #if not, then make temporary table of all remaining projects with the corresponding ratio
      {
        
        #temp tables does not have same nr of indices as nProjects, so make it flexible with j
        j <- j+1;
        temp[j, 1] <- ratio_table_orig[i,1] #project nr (unordered)
        temp[j, 2] <- ratio_table_orig[i,6] #ratio
        
        # order temp table
        ordertemp <- temp[order(temp[,2],decreasing =T),] 
      }
      
    } 
    
    if (nOptions==1) {
      bestOption <- ordertemp[1] #only one project nr, (is now a vector instead of matrix)
      
    }
    
    else {
      
      #Determine number of options to choose from with a maximum of three
      
      nOptions <-0
      while(nOptions < nProjects - iter && nOptions <2)
      {nOptions <- nOptions + 1}
      
      # Create restricted candidate list of best next options (that have not been chosen yet with a maximum of three)
      rcl <- matrix(0, nOptions, 2)
      
      for (i in 1:nOptions) {
        rcl[i, 1] <- ordertemp[i,1] #project nr
        rcl[i, 2] <- ordertemp[i,2]
      }; #ratio
      
      
      #sample one project out of three in the rcl
      index <- sample.int(nOptions, 1, replace=TRUE)
      bestOption <- rcl[index,1] #project nr
    }
    
    # Add the selected project to the order
    completed[bestOption] <- TRUE
    orderofProjects[iter+1] <- bestOption
    
    # Update the current project
    currentProject <- bestOption 
    
  }
  
  
  TotalFine <- 0 # initialization total fine
  sumtime <- 0 #initialization cumulative sum of finishing times
  
  for(i in 1:nProjects) {
    
    #cumulative finishing time corresponding to iteration i (ie project i in the best order)
    sumtime <- sumtime + ratio_table_orig[orderofProjects[i],4]
    
    #If deadline is met, then no fine and total fine remain unchanged
    if ((sumtime - deadlines[orderofProjects[i]]) * weights[orderofProjects[i]]<0) {
      TotalFine <- TotalFine
    }
    else {
      #if deadline is not met, then add this fine to total fine
      TotalFine <- TotalFine + ((sumtime - deadlines[orderofProjects[i]]) * weights[orderofProjects[i]])
    }
  }
    ###insert local search
    
  for(i in 1:(nProjects-1))
    {
      for(j in (i+1):(nProjects))
      {
        
        #neighbor solution (swap  projects to get new order from initial order)
        Swaporder  <- replace(orderofProjects, c(i, j), orderofProjects[c(j, i)]) 
        
        TotalFineLS <- 0 # initialization total fine local search solution
        sumtimeLS <- 0 #initialization cumulative sum of finishing times local search solution
        
        for(i in 1:nProjects) {
          
          #cumulative finishing time corresponding to iteration i (ie project i in the best order)
          sumtimeLS <- sumtimeLS + ratio_table_orig[Swaporder[i],4]
          
          #If deadline is met, then no fine and total fine remain unchanged
          if ((sumtimeLS - deadlines[Swaporder[i]]) * weights[Swaporder[i]]<0) {
            TotalFineLS <- TotalFineLS
          }
          else {
            #if deadline is not met, then add this fine to total fine
            TotalFineLS <- TotalFineLS + ((sumtimeLS - deadlines[Swaporder[i]]) * weights[Swaporder[i]])
          }
        }
        
        #Is the LS solution better than the initial solution? Yes, then update.
        difference <- TotalFineLS - TotalFine;
        if(difference < 0)
        {
          bestOrderLS <- Swaporder
          bestMinFineLS <- TotalFineLS
        }
        
      }
    }
    
  
  if(bestOrder == -1 || bestMinFineLS < bestMinFine)
  {
    bestMinFine <- bestMinFineLS;
    bestOrder <- bestOrderLS;
    
  }
}


# Print the objective
cat("Heuristic objective (NN - extension - randomized including local search): ",bestMinFine,'\n',"Heuristic solution (NN - extension - randomized including local search): ",bestOrder,'\n' )

# The local search algorithm has improved the initial solutions for both instances. The new objective value in the small instance is equal
# to 58 where project 3 and 7 have been swapped. The objective value in the large instance has improved to 1028.5 where project 15 and 26 have been swapped.










