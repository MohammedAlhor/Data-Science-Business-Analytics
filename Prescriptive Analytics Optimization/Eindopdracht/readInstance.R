# Specify the file to read
#fileName = "example.txt";
fileName = "small.txt";
#fileName = "large.txt";

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
