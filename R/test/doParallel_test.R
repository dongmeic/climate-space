library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

foreach(i=1:28)%dopar%{
  print(i)
}
print("all done")