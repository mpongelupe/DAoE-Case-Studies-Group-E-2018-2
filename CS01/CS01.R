library(ExpDE)
mre <- list(name = "recombination_bin", cr = 0.9) 
mmu <- list(name = "mutation_rand", f = 2)
mpo <- 100
mse <- list(name  = "selection_standard")
mst <- list(names = "stop_maxeval", maxevals = 10000)
mpr <- list(name  = "sphere", xmin  = -seq(1, 20), xmax  = 20 + 5 * seq(5, 24))
set.seed(1998)  # <--- ATTENTION: USE THE BIRTH YEAR OF YOUNGEST TEAM MEMBER


generate_sample <- function() {
  return(ExpDE(mpo, mmu, mre, mse, mst, mpr,
               showpars = list(show.iters = "none"))$Fbest);
}


generate_n_samples <- function(n) {
  return(replicate(n, generate_sample()))
}

#######
## Definitions of the problem
#######

alpha <- 0.01
beta <- 0.2
N <- 15

samples <- generate_n_samples(N)

samples
