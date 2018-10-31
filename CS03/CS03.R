rm(list = ls())
library("smoof")

suppressPackageStartupMessages(library(smoof))

getSamples <- function(n,dim) {
  # This assingment makes fn a global variable
  fn <<- function(X){
    if(!is.matrix(X)) X <- matrix(X, nrow = 1) # <- if a single vector is passed as X
    Y <- apply(X, MARGIN = 1,
               FUN = smoof::makeRosenbrockFunction(dimensions = dim))
    return(Y)
  }

  selpars <- list(name = "selection_standard")
  stopcrit <- list(names = "stop_maxeval", maxevals = 5000 * dim, maxiter = 100 * dim)
  probpars <- list(name = "fn", xmin = rep(-5, dim), xmax = rep(10, dim))
  popsize = 5 * dim

  ## Config 1
  recpars1 <- list(name = "recombination_lbga")
  mutpars1 <- list(name = "mutation_rand", f = 4.5)

  ## Config 2
  recpars2 <- list(name = "recombination_blxAlphaBeta", alpha = 0.1, beta = 0.4)
  mutpars2 <- list(name = "mutation_rand", f = 3)

  library("ExpDE")

  suppressPackageStartupMessages(library(ExpDE))

  generate_sample <- function(mutpars, recpars, popsize, selpars, stopcrit, probpars) {
    return(ExpDE(mutpars = mutpars,
                 recpars = recpars,
                 popsize = popsize,
                 selpars = selpars,
                 stopcrit = stopcrit,
                 probpars = probpars,
                 showpars = list(show.iters = "dots", showevery = 20))$Fbest);
  }

  generate_n_samples <- function(n, mutpars, recpars) {
    return(replicate(n, generate_sample(mutpars, recpars, popsize, selpars, stopcrit, probpars)))
  }

  return(data.frame("A1" = generate_n_samples(n, mutpars1, recpars1), "A2" =  generate_n_samples(n, mutpars1, recpars2), "dim" = dim))

}

generateAllSamples <- function(n_samples, N) {
  rosenbrok_dim_interval <- 2:150
  dimensions <- sort(sample(rosenbrok_dim_interval, N))

  result <- lapply(dimensions, FUN = function(x) {
    write.csv(getSamples(n_samples, x), paste('samples-dim-', x, '.csv', sep = ''), row.names=FALSE)
  })
  write.csv(data.frame("dim" = dimensions), 'dimensions.csv', row.names=FALSE)
}

n_samples_per_instance <- 1
N <- 2

generateAllSamples(n_samples_per_instance, N)

dimensionsFile <- 'dimensions.csv'

dimensions <- read.csv(file = dimensionsFile, header = T)
dataFrame <- data.frame('A1' = character(), 'A2' = character(), 'dim' = character(), stringsAsFactors = FALSE)
lapply(dimensions$dim, function(x) {
  fileName <- paste('samples-dim-', x, '.csv', sep = '')
  data <- read.csv(file = fileName, header = T)
  dataFrame <<- rbind(dataFrame, data)
})


