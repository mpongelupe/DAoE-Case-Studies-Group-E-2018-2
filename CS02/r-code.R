## Dependencies for function to get directory
library(base)
library(rstudioapi)

## Function that gets the current directory
get_directory <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file <- "--file="
  rstudio <- "RStudio"
  
  match <- grep(rstudio, args)
  if (length(match) > 0) {
    return(dirname(rstudioapi::getSourceEditorContext()$path))
  } else {
    match <- grep(file, args)
    if (length(match) > 0) {
      return(dirname(normalizePath(sub(file, "", args[match]))))
    } else {
      return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
    }
  }
}


## Path of the files to be imported
directory <- get_directory()
input_file1 <- paste(directory, '/', '20162.csv', sep = '')
input_file2 <- paste(directory, '/', '20171.csv', sep = '')

## Importing files
turma1<- read.csv(file = input_file1, header = T)
turma2<- read.csv(file = input_file2, header = T)

turma1.ele <- turma1[ turma1$Course == 'PPGEE', c('Gender', 'Height', 'Weight')]

## Separating values
turma1.f <- turma1.ele[ turma1.ele$Gender == 'F', c('Height', 'Weight')]
turma1.m <- turma1.ele[ turma1.ele$Gender == 'M', c('Height', 'Weight')]

turma2.f <- turma2[ turma2$Gender == 'F', c('Height', 'Weight')]
turma2.m <- turma2[ turma2$Gender == 'M', c('Height', 'Weight')]


imc <- function(h, w) {
  return( w / ( h^2 ) )
}

applyIMC <- function(df) {
  df$IMC <- apply(df, 1, function (x) imc(x['Height'], x['Weight']))
  return(df)
}

##Calculating IMCs of populations
turma1.f <- applyIMC(turma1.f)
turma1.m <- applyIMC(turma1.m)
turma2.f <- applyIMC(turma2.f)
turma2.m <- applyIMC(turma2.m)

hist(turma2.f$IMC)