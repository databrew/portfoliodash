#! /usr/bin/R
# reconstruct_data <- FALSE
library(devtools)
library(roxygen2)
library(rmarkdown)
document('.')
install('.')
# render('README.Rmd')
# setwd('vignettes')
# render('vignette.Rmd')
# setwd('..')
# if(reconstruct_data){
#   setwd('data-raw')
#   source('create_data_files.R')
#   setwd('..')
# }
