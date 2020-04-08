install.packages("devtools")
library(usethis)
library(devtools)
#devtools::install_github("klutometis/roxygen")
library(roxygen2)
library(testthat)
library(tidyr)

#This function called convert_in_to_cm will convert inches into cm
# input inches into function


convert_in_t0_cm <- function(inch) {
  centimeter <- (inch * 2.54)
  return(centimeter)
}


