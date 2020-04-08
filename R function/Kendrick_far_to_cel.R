install.packages("devtools")
library(usethis)
library(devtools)
library(roxygen2)
library(testthat)
library(tidyr)

#This function is used to convert fahrenheit to celsius
#must input farenheit temperature into function


fahrenheit_to_celsius <- function(Far_Temp) {
  Cel_Temp <- (Far_Temp - 32) * 5 / 9
  return(Cel_Temp)
}
