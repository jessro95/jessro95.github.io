# Load the libraries into R - needs to be done each time you want to use the library
library(dplyr) 
library(stringr)
library(ggplot2)
library(plotly)

#load data
race.data <- read.csv('./data/prepped/baltimore_race.csv')

dissimilarity <- function(data) {
  ti <- data$pop
  pi <- (data$pop.not.white / ti)
  Tsum <- sum(ti)
  P <- sum(data$pop.not.white)/Tsum
  
  numerator <- sum(ti * abs(pi - P))
  denominator <- (2 * Tsum * P * (1 - P))
  return (numerator / denominator)
  
}

interaction <- function(data){
  xi <- data$pop.not.white
  yi <- data$pop.white
  X <- sum(xi)
  ti <- data$pop
  
  return(sum((xi/X)*(yi/ti)))
}

isolation <- function(data){
  xi <- data$pop.not.white
  X <- sum(xi)
  ti <- data$pop
  
  return(sum((xi/X) * (xi/ti)))
}

correlation <- function(data){
  I <- isolation(data)
  Tsum <- sum(data$pop)
  P <- sum(data$pop.not.white)/Tsum
  return((I-P)/(1-P))
}

newMatric <- function(data){
  d <- dissimilarity(data)
  c <- correlation(data)
  return ((d+c)/2)
}