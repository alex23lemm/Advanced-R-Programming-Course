# Author: Alex Lemm
# Repo: https://github.com/alex23lemm/Advanced-R-Programming-Course

# Load libraries ---------------------------------------------------------------

library(purrr)
library(microbenchmark)

# Define functions -------------------------------------------------------------

factorial_loop <- function(x) {
  if (x == 0 || x == 1)
    return(1)
  for (i in (x - 1):1) {
    x <- x * i
  }
  x
}

factorial_reduce <- function(x) {
  if (x == 0)
    return(1)
  reduce(1:x, `*`)
}

factorial_func <- function(x) {
  if (x == 0)
    return(1)
  x * factorial_func(x - 1)
}


# Create lookup table for memoization. Unlike in the Fibonacci example,
# memoization does not make sense for individual function calls here because
# recursive factorial calls never occur more than once for a specific input
# value.
# Therefore, efficiency is only gained when calculating factorials for 
# subsequent input values
fact_tbl <- c(rep(NA, 65))

factorial_mem <- function(x) {
  if (x == 0)
    return(1)
  if (!is.na(fact_tbl)[x])
    return(fact_tbl[x])
  fact_tbl[x] <<- x * factorial_mem(x - 1)
  fact_tbl[x]
}
    


# Test functions ---------------------------------------------------------------

input <- c(0, 1, 6, 11, 13,  45, 63)

# Check if all functions produce the same results. R's built-in function
# factorial() is used to compare the results
factorial(input)
map_dbl(input, factorial_loop)
map_dbl(input, factorial_reduce)
map_dbl(input, factorial_func)
map_dbl(input, factorial_mem)

# Interestingly, the factorial_reduce() produces NAs fairly early because
# reduce() runs into an integer overflow in the lower second digits (here 
# starting at fact(12))


# Measure performance and create output ----------------------------------------

# Use microbenchmark and purrr package to calculate performance for different 
# input values

# Reset lookup table for comparing purposes
fact_tbl <- c(rep(NA, 65))

results <- map(input, ~ microbenchmark(
  factorial_loop(.),
  factorial_reduce(.),
  factorial_func(.),
  factorial_mem(.)
))

names(results) <- as.character(input)

# Write output to file
sink("factorial_output.txt")
results
sink()





