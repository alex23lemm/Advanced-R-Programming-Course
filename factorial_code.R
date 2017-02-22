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
# input values and for ranges of input values

sink("factorial_output.txt")

cat("====== PART 1: Performance and comparison of indivudual input values ======\n")
cat("======================== across factorial functions ======================= \n\n")

# Reset lookup table for comparing purposes
fact_tbl <- c(rep(NA, 65))


# Calculate and compare perforamnce of individual input values
individual_results <- map(input, ~ microbenchmark(
  factorial_loop(.),
  factorial_reduce(.),
  factorial_func(.),
  factorial_mem(.)
))

names(individual_results) <- as.character(input)
individual_results

# Calculate and compare performance of ranges of input values

cat("====== PART 2: Performance and comparison of ranges of input values =======\n")
cat("======================== across factorial functions ======================= \n\n")

get_benchmark <- function(x) {
  fact_tbl <<- c(rep(NA, 100))
  microbenchmark(map_dbl(x, factorial_loop),
                 map_dbl(x, factorial_reduce),
                 map_dbl(x, factorial_func),
                 map_dbl(x, factorial_mem))
}

ranges <- list(`range 1:10` = 1:10,
               `range 1:50` = 1:50,
               `range 1:100` = 1:100)

range_results <- map(ranges, get_benchmark)
range_results

sink()









