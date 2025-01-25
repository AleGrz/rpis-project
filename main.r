library(smoof)
library(ecr)

prs <- function(f, point_count, bounds) {
  
  min_value <- Inf
  min_point <- NULL

  for (i in 1:point_count) {

    value <- f(sapply(bounds, function(b) runif(1, b[1], b[2])))
    
    if (value < min_value) {
      min_value <- value
    }
  }
  
  return(min_value)
}

ms <- function(f, point_count, bounds) {
  
  min_value <- Inf
  total_calls <- 0

  for (i in 1:point_count) {
    start_point <- sapply(bounds, function(b) runif(1, b[1], b[2]))
    
    result <- optim(
      par = start_point, 
      fn = f, 
      method = "L-BFGS-B",
      lower = sapply(bounds, function(b) b[1]),
      upper = sapply(bounds, function(b) b[2])
    )

    total_calls <- total_calls + result$counts["function"]

    if (result$value < min_value) {
      min_value <- result$value
    }
  }

  return(list(value = min_value, calls = total_calls))
}


compare_prs_ms <- function(f, bounds) {
  print('MS')
  ms_res <- replicate(70, ms(f, 100, bounds))
  
  value <- mean(sapply(ms_res, function(x) x["value"]))
  budget <- mean(sapply(ms_res, function(x) x["calls"]))
  
  print('PRS')
  prs_res <- replicate(70, prs(f, budget, bounds))
  
}

get_alpine01_bounds <- function(dim) { 
  return(rep(c(0, 10), length.out = 2 * dim))
}

get_rosenbrock_bounds <- function(dim) {
  return(rep(c(-5, 10), length.out = 2 * dim))
}


dimensions = c(2,10,20)

for (dim in dimensions) {
  res <- compare_prs_ms(makeRosenbrockFunction(dim), get_rosenbrock_bounds(dim))
  write.csv(res["ms"], file="./res/rosenbrockms"+dim+".csv")
  write.csv(res["prs"], file="./res/rosenbrockprs"+dim+".csv")
  compare_prs_ms(makeAlpine01Function(dim), get_alpine01_bounds(dim))
  write.csv(res["ms"], file="./res/alpinems"+dim+".csv")
  write.csv(res["prs"], file="./res/alpineprs"+dim+".csv")
}

