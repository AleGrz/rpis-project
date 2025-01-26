library(smoof)
library(ecr)

prs <- function(f, point_count, bounds) {
  
  min_value <- Inf
  min_point <- NULL

  for (i in 1:point_count) {

    point <- sapply(seq(1, length(bounds), by = 2), function(i) runif(1, bounds[i], bounds[i + 1]))
    value <- f(point)
    
    if (value < min_value) {
      min_value <- value
    }
  }
  
  return(min_value)
}

ms <- function(f, point_count, bounds) {
    results <- replicate(
    point_count, 
    optim(
      par = runif(length(bounds) / 2, min = bounds[1], max = bounds[2]), 
      fn = f, 
      method = "L-BFGS-B", 
      lower = rep(bounds[1], length(bounds) / 2), 
      upper = rep(bounds[2], length(bounds) / 2)
    ),
    simplify = FALSE 
  )

  min_value <- min(sapply(results, function(res) res$value))
  total_calls <- sum(sapply(results, function(res) res$counts["function"]))


  return(list(value = min_value, calls = total_calls))
}

compare_prs_ms <- function(f, bounds) {
  print('MS')
  ms_res <- replicate(70, ms(f, 100, bounds), simplify = FALSE)
  # print(ms_res)
  
  value <- sapply(ms_res, function(x) x$value)
  budget <- mean(sapply(ms_res, function(x) x$calls))

  print(value)
  print(budget)
  
  print('PRS')
  prs_res <- replicate(70, prs(f, budget, bounds))
  # print(prs_res)

  return (list(value, prs_res))
  
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
  write.csv(res[1], file=paste0("./res/rosenbrockms", dim, ".csv"), row.names = FALSE, sep = ";")
  write.csv(res[2], file=paste0("./res/rosenbrockprs", dim, ".csv"), row.names = FALSE, sep = ";")
  compare_prs_ms(makeAlpine01Function(dim), get_alpine01_bounds(dim))
  write.csv(res[1], file=paste0("./res/alpine01ms", dim, ".csv"), row.names = FALSE, sep = ";")
  write.csv(res[2], file=paste0("./res/alpine01prs", dim, ".csv"), row.names = FALSE, sep = ";")
}

