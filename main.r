library(smoof)
library(ecr)

prs <- function(f, point_count, bounds) {
  
  min_value <- Inf
  min_point <- NULL

  for (i in 1:point_count) {

    value <- f(runif(1, min = bounds[i][1], max = bounds[i][2]))
    
    if (value < min_value) {
      min_value <- value
    }
  }
  
  return(min_value)
}

compare_prs_ms <- function(f, bounds) {
  print('MS')
  ms_res <- replicate(70, ms(f, 100, bounds))
  
  print('PRS')
  prs_res <- replicate(70, prs())
  
}

get_alpine01_bounds <- function(dim) { 
  return(rep(c(0, 10), dim))
}

get_rosenbrock_bounds <- function(dim) {
  return(rep(c(-5,10)), dim)
}


dimensions = c(2,10,20)

for (dim in dimensions) {
  compare_prs_ms(makeRosenbrockFunction(dim), get_rosenbrock_bounds(dim))
  compare_prs_ms(makeAlpine01Function(dim), get_alpine01_bounds(dim))
}
