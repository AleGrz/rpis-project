library(smoof)
# install.packages("BSDA", repos="https://cloud.r-project.org/")
# z BSDA bierzemy z.test do testów
library(BSDA)

prs <- function(f, point_count, bounds, dim) {
  min_value <- Inf
  
  for (i in 1:point_count) {
    point <- runif(dim, min = bounds[1], max = bounds[2])
    value <- f(point)
    
    if (value < min_value) {
      min_value <- value
    }
  }
  
  return(min_value)
}

ms <- function(f, point_count, bounds, dim) {
  results <- replicate(
    point_count, 
    optim(
      par = runif(dim, min = bounds[1], max = bounds[2]), 
      fn = f, 
      method = "L-BFGS-B", 
      lower = rep(bounds[1], dim), 
      upper = rep(bounds[2], dim)
    ),
    simplify = FALSE 
  )
  
  min_value <- min(sapply(results, function(res) res$value))
  total_calls <- sum(sapply(results, function(res) res$counts["function"]))
  
  return(list(value = min_value, calls = total_calls))
}

compare_prs_ms <- function(f, bounds, dim) {
  print('MS')
  ms_res <- replicate(100, ms(f, 100, bounds, dim), simplify = FALSE)
  
  value <- sapply(ms_res, function(x) x$value)
  budget <- round(mean(sapply(ms_res, function(x) x$calls)))
  
  print('PRS')
  prs_res <- replicate(100, prs(f, budget, bounds, dim))
  
  return(list(value, prs_res))
}

get_alpine01_bounds <- function() { 
  return(c(-10, 10))
}

get_rosenbrock_bounds <- function() {
  return(c(-30, 30))
}

dimensions <- c(2,10,20)

for (dim in dimensions) {

  res <- compare_prs_ms(makeRosenbrockFunction(dim), get_rosenbrock_bounds(), dim)
  write.csv(res[1], file=paste0("./res/rosenbrockms", dim, ".csv"), row.names = FALSE)
  write.csv(res[2], file=paste0("./res/rosenbrockprs", dim, ".csv"), row.names = FALSE)

	res_prs = res[[2]]
	res_ms = res[[1]]

	# przedziały ufności
	ci_prs <- c(mean(res_prs) - 1.96 * sd(res_prs) / sqrt(length(res_prs)), 
              mean(res_prs) + 1.96 * sd(res_prs) / sqrt(length(res_prs)))
  ci_ms <- c(mean(res_ms) - 1.96 * sd(res_ms) / sqrt(length(res_ms)), 
             mean(res_ms) + 1.96 * sd(res_ms) / sqrt(length(res_ms)))
	

	# test
	test_res <- z.test(res_prs, res_ms, sigma.x = sd(res_prs), sigma.y = sd(res_ms), conf.level = 0.95)
  print("TEST:")
  print(test_res)

  res <- compare_prs_ms(makeAlpine01Function(dim), get_alpine01_bounds(), dim)
  write.csv(res[1], file=paste0("./res/alpine01ms", dim, ".csv"), row.names = FALSE)
  write.csv(res[2], file=paste0("./res/alpine01prs", dim, ".csv"), row.names = FALSE)
	res_prs = res[[2]]
	res_ms = res[[1]]

	# przedziały ufności
	ci_prs <- c(mean(res_prs) - 1.96 * sd(res_prs) / sqrt(length(res_prs)), 
              mean(res_prs) + 1.96 * sd(res_prs) / sqrt(length(res_prs)))
  ci_ms <- c(mean(res_ms) - 1.96 * sd(res_ms) / sqrt(length(res_ms)), 
             mean(res_ms) + 1.96 * sd(res_ms) / sqrt(length(res_ms)))


	# test
	test_res <- z.test(res_prs, res_ms, sigma.x = sd(res_prs), sigma.y = sd(res_ms), conf.level = 0.95)
  print("TEST:")
  print(test_res)
	# na mocy testów odrzucamy hipotezę zerową - średnie wyniki prs i ms są wyraźnie różne
}