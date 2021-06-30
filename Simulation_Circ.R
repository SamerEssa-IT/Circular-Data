library(tidyverse)
library(rlang)
round_my <- function(x){
  y <- NULL
  for(i in seq_along(abs(as.integer(x)))) {
    if(abs(x[i]-as.integer(x[i])) >= 0.5) {y <- c(y, as.integer(x[i]) + 1*sign(x[i]))}
    else y <- c(y, as.integer(x[i]))
  }
  return(y)
}
#von-mises-distribution
nv_ms <- function(x, mu=0, kappa=1){
  (1/(2*pi*(sum(1/(factorial(1:1000)^2)*(kappa/2)^(1:1000)))))*exp(kappa*cos(x-mu))
}
v_ms_data <- function(n=10000, mu=0, kappa=1 ){
  x <- NULL
  theta <- seq(0, 2*pi, length.out=n)
  for(i in 1:n){
    x <- c(x, rep(theta[i], times = as.integer( 1000 * nv_ms(theta[i], mu, kappa))))
  }
  return(x)
}
data_env <- env(pooling = v_ms_data(n=10000))
pooling <- data_env$pooling
rv_ms_stetig <- function(n=1) {
  #ACHTUNG: Output ist in Bogenmaßen
  out <- sample(data_env$pooling, size=n, replace = TRUE)
  out
}

rv_ms_discrete <- function(n=1){
  #Achtung: Output ist in Winkelmaßen(0-360)
  round_my((1/pi)*180*rv_ms_stetig(n))
}

# uniform-distribution:
sim <- function(distribution, length, mean_n=0, sd_n=1) {
  stopifnot(typeof(as.integer(length))=="integer")
  noise <- round_my(rnorm(n=length, mean = mean_n, sd = sd_n))
  if(expr_text(enexpr(distribution)) == "unif") {
    y <- sample.int(359, size=length, replace = TRUE) + noise
    output <- tibble(angles=y)
    output %>%
      group_by(angles) %>%
      summarise(n=n()) -> output
    class_out <- class(output)
    attr(output, "class") <- c("circular data", class_out)
    return(output)
  }
  if(expr_text(enexpr(distribution)) == "rv_ms_disc") {
    y <- rv_ms_discrete(length) + noise
    output <- tibble(angles=y)
    output %>%
      group_by(angles) %>%
      summarise(n=n()) -> output
    class_out <- class(output)
    attr(output, "class") <- c("circular data", class_out)
    return(output)
  }
}
