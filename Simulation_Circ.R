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
  a <- 1/(2*pi*(sum(1/(factorial(1:1000)^2)*(kappa/2)^(1:1000))))
  b <- exp(kappa*cos(x-mu))
  return(a*b)
}
v_ms_data <- function(n=10000, mu=0, kappa=1){
  x <- NULL
  theta <- seq(0, 2*pi, length.out=n)
  for(i in 1:n){
    x <- c(x, rep(theta[i], times = as.integer( 1000 * nv_ms(theta[i], mu, kappa))))
  }
  return(x)
}

data_prod <- function(n=10000, mu=0, kappa=1) {
  data_env <- env(pooling = v_ms_data(n=10000, mu=0, kappa=1))
}
data_prod <- memoise::memoise(data_prod)

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
sim <- function(distribution, length, mu=0, kappa=1, mean_n=0, sd_n=1, sum=TRUE) {
  data_prod(n=10000, mu=0, kappa=1)
  stopifnot(typeof(as.integer(length))=="integer")
  noise <- round_my(rnorm(n=length, mean = mean_n, sd = sd_n))
  if(expr_text(enexpr(distribution)) == "unif") {
    y <- sample.int(359, size=length, replace = TRUE) + noise
    output <- tibble(angles=y, n=rep(1, times=length))
    output %>%
      select(angles) %>%
      group_by(angles) %>%
      summarise(n=n()) -> output2
    class_out <- class(output)
    attr(output, "class") <- c("circular data", class_out)
    if(sum==TRUE) return(output2)
    else return(output)
  }
  if(expr_text(enexpr(distribution)) == "rv_ms_discrete") {
    y <- rv_ms_discrete(length) + noise
    output <- tibble(angles=y, n=rep(1, times=length))
    output %>%
      select(angles) %>%
      group_by(angles) %>%
      summarise(n=n()) -> output2
    class_out <- class(output)
    attr(output, "class") <- c("circular data", class_out)
    if(sum==TRUE) return(output2)
    else return(output)
  }
}
