library(tidyverse)
library(rlang)

#von-mises-distribution
#' Approximation of the van-Mises-Distribution
#'
#' Approximation of the van-Mises-Distribution `nv_ms` returns the density
#' at any given point \eqn{ x \in [0,1] \subset \mathbb{R}} on a circle.
#' @section Warning: This is a helper function.
nv_ms <- function(x, mu=0, kappa=1){
  a <- 1/(2*pi*(sum(1/(factorial(1:1000)^2)*(kappa/2)^(1:1000))))
  b <- exp(kappa*cos(x-mu))
  return(a*b)
}
v_ms_data <- function(mu=0, kappa=1){
  x <- NULL
  theta <- seq(0, 2*pi, length.out=10000)
  for(i in 1:10000){
    x <- c(x, rep(theta[i], times = as.integer( 30 * nv_ms(theta[i], mu, kappa))))
  }
  return(x)
}
data_env <- env()
data_prod <- function(mu1=0, kappa1=1) {
  env_bind(data_env, pooling = v_ms_data(mu=mu1, kappa=kappa1))
}
data_prod <- memoise::memoise(data_prod)

rv_ms_stetig <- function(n=1) {
  #ACHTUNG: Output ist in Bogenmaßen
  out <- sample(data_env$pooling, size=n, replace = TRUE)
  out
}

rv_ms_discrete <- function(n=1){
  #Achtung: Output ist in Winkelmaßen(0-360)
  round((1/pi)*180*rv_ms_stetig(n))
}
#'@export
sim <- function(distribution, length, mu1=0, kappa1=1, mean_n=0, sd_n=1, sum=TRUE) {
  data_prod(mu=mu1, kappa=kappa1)
  stopifnot(typeof(as.integer(length))=="integer")
  noise <- round(rnorm(n=length, mean = mean_n, sd = sd_n))
  if(expr_text(enexpr(distribution)) == "unif") {
    y <- sample(1:359, size=length, replace = TRUE) + noise
    output <- tibble(direction=y, n=rep(1, times=length))
    output %>%
      select(direction) %>%
      group_by(direction) %>%
      summarise(n=n()) -> output2
    class_out <- class(output)
    attr(output, "class") <- c("circular data", class_out)
    if(sum==TRUE) return(output2)
    else return(output)
  }
  if(expr_text(enexpr(distribution)) == "rv_ms_discrete") {
    y <- rv_ms_discrete(length) + noise
    output <- tibble(direction=y, n=rep(1, times=length))
    output %>%
      select(direction) %>%
      group_by(direction) %>%
      summarise(n=n()) -> output2
    class_out <- class(output)
    attr(output, "class") <- c("circular data", class_out)
    if(sum==TRUE) return(output2)
    else return(output)
  }
}
