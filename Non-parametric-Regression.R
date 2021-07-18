#' Non-parametric Regression
#' import rlang
#' import tidyverse
#' 
#' 
#' 
#' 
#' 
#' 
#' 
local_linear_prediction <- function(t, h, data) {
    loc <- data$direction %% 360 >= t-h & data$direction %%360 <= t+h | 
                       data$direction %%360 >= t-h+360 & data$direction %%360 <= t+h
    data <- data[loc,]
    
    if (nrow(data) < 4) return(NA)
    re <- predict(lm(data$n ~ data$direction, data.frame(x=t)))
    return(re[h+1])
}

local_cubic_prediction <- function(t, h, data) {
    loc <- data$direction %% 360 >= t-h & data$direction %%360 <= t+h | 
                       data$direction %%360 >= t-h+360 & data$direction %%360 <= t+h
    data <- data[loc,]
    
    if (nrow(data) < 4) return(NA)
    re <- predict(lm(data$n ~ data$direction + I(data$direction^2) + I(data$direction^3), data.frame(x=t)))
    return(re[h+1])
}

# Faster approach (see lecture)
loc_pol_kern_reg <- function(t, data, h, kernel=function(delta) exp(-delta^2), poly_deg=1) {
    n <- nrow(data)
    m <- length(t)
    p <- poly_deg +1
    X <- outer(data$direction, 0:poly_deg, `^`)
    Ws <- kernel(outer(data$direction, t, `-`)/h)
    XtWs <- array(rep(t(X), times = m) * rep(Ws, each=p), dim=c(p, n, m))
    Psi_t <- outer(t, 0:poly_deg, `^`)
    f_hat <- double(m)
    tryCatch(
        for (j in 1:m){
            B <- XtWs[,,j] %*% X
            a <- XtWs[,,j] %*% data$n
            f_hat[j] <- Psi_t[j,] %*% .Internal(La_solve(B, a, .Machine$double.eps))
        },
        error = function(cond) return(NaN))
    f_hat
}


data <- sim(rv_ms_discrete, length=10000)
pred <- local_linear_prediction(t=9, h=7, data=data)


par(mar=c(0, 0.5, 0, 0.5), ann=FALSE, xaxt='n', yaxt='n')
plot(data$direction, data$n, col=2)
grid()
y_plot3 <- loc_pol_kern_reg(t = seq(1, 359, len=1000), data=data, h=30)
lines(seq(1, 359, len=1000), y_plot3)
y_plot2 <- sapply(seq(1, 359, len=1000), local_cubic_prediction, h=35, data=data)
lines(seq(1, 359, len=1000), y_plot2, col=3, lwd=2)
y_plot <- sapply(seq(1, 359, len=1000), local_linear_prediction, h=35, data=data)
lines(seq(1, 359, len=1000), y_plot, col=4, lwd=2)
