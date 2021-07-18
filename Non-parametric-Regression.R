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

data <- sim(rv_ms_discrete, length=10000)
pred <- local_linear_prediction(t=9, h=7, data=data)


par(mar=c(0, 0.5, 0, 0.5), ann=FALSE, xaxt='n', yaxt='n')
plot(data$direction, data$n, col=2)
grid()
y_plot2 <- sapply(seq(1, 359, len=1000), local_cubic_prediction, h=35, data=data)
lines(seq(1, 359, len=1000), y_plot2, col=3, lwd=2)
y_plot <- sapply(seq(1, 359, len=1000), local_linear_prediction, h=35, data=data)
lines(seq(1, 359, len=1000), y_plot, col=4, lwd=2)
