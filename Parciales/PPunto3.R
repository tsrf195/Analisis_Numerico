library(pracma)


f <- function(x) exp(x)*sin(x)
p <- taylor(f, 0.999999999, 3)

x <- seq(-1.0, 1.0, length.out=100)
yf <- f(x)
yp <- polyval(p, x)
plot(x, yf, type = "l", col = "gray", lwd = 3)
lines(x, yp, col = "red")
grid()



