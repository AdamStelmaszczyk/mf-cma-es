# x :: list[double]

f_sphere <- function(x) {
  sum(x^2)
}

# x :: list[double]

# http://www.cmap.polytechnique.fr/~nikolaus.hansen/copenhagen-cma-es.pdf
# p. 61

f_weighted_sphere <- function(x, alpha = 6) {
  n <- length(x)
  coeffs <- 10^(alpha * (1:n - 1) / (n - 1))
  sum(coeffs * x^2)
}

# x :: list[double]

f_rosenbrock <- function(x) {
  d <- length(x)
  z <- x + 1
  hz <- z[1:(d - 1)]
  tz <- z[2:d]
  sum(100 * (hz^2 - tz)^2 + (hz - 1)^2)
}


# x :: list[double]

f_rastrigin <- function(x) {
  sum(x * x - 10 * cos(2 * pi * x) + 10)
}

# x :: list[double]
# a :: double
# b :: double
# c :: double

f_ackley <- function(x, a=20, b=0.2, c=2*pi) {
  d <- length(x)
  sum1 <- sum(x^2)
  sum2 <- sum(cos(c*x))

  term1 <- -a * exp(-b*sqrt(sum1/d))
  term2 <- -exp(sum2/d)

  term1 + term2 + a + exp(1)
}

# x :: list[double]

f_griewank <- function(x) {
  ii <- c(1:length(x))
  sum <- sum(x^2/4000)
  prod <- prod(cos(x/sqrt(ii)))
  sum - prod + 1
}

# x :: list[double]

f_booth <- function(x) {
  x1 <- x[1]
  x2 <- x[2]
  term1 <- (x1 + 2*x2 - 7)^2
  term2 <- (2*x1 + x2 - 5)^2
  term1 + term2
}

# x :: list[double]

f_levy <- function(x) {
  d <- length(x)
  w <- 1 + (x - 1)/4
	
  term1 <- (sin(pi*w[1]))^2 
  term3 <- (w[d]-1)^2 * (1+1*(sin(2*pi*w[d]))^2)
	
  wi <- w[1:(d-1)]
  sum <- sum((wi-1)^2 * (1+10*(sin(pi*wi+1))^2))
	
  term1 + sum + term3
}
