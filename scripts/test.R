

mkde <- function(x, n) {
  H <- diag(apply(x, 2, bw.SJ))
  lims <- apply(x, 2, range, simplify = TRUE)
  g <- as.matrix(cbind(expand.grid(as.data.frame(apply(lims, 2, seq.int, length.out = n)))))
  out <- numeric(nrow(g))
  for(i in nrow(x)) out <- out + mvtnorm::dmvnorm(g, x[i, ], H)
  out <- out/nrow(x)
  g <- as.data.frame(g)
  g$fhat <- out
  return(g)
}


k <- 1e2
d <- 2
mu <- rep(0, d)
sigma <- diag(d)
x <- mvtnorm::rmvnorm(k, mu, sigma)
n <- 10

mkde(x, n)

