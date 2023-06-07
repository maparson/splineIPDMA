# One-dimensional betahat
betahat <- round(rnorm(n=100, mean = 2.5), 3)
wRaw <- round(runif(n=100), 3)
wSum <- sum(wRaw)
w <- wRaw/wSum

testTib <- tibble(
  beta = betahat,
  w = w
) %>%
  mutate(
    betaW = beta*w
  )

testBeta1 <- sum(testTib$betaW)

num1 <- t(betahat) %*% w
num2 <- t(sqrt(w)) %*% sqrt(w)

# N-dimensional betahat
K <- 100
N <- 5 
betahatN <- matrix(nrow = N, ncol = 100)
for(i in 1:K) {
  betahat <- round(rnorm( n = N, mean = 2.5), 3)
  betahati <- tibble(beta = betahat)
  betahatN[,i] <- betahati$beta
}
wN <- matrix(nrow = N, ncol = 100)
for(i in 1:K) {
  SEi <- round(runif(min = 0.1, max = 2.1,n=N), 3)
  wi <- 1/SEi
  wN[,i] <- wi
}

testbeta2 <- (t(betahatN[1,]) %*% wN[1,]) %*% solve(t(sqrt(wN[1,])) %*% sqrt(wN[1,]))






