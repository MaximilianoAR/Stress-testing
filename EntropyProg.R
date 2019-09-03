# Entropy prog ------------------------------------------------------------

EntropyProg <- function(p, A, b, Aeq, beq){
  
  K_ <- nrow(A)
  K <- nrow(Aeq)
  A_ <- t(A)
  b_ <- t(b)
  Aeq_ <- t(Aeq)
  beq_ <- t(beq)
  x0 <- rep(0, K_ + K)
  InqMat <- diag(K_ + K)[1:(K_ + K - 1), ]
  InqVec <- rep(0, K_)
  
  v <- constrOptim(theta = x0 + .01, grad = NULL, f = nestedfunC, ui = InqMat, ci = InqVec)
  
  lv <- v$par
  l <- lv[1:K_]
  v <- lv[(K_ + 1):length(lv)]
  
  p_ <- exp(log(p) - 1 - A_ %*% l - Aeq_ %*% v)
  
  return(p_)
}