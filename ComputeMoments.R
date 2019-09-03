# Compute moments ---------------------------------------------------------

ComputeMoments <- function(X, p){
  m <- t(X) %*% p
  Sm <- t(X) %*% (X * replicate(N, p))
  S <- Sm - m %*% t(m)
  C <- cov2cor(S)
  s <- sqrt(diag(S))
  
  return(list("m" = m, "s" = s, "C" = C))
}
