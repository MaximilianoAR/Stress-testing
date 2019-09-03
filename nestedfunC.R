# Nested functions --------------------------------------------------------

nestedfunC <- function(lv){
  
  K_ <- nrow(A)
  K <- nrow(Aeq)
  A_ <- t(A)
  b_ <- t(b)
  Aeq_ <- t(Aeq)
  beq_ <- t(beq)
  
  l <- lv[1:K_]
  v <- lv[(K_ + 1):length(lv)]
  x <- exp(log(p) - 1 - A_ %*% l - Aeq_ %*% v)
  #x <- max(x, 10^(-32))
  L <- t(x) %*% (log(x) - log(p)) + t(l) %*% (A %*% x - b) + t(v) %*% (Aeq %*% x - beq)
  ml <- as.numeric(-L)
  attr(ml, 'gradient') <- rbind(b - A %*% x, beq - Aeq %*% x)
  attr(ml, 'hessian') <- rbind(cbind(A %*% ((x %*% rep(1, K_)) * A_), A %*% ((x %*% rep(1, K)) * Aeq_)),
                               cbind(Aeq %*% ((x %*% rep(1, K_)) * A_), Aeq %*% ((x %*% rep(1, K)) * Aeq_)))
  return(ml)
}