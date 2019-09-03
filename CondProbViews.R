# Conditional probability views -------------------------------------------

CondProbViews <- function(View, X){
  
  A <- vector()
  b <- vector()
  g <- vector()
  
  for (k in 1:length(View)){
    
    I_mrg <- as.numeric(X[, 1] < Inf)
    
    for (s in 1:length(View[[k]][["Who"]])){
      Who <- View[[k]][["Who"]][[s]]
      Or_Targets <- View[[k]][["Equal"]][[s]]
      I_mrg_or <- as.numeric(X[, Who] > Inf)
      for (i in 1:length(Or_Targets)){
        I_mrg_or <- as.numeric(I_mrg_or | (X[, Who] == Or_Targets[i]))
      }
      I_mrg <- as.numeric(I_mrg & I_mrg_or) 
    }
    
    I_cnd <- as.numeric(X[, 1] < Inf)
    
    for (s in 1:length(View[[k]][["Cond_Who"]])){
      Who <- View[[k]][["Cond_Who"]][[s]]
      Or_Targets <- View[[k]][["Cond_Equal"]][[s]]
      I_cnd_or <- as.numeric(X[, Who] > Inf)
      for (i in 1:length(Or_Targets)){
        I_cnd_or <- as.numeric(I_cnd_or | (X[, Who] == Or_Targets[i])) 
      }
      I_cnd <- as.numeric(I_cnd & I_cnd_or)
    }
    
    I_jnt <- as.numeric(I_mrg & I_cnd)
    
    if (!is.na(View[[k]][["Cond_Who"]])){
      New_A <- View[[k]][["sgn"]] * t(I_jnt - View[[k]][["v"]] * I_cnd)
      New_b <- 0
    } else {
      New_A <- View[[k]][["sgn"]] * t(I_mrg)
      New_b <- View[[k]][["sgn"]] * View[[k]][["v"]]
    }
    
    A <- c(A, New_A)
    b <- c(b, New_b)
    g <- c(g, -log(1 - View[[k]][["c"]]))
  }
  
  A <- matrix(A, nrow = length(View), byrow = TRUE)
  
  return(list("A" = A, "b" = b, "g" = g))
  
}
