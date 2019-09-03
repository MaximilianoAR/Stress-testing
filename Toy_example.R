# Scenario data
X <- matrix(c(1,1,1,1,1,2,1,2,1,1,2,2,2,1,1,2,1,2,2,2,1,2,2,2,3,1,1,3,1,2,3,2,1,3,2,2), nrow = 12, byrow = TRUE)

# Matrix dimensions
J <- nrow(X)
N <- ncol(X)

# Initial probabilities
p <- rep(1, J) / J

# Views (entropy pooling)
View <- list()

View[[1]] <- list()
View[[1]][["Who"]] <- 1
View[[1]][["Equal"]] <- c(2, 3)
View[[1]][["Cond_Who"]] <- 2
View[[1]][["Cond_Equal"]] <- 1
View[[1]][["v"]] <- .7
View[[1]][["sgn"]] <- -1 
View[[1]][["c"]] <- 0.5

View[[2]] <- list()
View[[2]][["Who"]] <- 2
View[[2]][["Equal"]] <- 1
View[[2]][["Cond_Who"]] <- NA
View[[2]][["Cond_Equal"]] <- NA
View[[2]][["v"]] <- .3
View[[2]][["sgn"]] <- -1 
View[[2]][["c"]] <- 0.5

# Linear constraints
CPV <- CondProbViews(View, X)
A <- CPV[["A"]]
b <- CPV[["b"]]
g <- CPV[["g"]]

# Missing b consistency

# Posterior probabilities
Aeq <- matrix(rep(1, J), nrow = 1)
beq <- 1

# Posterior distribution
p_ <- EntropyProg(p, A, b, Aeq, beq)
