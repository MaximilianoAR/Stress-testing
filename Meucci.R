# Scenarios data
p <- read_csv("./FullFlexBayesNets/datos/p_ini.csv")
X <- read_csv("./FullFlexBayesNets/datos/x.csv")

# Matrix dimensions and parameter definition
e <- .01
J <- nrow(X)
N <- ncol(X)

# Initial probabilities
p <- p %>% 
  mutate(prob = (1 - e) * prob + e * (1 / J)) %>% 
  unlist()

X <- as.matrix(X)

# compute moments
ComputeMoments(X, p)$C %>% 
  round(., 2) * 100

# Views (entropy pooling)
View <- list()

for (n in 1:N){
  
  k <- 2 * n - 1
  
  View[[k]] <- list()
  View[[k]][["Who"]] <- c(n)
  View[[k]][["Equal"]] <- c(-1)
  View[[k]][["Cond_Who"]] <- NA
  View[[k]][["Cond_Equal"]] <- NA
  View[[k]][["v"]] <- .4
  View[[k]][["sgn"]] <- -1 
  View[[k]][["c"]] <- 0.5
  
  k <- 2 * n
  
  View[[k]] <- list()
  View[[k]][["Who"]] <- c(n)
  View[[k]][["Equal"]] <- c(1)
  View[[k]][["Cond_Who"]] <- NA
  View[[k]][["Cond_Equal"]] <- NA
  View[[k]][["v"]] <- .4
  View[[k]][["sgn"]] <- -1 
  View[[k]][["c"]] <- 0.5
}

k <- 2 * N + 1

View[[k]] <- list()
View[[k]][["Who"]] <- c(1, 2, 8)
View[[k]][["Equal"]] <- list(c(-1, 0), c(-1, 0), c(1))
View[[k]][["Cond_Who"]] <- c(4)
View[[k]][["Cond_Equal"]] <- c(-1)
View[[k]][["v"]] <- .9
View[[k]][["sgn"]] <- -1 
View[[k]][["c"]] <- 0.1

# Linear constraints
CPV <- CondProbViews(View, X)
A <- CPV[["A"]]
b <- CPV[["b"]]
g <- CPV[["g"]]

# Additional constraints
C_12_ <- .6
New_A <- t(X[, 1] * X[, 2])
New_b <- ComputeMoments(X, p)$s[1] * ComputeMoments(X, p)$s[2] * C_12_ + ComputeMoments(X, p)$m[1] * ComputeMoments(X, p)$m[2]
New_g <- -log(1 - .1)

A <- rbind(A, New_A)
b <- c(b, New_b)
g <- c(g, New_g)

# Missing b consistency

# Posterior probabilities
Aeq <- matrix(rep(1, J), nrow = 1)
beq <- 1

# Posterior distribution
p_ <- EntropyProg(p, A, b, Aeq, beq)

p_ <- as.numeric(p_)

# Compute moments
ComputeMoments(X, p_)$C %>% 
  round(., 2) * 100

# Probabilities distribution
p %>% 
  as.data.frame() %>% 
  mutate(n = row_number()) %>% 
  ggplot(aes(x = n, y = p)) +
  geom_point()

p_ %>% 
  as.data.frame() %>% 
  mutate(n = row_number()) %>% 
  ggplot(aes(x = n, y = p_)) +
  geom_point()
