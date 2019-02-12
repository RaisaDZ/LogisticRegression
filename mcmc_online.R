#implementation of our algorithm

#Inputs: outcomes - binary represenation of target vector. For example if there are three classes,
#        outcomes should be decoded as (0, 0, 1), (0, 1, 0) or (1, 0, 0)
#        X - matrix of features where rows are observations and columns are features,
#        X should be in matrix format. If data.frame then put X = as.matrix(X)
#        M - maximum number of iteration
#        M0 - the length of "burn-in" period
#        a - regualrization parameter
#        sigma - standard deviation

#Outputs: gamma - calculated predictions for each class,
#         theta - sampling parameters at each iteration,
#         lik - log-likelihood of parameters at each iteration

mcmc_online = function(outcomes, X, M, M0, a, sigma){
  eta = 1
  
  X <- cbind(rep(1, dim(X)[1]), X)  #add bias
  n <- dim(X)[2]   #dimension
  T <- dim(X)[1]   #time
  d <- dim(outcomes)[2]   #number of classes
  
  omega <- function(theta, ksi, outcomes) {
      logprobs = log(apply(ksi * outcomes, 1, sum))
      w <- - a * eta * sum(theta^2) + sum(logprobs)
   return(w)
  }
  
  omega0 <- function(theta) {
    w <- - a * eta * sum(theta^2)
    return(w)
  }
  
  calc_probs <- function(X, theta) {
    if (is.null(dim(X)[1])) {
      X = matrix(X, nrow = 1)
    }
    d = dim(theta)[1] + 1
    n = dim(X)[1]
    ksi <- matrix(0, n, d)
    for (l in 2:d) {
      ksi[, l] <- apply(X * matrix(rep(theta[l-1, ], n), nrow = n, byrow = T), 1, sum)
    }
    ksi = exp(ksi)
    ksi <- ksi / apply(ksi, 1, sum)
  }
  
  lik = matrix(0, nrow = T, ncol = M)
  
  alpha_mat <- array(0, dim = c(T, M, 5))
  gamma <- matrix(0, T, ncol = d)
  theta <- array(0, dim = c(T, M, d-1, n))
  
  for (t in 1:T) {
    #initial estimates of theta
    if (t > 1) {
      theta[t, 1, , ] <- theta[t-1, M, , ] 
    }
    ksi_tot = 0
    for (m in 2:M)  {
      theta_old <- theta[t, m-1, ,]  #theta from previous step m-1
      theta_new <- theta_old + matrix(rnorm((d-1)*n, 0, sigma^2), nrow = d-1, ncol = n) #sample new params
      if (t > 1) {
        ksi_old <- calc_probs(X[1:(t-1), ], theta_old)  #old probs
        ksi_new <- calc_probs(X[1:(t-1), ], theta_new)  #new probs
        omega_old = omega(theta_old, ksi_old, outcomes[1:(t-1), ])
        omega_new = omega(theta_new, ksi_new, outcomes[1:(t-1), ])
      } else {
        omega_old = omega0(theta_old)
        omega_new = omega0(theta_new)
      }
      alpha0 <- exp(omega_new - omega_old)
      alpha <- min(1, alpha0)
      alpha_mat[t, m, 1] <- omega_new
      alpha_mat[t, m, 2] <- omega_old
      alpha_mat[t, m, 3] <- alpha
      rand <- runif(1, 0, 1)  #flip a coin
      alpha_mat[t, m, 4] <- rand
      if (alpha >= rand)  {  #accept new params
        theta[t, m, , ] <- theta_new
        alpha_mat[t, m, 5] <- 1
        lik[t, m] = omega_new 
      } else {              #keep old params
        theta[t, m, , ] <- theta[t, m-1, , ]
        alpha_mat[t, m, 5] <- 0
        lik[t, m] = omega_old
      }
      ksi <- calc_probs(X[t, ], theta[t, m, , ])
      #burn-in
      if (m > M0) {
        ksi_tot <- ksi_tot + ksi
      }
    }
    gamma[t, ] <- ksi_tot / (M-M0)
  }
  return(list(gamma = gamma, theta = theta, alpha_mat = alpha_mat, lik = lik[, 2:M]))
}
