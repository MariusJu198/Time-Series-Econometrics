
KFilterLL <- function(y,sigEta,sigEps){ 
  T <- length(y)
  muFilt <- matrix(NA,T,1)  
  PFilt <- matrix(NA,T,1)
  muPred <- matrix(NA,T+1,1)
  PPred <- matrix(NA,T+1,1)
  
  if (sigEps < 0 | sigEta < 0){like <- -Inf} # Variances must be non-negative!
  else {
    muPred[1] = 0  #Initial condition
    PPred[1] = 10^7 #Initial condition
    
    like <- 0
    for (t in 1:T){
      epsilon <- y[t] - muPred[t]  #Prediction error
      F <- PPred[t] + sigEps 
      K <- PPred[t]/F
      muFilt[t] <- muPred[t]+K*epsilon  #Filtered state
      PFilt[t] <- PPred[t]*(1-K) 
      muPred[t+1] <- muFilt[t] #Predicted state
      PPred[t+1] <- PFilt[t] + sigEta
      like <- like - 0.5*(log(2*pi) + log(F) + (epsilon^2)/F) # Likelihood contribution at time t
    }
  }
  return (list(muFilt = muFilt,muPred = muPred,
               PFilt = PFilt,PPred = PPred, like = like))
}


KFilter <- function(y, A, mu0, Sigma0, Phi, Theta, cR, cQ){ 
  T <- ncol(y)
  q <- nrow(y)
  p <- ncol(A)
  R <- cR %*% t(cR) # Undo Cholesky factorization
  Q <- cQ %*% t(cQ)
  
  xFilt <- array(NA,dim = c(p,T)) # Initialize matrices for prediction and filtering
  PFilt <- array(NA,dim = c(p,p,T))
  xPred <- array(NA,dim = c(p,T+1))
  PPred <- array(NA,dim = c(p,p,T+1))
  F <- array(NA,dim = c(q,q,T))
  ThetaQ <- Theta %*% Q %*% t(Theta)
  xPred[,1] <- mu0 # Initial conditions
  PPred[,,1] <- Sigma0
  like <- 0
  for (t in 1:T){      
    F[,,t] <- A %*% PPred[,,t] %*% t(A) + R
    if (is.na(y[,t])){ epsilon = 0; K = matrix(rep(0,p*q),p,q)} #Check whether y[t] is missing
    else{
      epsilon <- y[,t] - A %*% xPred[,t]
      Finv <- solve((t(F[,,t]) + F[,,t])/2)
      K <- PPred[,,t] %*% t(A) %*% Finv
      like <- like - 0.5*(q*log(2*pi) + log(det(as.matrix(F[,,t]))) + 
                            t(epsilon) %*% Finv %*% epsilon)
    }
    
    xFilt[,t] <- xPred[,t] + K %*% epsilon
    PFilt[,,t] <- (diag(1,p,p) - K %*% A) %*% PPred[,,t]
    
    xPred[,t+1] <- Phi %*% xFilt[,t]
    PPred[,,t+1] <- Phi %*% PFilt[,,t] %*% t(Phi) + ThetaQ 
  }
  return (list(xFilt = xFilt,xPred = xPred,
               PFilt = PFilt,PPred = PPred, F=F, like = like))
}

KSmooth <- function(y, A, mu0, Sigma0, Phi, Theta, cR, cQ){ 
  
  kf <- KFilter(y, A, mu0, Sigma0, Phi, Theta, cR, cQ)
  T <- ncol(y)
  p <- ncol(A)
  
  
  xSmooth <- array(NA, dim = c(p, T))
  PSmooth <- array(NA, dim = c(p, p, T))
  
  xSmooth[,T] <- kf$xFilt[,T]
  PSmooth[,,T] <- kf$PFilt[,,T]
  for (t in (T-1):1){
    J <- kf$PFilt[,,t] %*% t(Phi) %*% solve(kf$PPred[,,t+1])
    xSmooth[,t] <- kf$xFilt[,t] + J %*% (xSmooth[,t+1] - kf$xPred[,t+1])
    PSmooth[,,t] <- kf$PFilt[,,t] + J %*% (PSmooth[,,t+1] - kf$PPred[,,t+1]) %*% t(J)
  }
  
  return (list(kf = kf, xSmooth = xSmooth, PSmooth = PSmooth))
  
}

