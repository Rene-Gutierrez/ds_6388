### Sampler

horseshoe_sampler <- function(X,
                              y,
                              S,
                              fast = TRUE){
  #############################################################################
  # Set-Up
  #############################################################################
  # Problem Dimensions
  n <- length(y)
  P <- dim(X)[2]
  
  # Variable Initialization
  xi <- 1
  v  <- rep(1, P / 2)
  t2 <- 1
  l2 <- rep(1, P / 2)
  s2 <- 1
  
  # Sample Variables
  sam_B        <- matrix(data = NA, nrow = S, ncol =  P)
  sam_l2       <- matrix(data = NA, nrow = S, ncol =  P)
  sam_l2_t2    <- matrix(data = NA, nrow = S, ncol =  P)
  sam_l2_t2_s2 <- matrix(data = NA, nrow = S, ncol =  P)
  sam_v        <- matrix(data = NA, nrow = S, ncol =  P)
  sam_t2       <- numeric(length = S)
  sam_xi       <- numeric(length = S)
  sam_s2       <- numeric(length = S)
  
  #############################################################################
  # Sampling
  #############################################################################
  # Progress Bar
  pb <- txtProgressBar(min     = 0,
                       max     = 1,
                       initial = 0,
                       style   = 3,
                       width   = 72)
  
  # Sampling
  for(s in 1:S){
    ###########################################################################
    # Samples B Structure
    ###########################################################################
    # Samples the Horseshoe Structure
    out <- fast_horseshoe(y  = y,
                          X  = X,
                          xi = xi,
                          v  = v,
                          l2 = l2,
                          t2 = t2,
                          s2 = s2)
    # Retrieves the Values
    b  <- out$b
    l2 <- out$l2
    t2 <- out$t2
    s2 <- out$s2
    v  <- out$v
    xi <- out$xi
    
    # Assigns the values
    B  <- out$b
    l2 <- out$l2
    t2 <- out$t2
    v  <- out$v
    xi <- out$xi
    s2 <- out$s2
    
    # Saves Variables
    sam_B[s,]        <- B
    sam_l2[s,]       <- l2
    sam_v[s,]        <- v
    sam_l2_t2[s,]    <- l2 * t2
    sam_l2_t2_s2[s,] <- l2 * t2 * s2
    sam_t2[s]        <- t2
    sam_xi[s]        <- xi
    sam_s2[s]        <- s2
    
    ###########################################################################
    # Progress Bar Update
    ###########################################################################
    setTxtProgressBar(pb    = pb,
                      value = s / S)
    
  }
  
  # Returns
  return(list(B        = sam_B,
              s2       = sam_s2,
              l2       = sam_l2,
              t2       = sam_t2,
              v        = sam_v,
              xi       = sam_xi,
              l2_t2    = sam_l2_t2,
              l2_t2_s2 = sam_l2_t2_s2))
}