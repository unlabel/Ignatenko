main <- function(objectCounter = 500){
    library(MASS)
    Sigma1 <- matrix(c(1, 0, 0, 1),2,2)
    Sigma2 <- matrix(c(1, 0, 0, 1),2,2)
    mu1 <- c(0,0)
    mu2 <- c(0,6)
    x1 <- mvrnorm(n = objectCounter, mu1, Sigma1)
    x2 <- mvrnorm(n = objectCounter, mu2, Sigma2)
    
    xy1 <- cbind(x1,1) 
    xy2 <- cbind(x2,2) 
    
    xl <- rbind(xy1,xy2)
    
    colors <- c("red", "green")
    
    muh1 <- get_mu_with_hat(x1)
    muh2 <- get_mu_with_hat(x2)
    #muh <- muh1-muh2
    
    sigma1 <- get_sigma_with_hat(x1, muh1)
    sigma2 <- get_sigma_with_hat(x2, muh2)
    print(muh1)
    print(muh2)
   # print(muh)
    print(sigma1)
    print(sigma2)
    #sigma <- sigma1 + sigma2
    #print(sigma)
    #sigma1 <- solve(sigma1)
    #sigma2 <- solve(sigma2)
    #print(sigma1)
    #print(sigma2)
    
#     alpha <- sigma[1,1]
#     beta <- sigma[1,2]
#     gamma <- sigma[2,2]
    
    #p1 <- alpha*(muh1[1]-muh2[1]) + beta*(muh1[2]-muh2[2])
    #p2 <- beta*(muh1[1]-muh2[1]) + gamma*(muh1[2]-muh2[2])
    #k <- sigma%*%muh
    

    #del <- muh2
    
    #b <- (alpha*(muh1[1]-muh2[1])^2 + 2*beta*(muh1[1]-muh2[1])*(muh1[2]*muh2[2]) + gamma*(muh1[2]-muh2[2])^2) / 2
    #b <- t(muh)%*%sigma%*%muh/2
    #print(k)
    #print(b)
    
    a1 <-get_alpha(sigma1, muh1)
    a2 <-get_alpha(sigma2, muh2)
    print(a1-a2)
    b1 <- get_beta(sigma1, muh1, 1, 0.5)
    b2 <- get_beta(sigma2, muh2, 1, 0.5)
    from = -1000
    to = 1000
    p1 <- (b2-b1-from*(a1-a2)[1])/(a1-a2)[2]
    p2 <- (b2-b1-to*(a1-a2)[1])/(a1-a2)[2]
    
    
    plot(xl[,1],xl[,2], pch = 21, xlab = 'x', ylab= 'y', col = colors[xl[,3]], asp = 1, bg=colors[xl[,3]])
    
    lines(c(from,to),c(p1,p2), col="blue", lwd=2)
}

get_mu_with_hat <- function(xl){
  l <- dim(xl)[1] 
  return(c(sum(xl[,1])/l, sum(xl[,2])/l))
}

get_sigma_with_hat <- function(xl, mu_h){
  l <- dim(xl)[1] 
  n <- dim(xl)[2]
  
  sigma <- matrix(0, n, n)
  
  for(i in 1 : l){
    sigma <- sigma + ((xl[i,] - mu_h) %*% t(xl[i,] - mu_h))/(l)
  }
  
  return(sigma)
}

get_alpha <- function(sigma, mu){
  return(solve(sigma)%*%mu)
}

get_beta <- function(sigma, mu, lamda, P){
  return(log(lamda*P) - 0.5 * t(mu)%*%solve(sigma)%*%mu)
}