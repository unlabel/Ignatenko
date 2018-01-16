main <- function(objectCounter = 500){
  library(MASS)
  Sigma1 <- matrix(c(4, 0, 0, 1),2,2)
  Sigma2 <- matrix(c(1, 0, 0, 8),2,2)
  mu1 <- c(0,0)
  mu2 <- c(0,4)
  x1 <- mvrnorm(n = objectCounter, mu1, Sigma1)
  x2 <- mvrnorm(n = objectCounter, mu2, Sigma2)
  
  xy1 <- cbind(x1,1) 
  xy2 <- cbind(x2,2) 
  
  xl <- rbind(xy1,xy2)
  
  colors <- c("red", "green")
  
  muh1 <- get_mu_with_hat(x1)
  muh2 <- get_mu_with_hat(x2)
  
  sigma11 <- get_sigma_with_hat(x1, muh1)
  sigma22 <- get_sigma_with_hat(x2, muh2)
  
  print(muh1)
  print(muh2)
  print(sigma11)
  print(sigma22)
  
  sigma1 <- solve(sigma11)
  sigma2 <- solve(sigma22)
  
  alpha1 <- sigma1[1,1]
  beta1 <- sigma1[1,2]
  gamma1 <- sigma1[2,2]
  
  alpha2 <- sigma2[1,1]
  beta2 <- sigma2[1,2]
  gamma2 <- sigma2[2,2]
  
  
  a <- alpha1 - alpha2
  b <- 2*(beta1 - beta2)
  c <- gamma1 - gamma2
  d <- 2*(-alpha1*muh1[1] + alpha2*muh2[1] - beta1*muh1[2] + beta2*muh2[2])
  e <- 2*(-beta1*muh1[1] + beta2*muh2[1] - gamma1*muh1[2] + gamma2*muh2[2])
  f <-  alpha1*muh1[1]^2 - alpha2*muh2[1]^2 + 2*beta1*muh1[1]*muh1[2] - 2*beta2*muh2[1]*muh2[2] + gamma1*muh1[2]^2 - gamma2*muh2[2]^2
        + log(det(sigma11)) - log(det(sigma22))
  
  x <- y <- seq(-20, 20, len = 100)
  z <- outer(x,y,function(x,y) a*x^2+b*x*y+c*y^2+d*x+e*y)
  #PLUG-IN PLOT
  plot(xl[,1],xl[,2], pch = 21, xlab = '1', ylab= '2', col = colors[xl[,3]], asp = 1, bg=colors[xl[,3]])
  contour(x, y, z, levels=-f, drawlabels=FALSE, asp = 0, col="blue", lwd=3, add = TRUE)
  
  
  #x1 <- -15
  #while(x1 < 20){
  #  x2 <- -8;
  #  while(x2 < 13){          
  #    class <- 0;
  #    if(plug(c(x1,x2), muh1, sigma11, 1, 0.5) > plug(c(x1,x2), muh2, sigma22, 1, 0.5)){
  #      class <- 1
  #    } else {
  #      class <- 2
  #    }
  #    points(x1, x2, pch = 21, col=colors[class], asp = 1)
  #    x2 <- x2 + 0.25
  #  }
  #  x1 <- x1 + 0.25
  #}
  print(quality(xl, muh1, sigma11, muh2, sigma22))
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
    sigma <- sigma + ((xl[i,] - mu_h) %*% t(xl[i,] - mu_h))/(l-1)
  }
  
  return(sigma)
}

get_alpha <- function(sigma, mu){
  return(solve(sigma)%*%mu)
}

get_beta <- function(sigma, mu, lamda, P){
  return(log(lamda*P) - 0.5 * t(mu)%*%solve(sigma)%*%mu)
}

plug <- function(x, mu, sigma, lamda, P){
  return(log(lamda*P) - 0.5 * t(x-mu)%*%solve(sigma)%*%(x-mu) - 0.5*log(det(sigma)))
}

quality <- function(xl, muh1, sigma1, muh2, sigma2){
  l <- dim(xl)[1] 
  mis <- 0
  for(i in 1 : l){      
    class <- 0;
    if(plug(xl[i, -3], muh1, sigma1, 1, 0.5) > plug(xl[i, -3], muh2, sigma2, 1, 0.5)){
      class <- 1
    } else {
      class <- 2
    }     
    if(class != xl[i, 3])
      mis <- mis + 1
  }
  return (mis/l)
}
