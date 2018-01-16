main <- function(objectCounter = 200){
  library(MASS)
  sigma1 <- matrix(c(1, 0, 0, 4),2,2)
  sigma2 <- matrix(c(1, 0, 0, 4),2,2)
  mu1 <- c(0,0)
  mu2 <- c(6,7)
  #set.seed(123)
  x1 <- mvrnorm(n = objectCounter, mu1, sigma1)
  x2 <- mvrnorm(n = objectCounter, mu2, sigma2)
  
  xy1 <- cbind(x1, -1)
  xy2 <- cbind(x2, -1)
  xy1 <- cbind(xy1,1) 
  xy2 <- cbind(xy2,2) 
  
  xl <- rbind(xy1,xy2)
  l <- dim(xl)[1]
  colors <- c("red", "green")
  print(xl)
  print(min(xl[,2]))
  
  #Normalization
  xl[,1] <-  (xl[,1] - min(xl[,1]))/(max(xl[,1]) - min(xl[,1]))
  xl[,2] <-  (xl[,2] - min(xl[,2]))/(max(xl[,2]) - min(xl[,2]))
  plot(xl[,1],xl[,2], pch = 21,main = "LogRegression", xlab = 'feature 1', ylab= 'feature 2', col = colors[xl[,4]], asp = 1, bg=colors[xl[,4]])
  #x  <- seq(-5, 5, len = 100)
 # plot(x, sigmoid(x))
  #stochGrad(xl, 0.5, 1)
  w <- stochGrad(xl, 0.5, 1)
  #points(xl[,1],xl[,2], pch = 21, col = colors[xl[,4]], bg=colors[xl[,4]], asp = 1)
  x <- c(0.1, 0.5, 0.7)
  y <- c(0.2, 0.5, 0.7)
 
  #points(x,y, pch = 21, col = "blue", bg="blue", asp = 1)
  
}

losefunction <- function(x){
  return (log2(1+exp(-x))) 
}

sigmoid <- function(x){
  return (1/(1+exp(-x))) 
}

getQ <- function(xl, w){
  new_classes <- xl[,4]
  for(i in 1 : dim(xl)[1]){
    if(new_classes[i] == 1){
      new_classes[i] = -1
    } else {
      new_classes[i] = 1
    }
  }
  res <- 0
  for(i in 1 : dim(xl)[1]){
    res <- res + (losefunction(crossprod(xl[i,1:3],w) * new_classes[i]))
  }
  return(res)
}

stochGrad <- function(xl, teta, lamda){
  new_classes <- xl[,4]
  for(i in 1 : dim(xl)[1]){
    if(new_classes[i] == 1){
      new_classes[i] = -1
    } else {
      new_classes[i] = 1
    }
  }
  
  l <- dim(xl)[1]
  n <- dim(xl)[2]-1 
  w <- c();
  w <-runif(n, -1/(2*n), 1/(2*n))
  Q <- getQ(xl, w)
  print(Q)
  
  cnt <- 0
  while(TRUE){
    ind <- sample(1 : l, 1)
    x_rand <- xl[ind,]  
    marg <- crossprod(x_rand[1:3],w) * new_classes[ind]
    eps <- (losefunction(marg))  
    #print(eps) 
      w <- w + teta * new_classes[ind] * x_rand[1:3] * sigmoid(-marg)
      #w <- w - teta * sigmoid(-marg)
      cnt <- cnt + 1
      abline(w[3]/w[2],-w[1]/w[2], col = "grey")
    
    Q_old <- Q
    Q <- (1 - lamda)*Q + lamda*eps
    #cnt <- cnt + 1
    if(abs(Q_old - Q) < 0.0001)
      break;
    print(Q)
    
  }
  abline(w[3]/w[2],-w[1]/w[2], col="red", lwd = 3)
  print(cnt)
  print(w)
  ver <- 0
  ind <- sample(1 : l, 3)
  print(ind)
  x_rand <- xl[ind,]
  print(x_rand)
  y_rand <- new_classes[ind]
  print(y_rand)
  print(crossprod(x_rand[1:3],w))
  #w <- c(w[1], w[2])
  for(i in 1:3){
    ver[i] <- sigmoid(crossprod(x_rand[i,1:3],w) * y_rand[i])
  }
  print(ver)
  points(x_rand[,1],x_rand[,2], pch = 21, col = "blue", bg="blue", asp = 1)
  text(x_rand[,1],x_rand[,2],col = "blue", labels = c(round(ver[1], 5), round(ver[2], 5), round(ver[3], 5)), pos=3, cex = 1.5)
  #return(ver)
}
