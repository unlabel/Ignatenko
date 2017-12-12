main <- function(objectCounter = 200){
  library(MASS)
  sigma1 <- matrix(c(5, 0, 0, 1),2,2)
  sigma2 <- matrix(c(5, 0, 0, 1),2,2)
  mu1 <- c(0,0)
  mu2 <- c(0,7)
  x1 <- mvrnorm(n = objectCounter, mu1, sigma1)
  x2 <- mvrnorm(n = objectCounter, mu2, sigma2)
  
  xy1 <- cbind(x1, -1)
  xy2 <- cbind(x2, -1)
  xy1 <- cbind(xy1,1) 
  xy2 <- cbind(xy2,2) 
  
  xl <- rbind(xy1,xy2)
  l <- dim(xl)[1]
  colors <- c("red", "green")
  
  print(min(xl[,2]))
 
  #Normalization
  xl[,1] <-  (xl[,1] - min(xl[,1]))/(max(xl[,1]) - min(xl[,1]))
  xl[,2] <-  (xl[,2] - min(xl[,2]))/(max(xl[,2]) - min(xl[,2]))
  plot(xl[,1],xl[,2], pch = 21,main = "ADALINE", xlab = 'feature 1', ylab= 'feature 2', col = colors[xl[,4]], asp = 1, bg=colors[xl[,4]])
  
  stochGrad(xl, 0.1, 0.5)
  points(xl[,1],xl[,2], pch = 21, col = colors[xl[,4]], bg=colors[xl[,4]], asp = 1)
}

loseFunction <- function(x){
  return ((1 - x)^2) 
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
        res <- res + (loseFunction(crossprod(xl[i,1:3],w) * new_classes[i]))
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
     w <- runif(n, -1/(2*n), 1/(2*n))
     Q <- getQ(xl, w)
     print(Q)
     
     cnt <- 0
     while(TRUE){
       ind <- sample(1 : l, 1)
       x_rand <- xl[ind,]  
       eps <- (loseFunction(crossprod(x_rand[1:3],w) * new_classes[ind])) 
       #print(eps)
       #if((crossprod(x_rand[1:3],w) * new_classes[ind]) <0 ){
       w <- w - teta*(crossprod(x_rand[1:3],w) - new_classes[ind])*x_rand[1:3]
       cnt <- cnt + 1
       #}
       Q_old <- Q
       Q <- (1 - lamda)*Q + lamda*eps
       
       if(abs(Q_old - Q) < 0.00001)
         break;
       print(Q)
       abline(w[3]/w[2],-w[1]/w[2], col = "blue")
     }
     abline(w[3]/w[2],-w[1]/w[2], col="red", lwd = 3)
     print(cnt)
}

