main<- function(){
  cov <- matrix(NA, 2, 2)
  cov[1,] = c(1,0)
  cov[2,] = c(0,4)
  print(cov)
  
  tmp = 0
  cov <- solve(cov)
  print(det(cov))
  
  det = det(cov)
  
  mu <- c(0,0)
  alpha <- cov[1,1]
  beta <- cov[1,2]
  gamma <- cov[2,2]
  
  a <- gamma/det
  b <- -2*beta/det
  c <- alpha/det
  d <- (-2*gamma*mu[1] + beta*mu[2] + beta*mu[2])/det
  e <- (beta*mu[1] + beta*mu[1] - 2*alpha*mu[2])/det
  f <- (gamma*mu[1]*mu[1]-beta*mu[1]*mu[2]-beta*mu[1]*mu[2]+alpha*mu[2]*mu[2])/det
  
  x <- y <- seq(-3, 3, len = 100) 
  
  z <- outer(x,y,function(x,y) 1/sqrt(2*pi*gamma) * exp(-1/2 * (a*x^2+b*x*y+c*y^2+d*x+e*y+f)))
  lvl <- c()
  lvl[1:10] <- 0
  t <- 0.1
  i <- 1
  while(t <= 1){
      lvl[i] <- round(f - 0.2*log(t), 2)
      i <- i + 1
      t <- t + 0.1
  }
  print(lvl)
  if (tmp == 0) { # ==0 линии, !=0 плотность в точке
  contour(x, y, z, levels=lvl, drawlabels=TRUE, asp = 1)
  } else {
    mx = max(z)
    mn = min(z)
    l1 = length(x)
    l2 = length(y)
    pos = 1
    mat = matrix(0, nrow=l1*l2, ncol=3)
    for (i in 1:l1) {
      for (j in 1:l2) {
        col = rgb(0, z[i,j] / mx, z[i,j] / mx)
        mat[pos,] = c(x[i], x[j], col)
        pos = pos + 1
      }
      print(sprintf("%d", i))
    }
    print(mat)
    plot(mat[,1], mat[,2], col=mat[,3], pch="@", asp=1, xlab="X", ylab="Y")
  }
}