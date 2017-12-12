Nu = function(x, mu, cov) {
  1/sqrt(2*pi*det(cov))*exp(-1/2*((x-mu) %*% solve(cov) %*% (x-mu)))
}

super = function(mu, cov, what=1, title) {  
  title = title;
  from = -4
  to = 4
  ticks = 200
  det = det(cov)
  a = cov[1,1]
  b = cov[1,2]
  c = cov[2,1]
  d = cov[2,2]
  mu1 = mu[1]
  mu2 = mu[2]
  A = d/det
  B = (-b-c)/det
  C = a/det
  D = (-2*d*mu1 + b*mu2 + c*mu2)/det
  E = (b*mu1 + c*mu1 - 2*a*mu2)/det
  f = (d*mu1*mu1-b*mu1*mu2-c*mu1*mu2+a*mu2*mu2)/det
  
  x = seq(from, to, (to - from) / ticks)
  y = seq(from, to, (to - from) / ticks)
  
  z = outer(x, y, function(x, y) 1/sqrt(2*pi*d) * exp(-1/2 * (A*x*x+B*y*x+C*y*y+D*x+E*y+f)))
  
  if (what == 1) {
    contour(x, y, z, levels=seq(0,1,0.05), main=title, drawlabels=T, asp=1)
  } else {
    mx = max(z)
    mn = min(z)
    l1 = length(x)
    l2 = length(y)
    pos = 1
    mat = matrix(0, nrow=l1*l2, ncol=3)
    for (i in 1:l1) {
      for (j in 1:l2) {
        col = rgb(z[i,j] / mx, 0, z[i,j] / mx)
        mat[pos,] = c(x[i], x[j], col)
        pos = pos + 1
      }
      print(sprintf("%d", i))
    }
    print(mat)
    plot(mat[,1], mat[,2], col=mat[,3], pch=22, main=title, asp=1, xlab="First feature", ylab="Second feature")
  }
}

par(mfrow=c(2,2))
what = 0
super(c(0,0), matrix(c(1,0,0,1), nrow=2, ncol=2), what=what, title="Same variance & no correlation")
super(c(0,0), matrix(c(1,1,0,1), nrow=2, ncol=2), what=what, title="Features are correlated")
super(c(0,0), matrix(c(3,0,0,1), nrow=2, ncol=2), what=what, title="No correlation")
super(c(0,0), matrix(c(1,0,0,3), nrow=2, ncol=2), what=what, title="No correlation")
