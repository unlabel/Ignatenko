readinteger <- function()
{ 
  n <- readline(prompt="Enter an integer: ")
  if(!grepl("^[0-9]+$",n))
  {
    return(readinteger())
  }
  
  return(as.integer(n))
}

#compute distance between 2 points
distance <- function(x1, x2)
{
  return(sqrt(sum((x1 - x2) ^ 2)))
}

#return class depending to distance to neighbors
#inputData - iris dataframe (or modify code a bit and pass any array or dataframe)
#k - how many neigbors should algorithm search for
knn <- function (inputData, x1, x2, k)
{
  rows <- nrow(inputData)
  distances <- matrix(NA, nrow = rows, ncol = 2)
  for (i in 1:rows)
  {
    if (inputData[i, "Species"] == "setosa")
      distances[i, 1] <- 1
    else if (inputData[i, "Species"] == "versicolor")
      distances[i, 1] <- 2
    else
      distances[i, 1] <- 3
    distances[i, 2] <-
      distance(c(x1, x2), c(inputData[i, 3], inputData[i, 4]))
  }
  
  distances <- distances[order(distances[, 2]), ]
  near <- c()
  for (i in 1:k)
  {
    near[i] <- distances[i, 1]
  }
  t <- table(near)
  class <- names(t)[which.max(t)]
  if (class == 1)
    return("setosa")
  else if (class == 2)
    return("versicolor")
  else
    return("virginica")
}

#leave one out for knn
#X - input dataframe
#k - parametr passed to knn
#returns errors quantity (natural value, not real one)
loo.knn <- function(inputData, k)
{
  rows <- nrow(inputData)
  errCnt = 0
  for (i in 1:rows)
  {
    ans <- knn(inputData[-i, ], inputData[i, 3], inputData[i, 4], k)
    if (ans != inputData[i, "Species"])
      errCnt = errCnt + 1
  }
  return(errCnt)
}

#draws classMap for knn
#k - number of neigbors
#output - R graphics
printer<-function (k)
{
  dev.new(width=5, height=4)
  colors <- c("setosa" = "red", "versicolor" = "green3",
              "virginica" = "blue")
  plot(iris[, 3:4], pch = 22, bg = colors[iris$Species], col =
         colors[iris$Species], asp = 1)
  
  for (i in (seq(0,3,0.1)))
  {
    for (j in (seq(0,8,0.1))){
      points(j,i,pch = 22, col = colors[knn(iris,j,i,k)], asp=1)
    }
  }
  
  legend(1,4,c("setosa","versicolor","virginica"),c("setosa" = "red", "versicolor" = "green3","virginica" = "blue"))
}

presentaion <- function()
{
  cat("Specify what you want:", "\n\t 1. knn", "\n\t 2. loo for knn", "\n\t 3. draw class map")
  answer <- readinteger()
  if (answer == 1)
  {
    cat("\nspecify point's x coordiate: ")
    x1 <- readinteger()
    cat("\nspecify point's y coordiate: ")
    x2 <- readinteger()
    cat("\nspecify neigbors count: ")
    k <- readinteger()
    cat("\nanswer: ", knn(iris,x1,x2,k))
    break
  }
  if (answer == 2)
  {
    cat("\nspecify neigbors count: ")
    k <- readinteger()
    cat("\nanswer: ", loo.knn(iris,k))
    break
  }
  if (answer == 3)
  {
    cat("\nspecify neigbors count: ")
    k <- readinteger()
    cat("draw in progress... ")
    printer(k)
    cat("Done!")
  }
}