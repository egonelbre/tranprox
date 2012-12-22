library("fields")
library("ggplot2")
library("rgl")

clamp = function(value, low, high){
  if(value < low) low
  else if (value > high) high
  else value
}

interpolate3 = function(mat, n, kernel){
  len = dim(mat)[2]
  res = apply(matrix(1:n), 1, function(i){ kernel(mat, len, i, n) })
  matrix(res, nrow=n, ncol=3)
}


linear = function(x, xlen, i, n){
  xlow = floor((i-0.5)*xlen/n)
  xhigh = ceiling((i+0.5)*xlen/n)
  
  li = clamp(xlow, 1, xlen)
  hi = clamp(xhigh, 1, xlen)
  count = hi - li + 1
  
  slice = matrix(mat[,li:hi], nrow=3)
  rowSums(slice)/count
}


##convert letter into a vector
letterToVector <- function(letter){
  if(letter == "A" || letter == "a"){x <- c(0,0,0) }
  else if (letter == "C" || letter == "c"){x <- c(1,0,0)}
  else if (letter == "G" || letter == "g"){x <- c(0,1,0)}
  else if (letter == "T" || letter == "t"){x <- c(0,0,1)}
  else if (letter == "U" || letter == "u"){x <- c(1,0,1)}
  else {x <- c(-1,-1,-1)}
}

##convert string into matrix
convertToMatrix <- function(string){
  splitted <- strsplit(string, '')[[1]]
  m <- matrix(0,length(splitted),3)
  for(i in 1:length(splitted)) {
    m[i,] <- letterToVector(splitted[i]) 
  }
  m
}


##compare to matrices
compare <- function(m1, m2){
  n<- max(nrow(m1),nrow(m2))
  
  if(nrow(m1) < nrow(m2)){
  m1 <- interpolate3(m1,n,linear)
  }
  else{
    m2 <- interpolate3(m2,n,linear)
  }
  
  return sqrt(sum((m1 - m2)^2))
}

#apply transformation function to matrix
transform <- function(m1, func){
  
}

transform_fourier <- function(m1){
  t(apply(m1,1,fft))
}

transform_haar <- function(m){
  
}

drawLines <- function(m,color){
  points3d(m[,1],m[,2],m[,3],col=color)
  lines3d(m[,1],m[,2],m[,3],col=color)
  
}

#TODO: still need to think how to export the picture
plotToFile <- function(m1,m2){
  
  drawLines(m2,"pink")
  drawLines(m1,"green")
  
  drawLines(transform(m1),"red")
  drawLines(transform(m2),"blue")
  

  
}

plotDistances <- function(distance_normal,distance_fourier){
 # jpeg('distances.jpg')
  #plot(distance_normal,type="l",col="red")
  #plot(distance_fourier,type="l",col="blue")
  #plot(distance_haar,type="l",col="green")
  #dev.off()
  qplot(distance_normal[,1],distance_normal[,2]) + geom_line()
  qplot(distance_fourier[,1],distance_fourier[,2]) + geom_line()
  
  
  
}


run <- function(fileName, separator){
  A <- as.matrix(read.table(fileName,sep=separator))
  B <- as.vector(A)
  
  n = (length(B)-1)
  distance_normal <-  matrix(0,n,2)
  distance_fourier <-  matrix(0,n,2)
  distance_haar <-  matrix(0,n,2)
  #distance_ <-  matrix(0,n,2)
  
  for(i in 1:n) {
    
    m1 <- convertToMatrix(B[i])
    m2 <- convertToMatrix(B[i+1])
    
    
    distance_normal[i,] = c(i,compare(m1,m2))
    distance_fourier[i,] <- c(i,compare(transform_fourier(m1),transform_fourier(m2)))
    #distance_haar[i,] <- c(i, compare(transform_haar(m1),transform_haar(m2)))
    #plotToFile(m1,m2)
  }
  
  
  plotDistances(distance_normal,distance_fourier)
  
  #TODO: save distances into csv - each type into separate column
}


#TODO: calculate levenstein?
#plot levenstein and transformed vs. normal 

run("/home/demente/Documents/data.csv",";")
