## Text --> Matrix

letterToVector <- function(letter){
  if(letter == "A" || letter == "a"){x <- c(0,0) }
  else if (letter == "C" || letter == "c"){x <- c(1,0)}
  else if (letter == "G" || letter == "g"){x <- c(0,1)}
  else if (letter == "T" || letter == "t"){x <- c(1,1)}
  else {x <- c(-1,-1)}
}

convertToMatrix <- function(string){
  splitted <- strsplit(string, '')[[1]]
  m <- matrix(0,length(splitted),2)
  for(i in 1:length(splitted)) {
    m[i,] <- letterToVector(splitted[i]) 
  }
  t(m)
}

## Var length --> Fix length

nearest = function(x, xlen, i, n){
  xi = i*xlen/n
  li = clamp(floor(xi), 1, xlen)
  hi = clamp(ceiling(xi), 1, xlen)
  if( li == hi ){
    x[,li]
  } else {
    p = xi - li
    matrix(x[,li]*(1-p) + x[,hi]*p)
  }
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

interpolate3 = function(mat, n, kernel){
  len = dim(mat)[2]
  res = apply(matrix(1:n), 1, function(i){ kernel(mat, len, i, n) })
  matrix(res, nrow=3, ncol=n)
}

mkInterpolate = function(n, method){
  function(seq){
    interpolate3(seq, n, method)
  }
}

## Haar
library("wavelets")

haar = function(data){
  h = dwt(data, filter="haar",n.levels=1, boundary="reflection", fast=FALSE)
  h@W$W1
}

## FFT

fourier = fft


