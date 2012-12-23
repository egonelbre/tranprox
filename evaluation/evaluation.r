run = function(data, transform, distance){
  transformed = lapply(data, transform)
  pairnames = outer(data, data, pairname)
  distances = outer(transformed, transformed, distance)
  #rownames(distances) = pairnames
  distances
  
}


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

convert = function(interpolation, transformation){
  function(seq){
    t(apply(interpolation(seq),1,transformation))
  }
}



mkInterpolate = function(n, method){
  function(seq){
    interpolate3(seq, n, method)
  }
}

eucledian = function(m1,m2){
  dist = list()
  for(i in seq(m1)){
    dist[i] = sqrt(sum((m1[[i]] - m2[[i]])^2))
  }
  dist
}

pairname = function(a,b){
  paste('(',a,',',b,')', sep="")
}

readSequences = function(filename){
  A = as.matrix(read.table(filename,sep=';'))
  sequences = lapply(A, convertToMatrix)
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

data = readSequences("data.csv")

dist.fourier = run(data, convert(mkInterpolate(5, linear), fft), eucledian)
dist.haar = run(data, convert(mkInterpolate(5, linear), haar), euclidean)


##TODO: (list) object cannot be coerced to type 'double'
matplot(cbind(fourier, haar), type="l")
