clamp = function(value, low, high){
  if(value < low) low
  else if (value > high) high
  else value
}

interpolate3 = function(mat, n, kernel){
  len = dim(mat)[2]
  res = apply(matrix(1:n), 1, function(i){ kernel(mat, len, i, n) })
  matrix(res, nrow=3, ncol=n)
}

# kernels

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

library(ggplot2)


plot(sin(1:100/10), type="l", col=2)
lines(sin(1+1:100/10), col=3)

w = sin(1:100/10)
x = sin(1 + 1:100/10)
matplot(cbind(w,x), type="l")

sum(dnorm(c(1:10), m=3) * c(1:10))

plotLine = function(mat, col=1){
  lines3d(mat[1,], mat[2,], mat[3,], col=col)
}


open3d()

mat = rbind(sin(1:100/10), sin(2+1:100/3), sin(3+1:100/7))
# original
plotLine(mat, 2)

# interpolated with linear
mat2 = interpolate3(mat, 30, linear)
plotLine(mat2, 3)

# interpolated with nearest
# mat3 = interpolate3(mat, 10, nearest)
# plotLine(mat3, 4)

rgl.bringtotop()


