pairname = function(a,b){
  paste('(',a,',',b,')', sep="")
}

louter = function(x, y, f){
  xn = length(x)
  yn = length(y)
  i = 1
  res = list()
  for(xi in 1:xn ){
    for(yi in 1:yn ){
      res[[i]] = f(x[[xi]], y[[yi]])
      i = i + 1
    }
  }
  res
}

lcross = function(x, y, f){
  xn = length(x)
  yn = length(y)
  i = 1
  res = matrix(nrow=xn, ncol=yn)
  for(xi in 1:xn ){
    for(yi in 1:yn ){
      res[xi,yi] = f(x[[xi]], y[[yi]])
      i = i + 1
    }
  }
  res
}

run = function(data, transform, distance){
  transformed = lapply(data, transform)
  distances = louter(transformed, transformed, distance)
  unlist(distances)
}

runcross = function(data, transform, distance){
  transformed = lapply(data, transform)
  distances = lcross(transformed, transformed, distance)
  distances
}

compose = function(...){
  fns = list(...)
  function(data){
    for(i in seq(fns)){
      data = fns[[i]](data)
    }
    data
  }
}

calcNorm = function(ref,oth,perc=0.05){
  quantile(ref/oth,1-perc,na.rm=TRUE)
}

calcImpr = function(t1, t2){
  sum(t1 > t2)/length(t1)
}
