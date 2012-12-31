readSequences = function(filename){
  # return a list of sequence strings
}

pairname = function(a,b){
  paste('(',a,',',b,')', sep="")
}

run = function(data, transform, distance){
  transformed = lapply(data, transform)
  pairnames = outer(data, data, pairname)
  distances = outer(transformed, transformed, distance)
  rownames(distances) = pairnames
  distances
}

compose = function(...){
  fns = list(...)
  function(seq){
    for(i in 1:length(fns)){
      seq = fns[[i]](seq)
    }
    seq
  }
}

identity = function(x){ x }

convertToMatrix = function(s){
  # 
}

mkInterpolate = function(n, method){
  function(seq){
    interpolate3(seq, n, method)
  }
}

data = readSequences("data.csv")

dist.fourier = run(data, compose(convertToMatrix, mkInterpolate(5, linear), fourier), euclidean)
dist.haar = run(data, compose(convertToMatrix, mkInterpolate(5, linear), haar), euclidean)

matplot(cbind(fourier, haar), type="l")