source('../lib/tranprox.r')

data = readSequences("dna.csv")[500:3500]
queries = readSequences("dna.csv")[3500:3600]
dataM = lapply(data, convertToMatrix)

hamming.data = dataM
hamming.norm = 1
hamming.tran = convertToMatrix

blur.data = lapply(dataM, mkBlur())
blur.norm = 1.470526 # from eval.R
blur.tran = compose(convertToMatrix, mkBlur())

inRange = function(data, x, range, tran, norm, dist){
  t = tran(x)
  unlist(lapply(data, function(d){ floor(dist(d, t) * norm) <= range }))
}

hquery = function(x, range){
  inRange(hamming.data, x, range, hamming.tran, hamming.norm, euclidean)
}

bquery = function(x, range){
  inRange(blur.data, x, range, blur.tran, blur.norm, euclidean)
}

query = function(x, range){
  h = inRange(hamming.data, x, range, hamming.tran, hamming.norm, euclidean)
  b = inRange(blur.data, x, range, blur.tran, blur.norm, euclidean)
  h + b >= 1
}

lquery = function(x, range){
  inRange(data, x, range, identity, 1, levenshtein)
}

confusion = function(q, range, qry){
  approx = qry(q, range)
  leven = lquery(q, range)
  table(leven, approx)
}

validate = function( queries, range, qry ) {
  r = matrix(0,ncol=2,nrow=2)
  for( i in seq(queries) ){
    q = queries[i]
    c = confusion(q, range, qry)
    r = r + c
  }
  r / sum(r)
}

t = validate(queries, 3)

r3 = validate(queries, 3, query)
r4 = validate(queries, 4, query)
r5 = validate(queries, 5, query)

h3 = validate(queries, 3, hquery)
h4 = validate(queries, 4, hquery)
h5 = validate(queries, 5, hquery)

b3 = validate(queries, 3, bquery)
b4 = validate(queries, 4, bquery)
b5 = validate(queries, 5, bquery)


sensitivity = function(confs, ncol){
  matrix(lapply(confs, function(x){ x[2,2] / sum(x[2,]) }), ncol=ncol, byrow=T)
}

precision = function(confs, ncol){
  matrix(lapply(confs, function(x){ x[2,2] / sum(x[,2]) }), ncol=ncol, byrow=T)
}

falseDiscovery = function(confs, ncol){
  matrix(lapply(confs, function(x){ x[1,2] / sum(x[,2]) }), ncol=ncol, byrow=T)
}

accuracy = function(confs, ncol){
  matrix(lapply(confs, function(x){ x[1,1] + x[2,2] / sum(x) }), ncol=ncol, byrow=T)
}

sensitivity( list(r3, r4, r5, h3, h4, h5, b3, b4, b5), 3)
precision( list(r3, r4, r5, h3, h4, h5, b3, b4, b5), 3)
falseDiscovery( list(r3, r4, r5, h3, h4, h5, b3, b4, b5), 3)
accuracy( list(r3, r4, r5, h3, h4, h5, b3, b4, b5), 3)
