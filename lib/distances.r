difference = function(m1,m2){
  df = m1 - m2
  sum(abs(Re(df)), abs(Im(df)))
}

euclidean = function(m1, m2){
  sum(sqrt(colSums((m1 - m2)^2)))
}

euclideanIm = function(m1, m2){
  df = m1 - m2
  sqrt(sum(Re(df)^2, Im(df)^2))
}

manhattan = function(m1, m2){
  sum(abs(m1-m2))
}

levenshtein = function(s1, s2){
  adist(s1, s2)
}
