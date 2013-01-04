euclidean = function(m1, m2){
  sqrt(sum((m1 - m2)^2))
}

manhattan = function(m1, m2){
  sum(abs(m1-m2))
}


levenshtein = function(s1, s2){
  adist(s1, s2)
}
