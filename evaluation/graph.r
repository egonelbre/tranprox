library("rgl")

plotLine = function(mat, col=1){
  tmp = mat
  tmp[1,] = tmp[1,] + seq(tmp[1,])
  lines3d(mat[1,], mat[2,], mat[3,], col=col)
}
