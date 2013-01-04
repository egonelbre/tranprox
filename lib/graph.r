library("rgl")

plotLine = function(mat, col=1){
  lines3d(mat[1,] + seq(mat[1,]), mat[2,], mat[3,], col=col)
}
