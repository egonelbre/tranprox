library("rgl")

plotLine = function(mat, col=1){
  lines3d(seq(mat[1,]), mat[1,], mat[2,], col=col)
}
