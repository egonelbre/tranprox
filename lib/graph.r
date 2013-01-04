library("rgl")
library("ggplot2")

plotLine = function(mat, col=1){
  lines3d(mat[1,] + seq(mat[1,]), mat[2,], mat[3,], col=col)
}

plotComparsion = function(distance, n, xlabels, linelabels,horizontal, ySpace){
  matplot(distance[1:n,],type="l",axes=FALSE,ylab="Distance")
  axis(1, pos=0, 1:n, linelabels[1:n,], las=2, cex.axis=0.7)
  legend("topleft",xlabels, fill=1:length(xlabels),cex=0.7,horiz=horizontal,x.intersp=0.5,y.intersp=ySpace,box.lwd = 0,box.col = "white",bg = "transparent")
}