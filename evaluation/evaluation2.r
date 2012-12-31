source('input.r')
source('algorithm.r')
source("graph.r")
source('transformations.r')
source('distances.r')


tmp = lapply(data, convertToMatrix)

plotLine = function(mat, col=1){
  tmp = mat
  tmp[1,] = tmp[1,] + seq(tmp[1,])
  lines3d(tmp[1,], tmp[2,], tmp[3,], col=col)
}

open3d()
plotLine(tmp[[9]], col=3)
plotLine(tmp2[[9]], col=2)

library("rgl")
mat = rbind(sin(1:100/10), sin(2+1:100/3), sin(3+1:100/7))
lines3d(mat[1,], mat[2,], mat[3,])
mat2 = mkInterpolate(50, linear)(mat)
lines3d(mat2[1,], mat2[2,], mat2[3,], col=2)

data = readSequences("dna8.csv")[1:8]

labels = as.matrix(louter(data, data, pairname))
dist.levenshtein = run(data, compose(), levenshtein)
dist.none = run(data, compose(convertToMatrix), euclidean)
dist.fourier = run(data, compose(convertToMatrix, fourier), manhattan)
dist.haar = run(data, compose(convertToMatrix, haar), euclidean)

dist.comp = (dist.fourier/12 + dist.haar)/1.6
matplot(cbind(dist.levenshtein, dist.none, dist.fourier/12, dist.haar, dist.comp), type="l", axes=FALSE)
axis(2)
axis(3, pos=6, seq(labels), labels, las=2, cex.axis=0.7)
#legend(0,0, c("Levenshtein", "None", "Fourier", "Haar"), fill=1:6)

?axis

data = readSequences("dna8.csv")
labels = as.matrix(louter(data, data, pairname))
dist.levenshtein = run(data, compose(), levenshtein)
dist.none = run(data, compose(convertToMatrix), euclidean)
dist.fourier = run(data, compose(convertToMatrix, fourier), manhattan)
dist.haar = run(data, compose(convertToMatrix, haar), euclidean)

lm(dist.levenshtein ~ dist.none + dist.fourier + dist.haar)

dist.comp = (dist.fourier/12 + dist.haar)/1.6
matplot(cbind(dist.levenshtein, dist.none * 1.51 + dist.fourier*0.013 - dist.haar*0.011)[1:100,], type="l", axes=FALSE)
axis(2)
axis(3, pos=6, seq(labels[1:100,]), labels[1:100,], las=2, cex.axis=0.7)



