setwd("f:/dev/school/ut/mining/project/evaluation")

source('../lib/tranprox.r')

data = readSequences("dna8.csv")
plotLine(convertToMatrix(data[4]))

labels = as.matrix(louter(data, data, pairname))
dist.levenshtein = run(data, compose(), levenshtein)
dist.none = run(data, compose(convertToMatrix), euclidean)
dist.blur = run(data, compose(convertToMatrix, mkBlur()), euclidean)
dist.fourier = run(data, compose(convertToMatrix, fourier), euclideanIm)
dist.fourierM = run(data, compose(convertToMatrix, fourier), manhattan)
dist.fourierRe = run(data, compose(convertToMatrix, fourier, Re), euclidean)
dist.fourierIm = run(data, compose(convertToMatrix, fourier, Im), euclidean)
dist.haar = run(data, compose(convertToMatrix, haar), euclidean)

matplot(cbind(dist.levenshtein, dist.none, dist.blur, dist.fourierRe, dist.fourierIm, dist.haar)[1:50,], type="l", axes=FALSE)

matplot(cbind(dist.levenshtein, dist.none, dist.fourierRe, dist.fourierIm, dist.fourier*1.5, dist.fourierM/2)[1:50,], type="l", axes=FALSE)
axis(2)
#axis(3, pos=6, seq(labels), labels, las=2, cex.axis=0.7)
legend(2,4, c("Levenshtein", "None", "FourierRe", "FourierIm", "Fourier", "FourierM/2"), fill=1:6, cex=0.7)

lm(dist.levenshtein ~ dist.none + dist.haar)

frac = function(data){ data - trunc(data) }

lm(dist.levenshtein ~ floor(dist.blur) + frac(dist.blur))
lm(dist.fourier ~ dist.none)

dist.comp = ceiling(floor(dist.blur) * 1.2 + frac(dist.blur) * 1.25 + 0.3481)
matplot(cbind(dist.levenshtein, dist.none, dist.comp)[1:50,], type="l", axes=FALSE)
matplot(cbind(dist.levenshtein, dist.none, floor(dist.blur) + 2)[1:50,], type="l", axes=FALSE)

dist.comp = (dist.fourier/12 + dist.haar)/1.6

dist = cbind(dist.levenshtein, dist.none * 0.6082 - dist.fourier*0.0 - dist.haar*0.2613)
colnames(dist) = labels
matplot(dist[1:50,], type="l", axes=T)
axis(2)
axis(3, pos=6, seq(labels[1:100,]), labels[1:100,], las=2, cex.axis=0.7)



