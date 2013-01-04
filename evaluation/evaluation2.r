setwd("f:/dev/school/ut/mining/project/evaluation")

source('../lib/tranprox.r')

data = readSequences("dna8.csv")
plotLine(convertToMatrix(data[4]))

labels = as.matrix(louter(data, data, pairname))
dist.levenshtein = run(data, compose(), levenshtein)
dist.none = run(data, compose(convertToMatrix), euclidean)
dist.blur = run(data, compose(convertToMatrix, mkBlur()), euclidean)
dist.fourier = run(data, compose(convertToMatrix, fourier), euclideanIm)
dist.haar = run(data, compose(convertToMatrix, haar), euclidean)

matplot(cbind(dist.levenshtein, dist.none, dist.blur, dist.fourier, dist.haar)[1:50,], type="l", axes=FALSE)
axis(2)
axis(3, pos=6, seq(labels), labels, las=2, cex.axis=0.7)
legend(0,4, c("Levenshtein", "None", "Blur", "Fourier", "Haar"), fill=1:6)

lm(dist.levenshtein ~ dist.none + dist.haar)

frac = function(data){ data - trunc(data) }

lm(dist.levenshtein ~ floor(dist.blur) + frac(dist.blur))
lm(dist.fourier ~ dist.none)

dist.comp = ceiling(floor(dist.blur) * 1.2 + frac(dist.blur) * 1.25 + 0.3481)
matplot(cbind(dist.levenshtein, dist.none, dist.comp)[1:50,], type="l", axes=FALSE)

matplot(cbind(dist.levenshtein, dist.none, ceiling(dist.blur) + 1)[1:50,], type="l", axes=FALSE)

dist.comp = (dist.fourier/12 + dist.haar)/1.6

matplot(cbind(dist.levenshtein, dist.none * 0.6082 - dist.fourier*0.0 - dist.haar*0.2613)[1:50,], type="l", axes=FALSE)
axis(2)
axis(3, pos=6, seq(labels[1:100,]), labels[1:100,], las=2, cex.axis=0.7)



