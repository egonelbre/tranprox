setwd("f:/dev/school/ut/mining/project/evaluation")

source('../lib/tranprox.r')

data = readSequences("dna8.csv")
plotLine(convertToMatrix(data[4]))

labels = as.matrix(louter(data, data, pairname))
dist.levenshtein = run(data, compose(), levenshtein)
dist.none = run(data, compose(convertToMatrix), euclidean)
dist.fourier = run(data, compose(convertToMatrix, fourier), euclidieanIm)
dist.haar = run(data, compose(convertToMatrix, haar), euclidean)


matplot(cbind(dist.levenshtein, dist.none, dist.fourier, dist.haar)[1:50,], type="l", axes=FALSE)
axis(2)
axis(3, pos=6, seq(labels), labels, las=2, cex.axis=0.7)
legend(0,4, c("Levenshtein", "None", "Fourier", "Haar"), fill=1:6)

lm(dist.levenshtein ~ dist.none + dist.fourier + dist.haar)

dist.comp = (dist.fourier/12 + dist.haar)/1.6
matplot(cbind(dist.levenshtein, dist.none * 1.617 - dist.fourier*0.0152 - dist.haar*0.02114)[1:50,], type="l", axes=FALSE)
axis(2)
axis(3, pos=6, seq(labels[1:100,]), labels[1:100,], las=2, cex.axis=0.7)



