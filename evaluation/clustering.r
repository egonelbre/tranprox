source('input.r')
source('algorithm.r')
source("graph.r")
source('transformations.r')
source('distances.r')


data = readSequences("dna8.csv")
labels = as.matrix(louter(data, data, pairname))
dist.levenshtein = runcross(data, compose(), levenshtein)
dist.none = runcross(data, compose(convertToMatrix), euclidean)
dist.fourier = runcross(data, compose(convertToMatrix, fourier), manhattan)
dist.haar = runcross(data, compose(convertToMatrix, haar), euclidean)

# lm(dist.levenshtein ~ dist.none + dist.fourier + dist.haar)

dist.comp = dist.none * 1.51 + dist.fourier*0.013 - dist.haar*0.011
matplot(cbind(dist.levenshtein, dist.comp)[1:100,], type="l", axes=FALSE)
axis(2)
axis(3, pos=6, seq(labels[1:100,]), labels[1:100,], las=2, cex.axis=0.7)

data
dist.comp

par(mfrow=c(1,1))

distance = dist(dist.comp)
cluster = hclust(distance, method="ward")
plot(cluster, hang=-1, label=data)
cluster.composite = cutree(cluster, 7)

distance = dist(dist.levenshtein)
cluster = hclust(distance, method="ward")
plot(cluster, hang=-1, label=data)
cluster.levenshtein = cutree(cluster, 7)


colnames(dist.comp) = data
rownames(dist.comp) = data

colnames(dist.levenshtein) = data
rownames(dist.levenshtein) = data

library(fpc)

cluster.stats(d, cluster.composite, cluster.levenshtein)