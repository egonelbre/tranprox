source('../lib/tranprox.r')

source('../lib/input.r')
source('../lib/algorithm.r')
source("../lib/graph.r")
source('../lib/transformations.r')
source('../lib/distances.r')

data = readSequences("dna.csv")[1:500]
dataM = lapply(data, convertToMatrix)

louter(data[1:10], data[1:10], pairname)

labels = as.matrix(vouter(data, data, pairname))
dist.levenshtein = run(data, compose(), levenshtein)
dist.none = run(data, compose(convertToMatrix), euclidean)
dist.blur = run(dataM, mkBlur(), euclidean)
dist.blur5 = run(dataM, mkBlur(blur.5), euclidean)
dist.fourier = run(dataM, fourier, euclideanIm)
dist.fourierM = run(dataM, fourier, manhattan)
dist.fourierRe = run(dataM, function(x){Re(fourier(x))}, euclidean)
dist.fourierIm = run(dataM, function(x){Im(fourier(x))}, euclidean)
dist.haar = run(dataM, haar, euclidean)

norm.none = calcNorm(dist.levenshtein,dist.none,0.05)
norm.blur = calcNorm(dist.levenshtein,dist.blur,0.05)
norm.blur5 = calcNorm(dist.levenshtein,dist.blur5,0.05)
norm.fourier = calcNorm(dist.levenshtein,dist.fourier,0.05)
norm.fourierM = calcNorm(dist.levenshtein,dist.fourierM,0.05)
norm.fourierRe = calcNorm(dist.levenshtein,dist.fourierRe,0.05)
norm.fourierIm = calcNorm(dist.levenshtein,dist.fourierIm,0.05)
norm.haar = calcNorm(dist.levenshtein,dist.haar,0.05)

impr.fourier = calcImpr(dist.none,norm.fourier*dist.fourier)
impr.fourierM = calcImpr(dist.none,norm.fourierM*dist.fourierM)
impr.fourierRe = calcImpr(dist.none,norm.fourierRe*dist.fourierRe)
impr.fourierIm = calcImpr(dist.none,norm.fourierIm*dist.fourierIm)
impr.haar = calcImpr(dist.none,norm.haar*dist.haar)
impr.blur = calcImpr(dist.none,norm.blur*dist.blur)
impr.blur5 = calcImpr(dist.none,norm.blur5*dist.blur5)


impr.fourier
impr.fourierM
impr.fourierRe
impr.fourierIm

#Hamming
plotComparsion(cbind(dist.levenshtein, dist.none), 40, c("Levenshtein", "Hamming"), labels, T, 1)

#Fourier
plotComparsion(cbind(dist.levenshtein, dist.none, norm.fourier*dist.fourier,norm.fourierM*dist.fourierM,norm.fourierRe*dist.fourierRe,norm.fourierIm*dist.fourierIm), 50, c("Levenshtein", "Hamming", "Fourier","Fourier Manhattan","Fourier Real","Fourier Imaginary"), labels,T,0.7)

impr.fourier
impr.fourierM
impr.fourierRe
impr.fourierIm

#Haar
plotComparsion(cbind(dist.levenshtein, dist.none, norm.haar*dist.haar), 50, c("Levenshtein", "Hamming", "Haar"), labels,TRUE,0)

#Blur
plotComparsion(cbind(dist.levenshtein, dist.none, norm.blur*dist.blur, norm.blur5*dist.blur5), 50, c("Levenshtein", "Hamming", "Blur", "Blur5"), labels,TRUE,0)


#Evaluate composing

calcImpr(min(dist.none, norm.blur*dist.blur), norm.fourier*dist.fourier)
calcImpr(min(dist.none, norm.blur*dist.blur), norm.blur5*dist.blur5)
calcImpr(min(dist.none, norm.blur*dist.blur), norm.haar*dist.haar)




