source('../lib/tranprox.r')

data = readSequences("dna.csv")

labels = as.matrix(louter(data, data, pairname))
dist.levenshtein = run(data, compose(), levenshtein)
dist.none = run(data, compose(convertToMatrix), euclidean)
dist.blur = run(data, compose(convertToMatrix, mkBlur()), euclidean)
dist.fourier = run(data, compose(convertToMatrix, fourier), euclideanIm)
dist.fourierM = run(data, compose(convertToMatrix, fourier), manhattan)
dist.fourierRe = run(data, compose(convertToMatrix, fourier, Re), euclidean)
dist.fourierIm = run(data, compose(convertToMatrix, fourier, Im), euclidean)
dist.haar = run(data, compose(convertToMatrix, haar), euclidean)

norm.none = calcNorm(dist.levenshtein,dist.none,0.05)
norm.blur = calcNorm(dist.levenshtein,floor(dist.blur) + 2,0.05)
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
impr.blur = calcImpr(dist.none,norm.blur*floor(dist.blur) + 2)

#Fourier
plotComparsion(cbind(dist.levenshtein, dist.none, norm.fourier*dist.fourier,norm.fourierM*dist.fourierM,norm.fourierRe*dist.fourierRe,norm.fourierIm*dist.fourierIm), 50, c("Levenshtein", "None", "Fourier","Fourier Manhattan","Fourier Real","Fourier Imaginary"), labels,FALSE,0.7)

#Haar
plotComparsion(cbind(dist.levenshtein, dist.none, norm.haar*dist.haar), 50, c("Levenshtein", "None", "Haar"), labels,TRUE,0)

#Blur
plotComparsion(cbind(dist.levenshtein, dist.none,floor(dist.blur) + 2), 50, c("Levenshtein", "None", "Blur"), labels,TRUE,0)