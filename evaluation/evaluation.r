#install.packages("rgl")

library(rgl)

#plot3d()

run <- function(dataFile,customSeparator){
 read.table(dataFile, sep = customSeparator)
 #read.csv(dataFile)
}

##CASE SENSITIVE
letterToVector <- function(letter){
if(letter == "A"){x <- c(0,0,0) }
else if (letter == "C"){x <- c(1,0,0)}
else if (letter == "G"){x <- c(0,1,0)}
else if (letter == "T"){x <- c(0,0,1)}
}

createMatrix <- function(string){
splitted <- strsplit(string, '')[[1]]
m <- matrix(0,length(splitted),3)
for(i in 1:length(splitted)) {
 m[i,] <- letterToVector(splitted[i]) 
}
m
}

evaluate <- function(data1, data2){

}

##fft - fourier transform
transform <- function(data, func){
apply(data,1,func)
#	for(i in 1:nrow(data)) {
#apply(data[i,],func)		
#	}
}


