#install.packages("rgl")

library(rgl)

#plot3d()

run <- function(dataFile,customSeparator){
 read.table(dataFile, sep = customSeparator)
 #read.csv("http://www.ats.ucla.edu/stat/data/hsb2.csv")
}

letterToVector <- function(letter){
if(letter == "A"){x <- c(0,0,0) }
else if (letter == "C"){x <- c(1,0,0)}
else if (letter == "G"){x <- c(0,1,0)}
else if (letter == "T"){x <- c(0,0,1)}
x
}

createMatrix <- function(string){

splitted <- strsplit(string, '')[[1]]
	for(entry in splitted){
	
	}

}

evaluate <- function(data1, data2){

}

transform <- function(data, func){
	for(i in 1:nrow(data)) {
#apply(data[i,],func)		


	}
}


for(i in 1:nrows(m)) {apply(m[i,],sum)	}
