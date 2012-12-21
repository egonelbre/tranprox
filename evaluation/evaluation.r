library("fields")
library("rgl")

##convert letter into a vector
letterToVector <- function(letter){
  if(letter == "A" || letter == "a"){x <- c(0,0,0) }
  else if (letter == "C" || letter == "c"){x <- c(1,0,0)}
  else if (letter == "G" || letter == "g"){x <- c(0,1,0)}
  else if (letter == "T" || letter == "t"){x <- c(0,0,1)}
  else {x <- c(-1,-1,-1)}
}

##convert string into matrix
convertToMatrix <- function(string){
  splitted <- strsplit(string, '')[[1]]
  m <- matrix(0,length(splitted),3)
  for(i in 1:length(splitted)) {
    m[i,] <- letterToVector(splitted[i]) 
  }
  m
}


##compare to matrices
compare <- function(m1, m2){
  
  print("Comparing matrices...")
  if(!is.null(m1) && nrow(m1) == nrow(m2)){
    #rdist(m1, m2) #package fields
    
    #temporary solution
    n <- nrow(m1)
    dis <- matrix(0,n,1)
    
    for(i in 1:n){
      
      x1 <- m1[i,1]
      y1 <- m1[i,2]
      z1 <- m1[i,3]
      
      x2 <- m2[i,1]
      y2 <- m2[i,2]
      z2 <- m2[i,3]
      
      
      
      dis[i,1] <- sqrt(sum((x1 - x2) ^ 2,(y1-y2)^2,(z1-z2)^2))
    }
    dis
  }
  else{
    #TODO: interpolation if length is different
    #approx   (x, y = NULL, xout, method = "linear", n = 50, yleft, yright, rule = 1, f = 0, ties = mean)
    #n <- max(nrow(m1),nrow(m2))
  }
}

#apply transformation function to matrix
transform <- function(m1, func){
  
}

transform <- function(m1){
  apply(m1,1,fft)
}

plotToFile <- function(m,color){
  #TODO: save plot to file
  #jpeg('rplot.jpg')
  lines3d(m[,1],m[,2],m[,3],col=color)
  # dev.off()
  
}


run <- function(fileName, separator){
  print("Reading from file...")
  A <- as.matrix(read.table(fileName,sep=separator))
  B <- as.vector(A)
  
  for(i in 1:(length(B)-1)) {
    
    m1 <- convertToMatrix(B[i])
    m2 <- convertToMatrix(B[i+1])
    
    
    c1 <- compare(m1,m2)
    c2 <- compare(transform(m1),transform(m2))
    
    plotToFile(m2,"pink")
    plotToFile(m1,"green")
    
  }  
}


#TODO: calculate levenstein?
#plot levenstein and transformed vs. normal 

run("/home/demente/Documents/data.csv",";")
