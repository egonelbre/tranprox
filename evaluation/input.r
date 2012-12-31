readSequences = function(filename){
  as.vector(as.matrix(read.table(filename,sep=';')))
}
