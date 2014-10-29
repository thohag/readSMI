#' Read folder
#' 
#' This function reads all files in a folder.
#' @param file smi filename to read.
#' @examples
#' 1+1 #figure it out!



readSMI = function(file) {
  

  datas <- read.csv(file=file, header=T, sep="\t", skip=38, stringsAsFactors=FALSE)
  msgcol = names(datas)[4]
  datas = data.table(datas)
  datas[Type == "MSG", MSG:=strsplit(get(msgcol),"# Message: ")[[1]][2] ,by=Time]
  
  repeat.before = function(x) {   # repeats the last non NA value. Keeps leading NA
    ind = which(!is.na(x))      # get positions of nonmissing values
    if(is.na(x[1]))             # if it begins with a missing, add the 
      ind = c(1,ind)        # first position to the indices
    rep(x[ind], times = diff(   # repeat the values at these indices
      c(ind, length(x) + 1) )) # diffing the indices + length yields how often 
  } 
  
  datas$MSG = repeat.before(datas$MSG)
  
  datas = datas[Type == "SMP",]
  
  datas <- datas[, eval(msgcol):=as.numeric(get(msgcol))] #make sure it is actually numeric!
  
  return(datas)
  
}