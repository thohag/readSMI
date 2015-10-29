#' readSMI
#' 
#' This function reads SMI eye-tracker files
#' @param file smi filename to read.
#' @examples
#' data = readSMI("AXCPT_Subject_1 Samples.txt")



readSMI = function(file) {
  
  datas <- readFast(file)
  #datas <- data.table(read.csv(file=file, header=T, sep="\t", skip=38, stringsAsFactors=FALSE))
  msgcol = names(datas)[4]
    
  start = nchar("# Message: ")+1
  datas[Type == "MSG", MSG:=substring(get(msgcol),start) ,by=Time]
  #datas[Type == "MSG", MSG:=strsplit(get(msgcol),"# Message: ")[[1]][2] ,by=Time]
  
  repeat.before = function(x) {   # repeats the last non NA value. Keeps leading NA
    ind = which(!is.na(x))      # get positions of nonmissing values
    if(is.na(x[1]))             # if it begins with a missing, add the 
      ind = c(1,ind)        # first position to the indices
    rep(x[ind], times = diff(   # repeat the values at these indices
      c(ind, length(x) + 1) )) # diffing the indices + length yields how often 
  } 
  
  #datas$MSG = repeat.before(datas$MSG)
  datas[, MSG:=repeat.before(MSG)]
  
  datas = datas[Type == "SMP",]
  
  if (is.factor(datas[,msgcol,with=F][[1]])) {
    datas[, eval(msgcol):=as.character(get(msgcol))]
  }
  
  datas <- datas[, eval(msgcol):=as.numeric(get(msgcol))] #make sure it is actually numeric!
  
  return(datas)
  
}

readFast = function(file) {

  result = tryCatch({
    
    nlines = 50
    first <- scan(file, what = "", sep = "\n", quote = "\"", 
                  nlines = nlines, quiet = TRUE, skip = 0, strip.white = TRUE, 
                  comment.char = "#", blank.lines.skip = TRUE, skipNul = TRUE
    )
    skip = nlines-length(first)
    
    tab5rows <- read.table(file, header = TRUE, skip=skip, sep="\t", nrows = 5, stringsAsFactors=F)
    classes <- sapply(tab5rows, class)
    #tabAll <- read.table(file, header = TRUE, skip=38, sep="\t", colClasses = classes)
    
    gottime = names(classes)[names(classes) == "Time"]
    if (length(gottime) != 0) {
      classes[gottime][[1]] = "numeric"
    }
    
    classes[4][[1]] = "character"
    data.table(read.table(file = file, header = TRUE, sep = "\t", quote = "\"",  fill = TRUE, comment.char = "", skip=skip, colClasses = classes))
    
  }, warning = function(warn) {
    
  }, error = function(error) {
    data.table(read.table(file = file, header = TRUE, sep = "\t", quote = "\"",  fill = TRUE, comment.char = "", skip=skip))
  }, finally={
    
  })
  
  return(result)
  
  
}
