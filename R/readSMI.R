#' readSMI
#' 
#' This function reads SMI eye-tracker files
#' @param file smi filename to read.
#' @param msgcolposition column of MSG data (optional)
#' @param msgprefix prefix to remove from MSG data (optional)
#' @param eventsfile events file for merging with main file (optional / experimental)
#' @examples
#' data = readSMI("AXCPT_Subject_1 Samples.txt")
readSMI = function(file, msgcolposition=4, msgprefix="# Message: ", eventsfile=NULL) {
  
  datas <- readFast(file, msgcolposition)
  #datas <- data.table(read.csv(file=file, header=T, sep="\t", skip=38, stringsAsFactors=FALSE))
  msgcol = names(datas)[msgcolposition]
  
  if (nrow(head(datas[Type == "MSG",])) > 0) {
    
    
    start = nchar(msgprefix)+1
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
    
  }
  
  if (is.factor(datas[,msgcol,with=F][[1]])) {
    datas[, eval(msgcol):=as.character(get(msgcol))]
  }
  datas <- datas[, eval(msgcol):=as.numeric(get(msgcol))] #make sure it is actually numeric!
  
  if (!is.null(eventsfile)) {
    readEvents(eventsfile, datas)
  }
  
  return(datas)
  
}

readFast = function(file, msgcolposition=4) {

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
    
    classes[msgcolposition][[1]] = "character"
    data.table(read.table(file = file, header = TRUE, sep = "\t", quote = "\"",  fill = TRUE, comment.char = "", skip=skip, colClasses = classes))
    
  }, warning = function(warn) {
    
  }, error = function(error) {
    data.table(read.table(file = file, header = TRUE, sep = "\t", quote = "\"",  fill = TRUE, comment.char = "", skip=skip))
  }, finally={
    
  })
  
  return(result)
  
  
}

readEvents = function(eventsfile, data) {
  
  blinks <- read.csv(file=eventsfile, header=F, sep="\t", skip=20)
  
  repeat.before = function(x) {
    ind = which(!is.na(x))
    if(is.na(x[1])) ind = c(1,ind)
    rep(x[ind], times = diff(c(ind, length(x) + 1) ))
  }
  
  #Left
  blinksL = blinks[blinks$V1 == "Blink L",]
  if (nrow(blinksL) > 0) {
    blinksL = transform(blinksL, V4 = as.numeric(as.character(V4)))
    blinksL = transform(blinksL, V5 = as.numeric(as.character(V5)))
    
    data[Time %in% blinksL$V4,BlinkL:=1]
    data[Time %in% blinksL$V5,BlinkL:=0]
    
    data[, BlinkL:=repeat.before(BlinkL)]
    data[is.na(BlinkL),BlinkL:=0]
  }

  
  
  #Right
  blinksR = blinks[blinks$V1 == "Blink R",]
  if (nrow(blinksR) > 0) {
    blinksR = transform(blinksR, V4 = as.numeric(as.character(V4)))
    blinksR = transform(blinksR, V5 = as.numeric(as.character(V5)))
    
    data[Time %in% blinksR$V4,BlinkR:=1]
    data[Time %in% blinksR$V5,BlinkR:=0]
    
    data[, BlinkR:=repeat.before(BlinkR)]
    data[is.na(BlinkR),BlinkR:=0]
  }
  
}