#' Formatting Data
#'
#' @param udata data frame received from rrcData function with unformatted data.
#' @param of output file name.
#' @param omd missing data value to be written in the output file.
#' @param traits vector indicating traits columns.
#' @param EoL end of line indicator. Unix and Linux uses "\\n", while Windows uses "\\r\\n".
#' @param width vector specifying the width of columns in the formatted file.
#'
#' @returns a formatted file.
#' @export
#'
formatB<-function(udata, of = "formatA_data", omd = -99999, traits = NULL,
                  width = NULL, EoL = "\n"){
  if(stringr::str_detect(of,"#")){
    stop("File name cannot contain a #. Choose a name without a #")
  }

  #Formatting columns of data
  #Effects columns
  d1<-udata[, -traits]
  i<-1
  while(i <= length(traits)){
    d1<-rbind(d1,udata[, -traits])
    i <- i + 1
  }

  #Traits columns
  d2<-rep(unlist(lapply(udata[, traits], c)), traits)

  #Adding trait number column
  d3<-rep(1:length(traits), each = length(d2)/length(traits))

  udata<-data.frame(d3,d1,d2)

  #Ordering
  fdata<-udata[order(udata[, 2], udata[, 1], na.last = FALSE), ]

  #Ensuring that output file's name is no longer than 30 characters
  length_of<-nchar(of)
  if(length_of > 30){
    of<-stringr::str_sub(of,-30)
  }

  #Writing data with fixed columns format
  gdata::write.fwf(fdata, file = of, sep = " ", na = omd, rownames = FALSE,
                   colnames = FALSE, eol = Eol, scientific = FALSE, width)
}


