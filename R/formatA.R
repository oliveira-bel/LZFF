#' Data Formatting
#'
#' @description
#' Function for formatting phenotypic data following the rules of the BLUPF90 software.
#'
#' @param udata data frame received from rrcData function with unformatted data.
#' @param omd missing data value to be written in the output file.
#' @param of output file name.
#' @param EoL end of line indicator. Unix and Linux uses "\\n", while Windows uses "\\r\\n".
#'
#' @return a formatted file.
#' @examples
#' #Creating a data
#' y<-data.frame(id=c("id1","id2","id3","id4"),sire=c("s1","s2","s3","s4"),dam=c("d1","d2","d3","d4"),psn=c(10,12,13,16),pdm=c(1,5,8,9))
#'
#' #formatA data
#' formatA(udata= y, omd = "0", of = "formatted_file", EoL = "\n")
#' unlink("formatted_file")
#'
#'
#' @export


formatA<-function(udata, omd = "0", of = "formatted_file", EoL = "\n"){
  if(stringr::str_detect(of,"#")){
    stop("File name cannot contain a #. Choose a name without a #")
  }

  utils::write.table(udata, file = of, quote = FALSE, sep = " ",
                     row.names = FALSE, col.names = FALSE, eol = EoL, na = omd)
}
