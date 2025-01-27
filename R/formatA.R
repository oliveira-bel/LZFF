#' Function for phenotypic data file formatting to BLUPF90 software
#'
#' @param local path of the data file
#' @param h logical value indicating the presence of header
#' @param s field separator used in data file
#' @param d character used as decimal separator
#' @param md value used to indicate missing values in data file
#' @param of name of the output file
#' @param omd missing data value to be written in the output file
#' @param eol end of line marker
#'
#' @return a data file in the appropriate format to run the software
#' @export
#'

formatA<-function(dados,omd=0){

  #Replacing NAs
  dfnas<-is.na(dados)
  for(i in 1:length(dados)){
    if(sum(dfnas[,i])!=0){
      for(j in 1:nrow(dfnas)){
        if(dfnas[j,i]==TRUE){
          dados[j,i]<-omd
        }
      }
    }
  }
  # Writing the formated data file
  utils::write.table(dados,of,quote=FALSE,sep=" ",row.names=FALSE,
                     col.names=FALSE,eol=EoL)
}
