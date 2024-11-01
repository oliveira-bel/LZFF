#' Simple function to recode alphanumeric data
#'
#' @param data a vector of data to be recoded
#'
#' @return a data frame with the original codes as the first column and the recodes as a second column
#'
#' @examples
#' y<-c("2","4","a7","b854fg","34")
#' simpleRecode(y)


simpleRecode<-function(data){
  data<-unique(data)
  codes<-1:length(data)
  mapa<-data.frame("Original_Codes"=data,"Recodes"=codes)
}
