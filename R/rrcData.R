#' Simple function to recode alphanumeric data
#'
#' @param data a vector of data to be recoded
#'
#' @return a data frame with the original codes as the first column and the recodes as a second column
#'
#' @examples
#' y<-c("2","4","a7","b854fg","34")
#' simpleRecode(y)


rrcData<-function(local, s, d, h = TRUE, md = c(""," ","NA"), colsPed = NULL){
  #Data reading
  tipo<-stringr::str_extract(local,"(\\w+)$")
  if(tipo == basename(local)){
    tipo<-"txt"
  }
  if(stringr::str_detect(local,"https://docs.google.com/spreadsheets")){
    tipo<-"gsheet"
    }
  switch(tipo,
        csv = dados<-utils::read.csv(local, header = h, sep = s, dec = d,
                                     strip.white = FALSE, na.strings = md),

        xls = dados<-as.data.frame(readxl::read_excel(local,na = md), col_names = h),

        xlsx = dados<-as.data.frame(readxl::read_excel(local,na = md), col_names = h),

        ods = dados<-as.data.frame(readODS::read_ods(local,na = md), col_names = h),

        gsheet = {googlesheets4::gs4_auth()
                  dados<-as.data.frame(googlesheets4::read_sheet(local, na = md),
                                       col_names = h)},

        txt = dados<-utils::read.table(local, header = h, sep = s, dec = d,
                                       strip.white = FALSE, na.strings = md),

        print("I did not detect the type of the file.")
        )

  ##########
  #Recoding#
  ##########
  #checking if all values are numeric
  isnum<-sapply(dados,is.numeric)
  mapList<-list()
  for(i in 1:length(isnum)){
    if(!isnum[i] && all(i != colsPed)){
      tempData<-unique(dados[,i])
      codes<-1:length(tempData)
      mapa<-data.frame("Original Codes" = tempData, "Recodes" = codes)
      mapList<-append(mapList, list(mapa))
      index<-match(dados[,i],mapList[[length(mapList)]]$Original_Codes)
      dados[,i]<-mapList[[length(mapList)]]$Recodes[index]
    }
  }
}
