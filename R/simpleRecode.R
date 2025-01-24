#' Simple function to recode alphanumeric data
#'
#' @param data a vector of data to be recoded
#'
#' @return a data frame with the original codes as the first column and the recodes as a second column
#'
#' @examples
#' y<-c("2","4","a7","b854fg","34")
#' simpleRecode(y)


rrcData<-function(local, s, d, h = TRUE, md = c(""," ","NA")){
  #Leitura de dados
  tipo<-stringr::str_extract(local,"(\\w+)$")
  if(stringr::str_detect(local,"https://docs.google.com/spreadsheets")){
    tipo<-"gsheet"
    }
  if(tipo == basename(local)){
    tipo<-"txt"
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
  #data<-unique(data)
  #codes<-1:length(data)
  #mapa<-data.frame("Original_Codes"=data,"Recodes"=codes)
}
