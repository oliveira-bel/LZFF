#' Read, recode and check
#'
#' @param local data file path
#' @param s field/column separator
#' @param d decimal point used in data file
#' @param h logical value indicating presence of header in data file
#' @param md missing data indicator
#' @param colsPed identification of columns related to pedigree data
#' @param colsTraits identification of columns related to traits
#' @param colsDate identification of columns related to Dates
#'
#' @description
#' Simple function for read, recode and perform some checks in a data file.
#'
#' @return a data frame with data file columns read and recoded as needed. Pedigree data are not recoded by this function
#' @export

rrcData<-function(local, s, d, h = FALSE, md = c(""," ","NA"), colsPed,
                  colsTraits, colsDate){
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
                                     strip.white = FALSE, na.strings = md, fill = TRUE),

        xls = dados<-as.data.frame(readxl::read_excel(local, na = md, col_names = h)),

        xlsx = dados<-as.data.frame(readxl::read_excel(local, na = md, col_names = h)),

        ods = dados<-as.data.frame(readODS::read_ods(local, na = md, col_names = h)),

        gsheet = {googlesheets4::gs4_auth()
                  dados<-as.data.frame(googlesheets4::read_sheet(local, na = md),
                                       col_names = h)},

        txt = dados<-utils::read.table(local, header = h, sep = s, dec = d,
                                       strip.white = FALSE, na.strings = md, fill = TRUE),

        print("I did not detect the type of the file.")
        )

  ##########
  #Recoding#
  ##########
  for(i in 1:length(dados)){
    if(all(i != c(colsPed, colsTraits, colsDate))){
      tempData<-unique(dados[,i])
      codes<-1:length(tempData)
      mapa<-data.frame("Original.Codes" = tempData, "Recodes" = codes)
      index<-match(dados[,i],mapa$Original.Codes)
      dados[,i]<-mapa$Recodes[index]
    }
  }

  #Checking if the file is ASCII
  dadosTemp<-NULL
  dadosTemp<-sapply(dados, paste0, collapse = " ")
  stopifnot(all(grepl("^[ -~]+$", dadosTemp)))

  #checking if all values are numeric
  effects<-as.data.frame(dados[,-c(colsTraits, colsPed, colsDate)])
  for(i in 1:length(effects)){
    if(any(effects[,i] < 0)){
      stop("All values should be positive.")
    }
  }

  #Checking if the integers are within limits
  if(any(effects > 2147483647)){
    stop("Effects values are too big.")
  }

  #Checking if variances of traits are within a reasonable interval
  if(!is.null((colsTraits))){
    varTemp<-sapply(dados[, colsTraits], stats::var, na.rm = TRUE)
    if(any(varTemp < 1e-5 | varTemp > 1e5)){
      warning("Variances of the traits are too small ou too big. You should scale the data.")
    }
  }
  dados
}
