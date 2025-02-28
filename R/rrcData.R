#' Read, recode and check
#'
#' @param local data file path
#' @param s field/column separator
#' @param d decimal point used in data file
#' @param h logical value indicating presence of header in data file
#' @param md missing data indicator
#' @param colsPed identification of columns related to pedigree data
#' @param colsTraits identification of columns related to traits
#'
#' @description
#' Simple function for read, recode and perform some checks in a data file.
#'
#' @return a data frame with data file columns read and recoded as needed. Pedigree data are not recoded by this function
#' @export

rrcData<-function(local, s, d, h = TRUE, md = c(""," ","NA"), colsPed = NULL, colsTraits = NULL){
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

        xls = dados<-as.data.frame(readxl::read_excel(local, na = md, col_names = h)),

        xlsx = dados<-as.data.frame(readxl::read_excel(local, na = md, col_names = h)),

        ods = dados<-as.data.frame(readODS::read_ods(local, na = md, col_names = h)),

        gsheet = {googlesheets4::gs4_auth()
                  dados<-as.data.frame(googlesheets4::read_sheet(local, na = md),
                                       col_names = h)},

        txt = dados<-utils::read.table(local, header = h, sep = s, dec = d,
                                       strip.white = FALSE, na.strings = md),

        print("I did not detect the type of the file.")
        )

  #Checking if the file is ASCII
  dadosTemp<-NULL
  dadosTemp<-sapply(dados, paste0, collapse = " ")
  stopifnot(all(grepl("^[ -~]+$", dadosTemp)))

  ##########
  #Recoding#
  ##########
  #checking if all values are numeric
  isnum<-sapply(dados,is.numeric)
  mapList<-list()
  for(i in 1:length(isnum[-colsTraits])){
    if(any(dados[,i] < 0)){
      stop("All values should be positive.")
    }
  }

  for(i in 1:length(isnum)){
    if(!isnum[i] && all(i != colsPed)){
      tempData<-unique(dados[,i])
      codes<-1:length(tempData)
      mapa<-data.frame("Original.Codes" = tempData, "Recodes" = codes)
      mapList<-append(mapList, list(mapa))
      index<-match(dados[,i],mapList[[length(mapList)]]$Original.Codes)
      dados[,i]<-mapList[[length(mapList)]]$Recodes[index]
    }
  }

  #Checking if the integers are within limits
  if(any(dados[, -c(colsPed, colsTraits)] > 2147483647)){
    stop("Effects values are too big.")
  }

  #Checking if variances of trais are within a reasonable interval
  if(!is.null((colsTraits))){
    varTemp<-sapply(dados[, colsTraits], stats::var)
    if(any(varTemp < 1e-5 | varTemp > 1e5)){
      warning("Variances of the traits are too small ou too big. You should scale the data.")
    }
  }
  dados
}
