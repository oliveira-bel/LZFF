#' Read, Recod and check Pedigree Data
#'
#' @param local data file path
#' @param s field/column separator
#' @param h logical value indicating presence of header in data file
#' @param isdd vector of columns number for individual, sire, dam and date of birth
#' @param md missing data indicator
#' @param colsPedData identification of columns with pedigree information in data object
#' @param udata unformatted data object
#'
#' @description
#' Simple function for read, recode, format and perform checks in a pedigree file.
#'
#' @returns a list with three components: 1. a data frame with code map; 2. a data
#' frame with the formatted pedigree data; 3. a data frame with pedigree columns
#' in data recoded.
#' @export
#'
rrcPed<-function(local, s, h = FALSE, isdd = c(1, 2, 3, 4), md = c(""," ","NA"),
                 colsPedData = c(1, 2, 3), udata){
  #Data reading
  tipo<-stringr::str_extract(local,"(\\w+)$")
  if(tipo == basename(local)){
    tipo<-"txt"
  }
  if(stringr::str_detect(local,"https://docs.google.com/spreadsheets")){
    tipo<-"gsheet"
  }
  switch(tipo,
         csv = dadosPed<-utils::read.csv(local, header = h, sep = s,
                                      strip.white = FALSE, fill = TRUE,
                                      na.strings = md),

         xls = dadosPed<-as.data.frame(readxl::read_excel(local, na = md, col_names = h)),

         xlsx = dadosPed<-as.data.frame(readxl::read_excel(local, na = md, col_names = h)),

         ods = dadosPed<-as.data.frame(readODS::read_ods(local, na = md, col_names = h)),

         gsheet = {googlesheets4::gs4_auth()
           dadosPed<-as.data.frame(googlesheets4::read_sheet(local, na = md),
                                col_names = h)},

         txt = dadosPed<-utils::read.table(local, header = h, sep = s,
                                        strip.white = FALSE, fill = TRUE,
                                        na.strings = md),

         print("I did not detect the type of the file.")
  )

  #reorganizing columns
  dadosPed<-data.frame(dadosPed[,isdd])

  #Checking if date of birth column is character
  if(methods::is(dadosPed[,4], "character")){
    dadosPed[,4]<-as.Date(dadosPed[,4], tryFormats = c("%d/%m/%Y", "%d/%m/%y",
                                                       "%d/%B/%Y", "%d/%B/%y",
                                                       "%d/%b/%Y", "%d/%b/%y",
                                                       "%m/%d/%Y", "%m/%d/%y",
                                                       "%B/%d/%Y", "%B/%d/%y",
                                                       "%Y-%m-%d", "%y-%m-%d"))
  }

  #checking if there are duplicate data
  dadosPed<-unique(dadosPed)

  #Checking if there are animal who appear in both columns: sire and dam
  bad<-match(dadosPed[,2], dadosPed[,3], incomparables = NA)
  if (!all(is.na(bad))){
    stop("Error: An animal appears in both columns: sire and dam. Please, check your pedigree.
         Function aborted!")
  }

  #Recoding the Pedigree
  dfPai<-data.frame(id = dadosPed[,2], datanas = as.Date(NA))
  dfMae<-data.frame(id = dadosPed[,3], datanas = as.Date(NA))
  dfId<-data.frame(id = dadosPed[,1], datanas = dadosPed[,4])

  #Ordering individuals with date of birth
  dfId<-dfId[order(dfId$datanas, na.last = FALSE), ]

  mapaCod<-rbind(dfPai, dfMae, dfId) #order of the argument is important

  #Removing duplications
  mapaCod<-mapaCod[!duplicated(mapaCod[,1], fromLast = TRUE), ]
  mapaCod<-mapaCod[!is.na(mapaCod[,1]),]

  #Finally recoding
  mapaCod<-data.frame(cod = mapaCod$id, recod = 1: nrow(mapaCod))

  #Removing date of birth column
  dadosPed<-dadosPed[,-4]

  #Replacing the original codes in the pedigree object
  for(j in 1:3){
    i<-match(dadosPed[,j], mapaCod[,1])
    dadosPed[,j]<-mapaCod[,2][i]
  }

  #Replacing absent parents in the pedigree object
  dadosPed<-replace(dadosPed, list = is.na(dadosPed), values = 0)

  #Checking if the file is ASCII
  dadosPedTemp<-NULL
  dadosPedTemp<-sapply(dadosPed, paste0, collapse = " ")
  stopifnot(all(grepl("^[ -~]+$", dadosPedTemp)))

  #Replacing the original codes in data object
  for(j in colsPedData){
    i<-match(udata[,j], mapaCod[,1])
    udata[,j]<-mapaCod[,2][i]
  }

  #Replacing absent parents in the data object
  udata[,colsPedData]<-replace(udata[,colsPedData], list = is.na(udata[,colsPedData]), values = 0)

  pedDataList<-list(map = mapaCod, ped = dadosPed, data = udata)
  pedDataList
}
