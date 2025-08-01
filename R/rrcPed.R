#' Read, Recod and check Pedigree Data
#'
#' @pedObj object with pedigree data
#' @param local data file path
#' @param s field/column separator
#' @param h logical value indicating presence of header in data file
#' @param isdd vector of columns number for individual, sire, dam and date of birth
#' @param md missing data indicator
#' @param udata unformatted data object
#' @param colsPedData.isd identification of columns with pedigree information in the unformatted data object in the order individual, sire, dam
#'
#' @description
#' Simple function for read, recode, format and perform checks in a pedigree file.
#'
#' @returns a list with three components: 1. a data frame with code map; 2. a data
#' frame with the formatted pedigree data; 3. a data frame with pedigree columns
#' in data recoded.
#' @export
#'
rrcPed<-function(pedObj, isdd = c(1, 2, 3, 4), udata, colsPedData.isd = c(1, 2, 3)){
  dadosPed<-pedObj

  #reorganizing columns
  dadosPed<-data.frame(dadosPed[,isdd])
  names(dadosPed)<-c("ind", "sire", "dam", "birthDate")

  #Checking if date of birth column is character
  if(methods::is(dadosPed$birthDate, "character")){
    dadosPed$birthDate<-as.Date(dadosPed$birthDate, tryFormats = c("%d/%m/%Y", "%d/%m/%y",
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

  #Creating Founders
  pedVec<-c(dadosPed$sire, dadosPed$dam, dadosPed$ind)
  pedVec<-unique(pedVec, fromLast = TRUE)
  pedVec<-pedVec[!is.na(pedVec)]
  founders<-data.frame(pedVec[1:(length(pedVec) - length(dadosPed$ind))], NA, NA, as.Date(NA))
  names(founders)<-c("ind", "sire", "dam", "birthDate")

  #Recreating the Pedigree
  dadosPed<-rbind(founders, dadosPed)

  ###################################################################
  #Recoding the Pedigree                                            #
  ###################################################################
  #Ordering individuals with date of birth
  dadosPed<-dadosPed[order(dadosPed$birthDate, na.last = FALSE),]

  #Recoding
  mapaCod<-data.frame(cod = dadosPed$ind, recod = 1: nrow(dadosPed))
  ###################################################################

  #Replacing the original codes in the pedigree object
  for(j in 1:3){
    i<-match(dadosPed[,j], mapaCod[,1])
    dadosPed[,j]<-mapaCod[,2][i]
  }

  #Replacing the original codes in data object
  for(j in colsPedData.isd){
    i<-match(udata[,j], mapaCod[,1])
    udata[,j]<-mapaCod[,2][i]
  }

  #Replacing absent parents in the data object
  udata[,colsPedData.isd]<-replace(udata[,colsPedData.isd], list = is.na(udata[,colsPedData.isd]), values = 0)

  pedDataList<-list(map = mapaCod, ped = dadosPed, data = udata)
  pedDataList
}

#############################################################################################

rrcPedF<-function(local, s, h = FALSE, isdd = c(1, 2, 3, 4), md = c(""," ","NA"),
                 udata, colsPedData.isd = c(1, 2, 3)){
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
  names(dadosPed)<-c("ind", "sire", "dam", "birthDate")

  #Checking if date of birth column is character
  if(methods::is(dadosPed$birthDate, "character")){
    dadosPed$birthDate<-as.Date(dadosPed$birthDate, tryFormats = c("%d/%m/%Y", "%d/%m/%y",
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


  #Creating Founders
  pedVec<-c(dadosPed$sire, dadosPed$dam, dadosPed$ind)
  pedVec<-unique(pedVec, fromLast = TRUE)
  pedVec<-pedVec[!is.na(pedVec)]
  founders<-data.frame(pedVec[1:(length(pedVec) - length(dadosPed$ind))], NA, NA, as.Date(NA))
  names(founders)<-c("ind", "sire", "dam", "birthDate")

  #Recreating the Pedigree
  dadosPed<-rbind(founders, dadosPed)

  #####################################################################
  #Recoding the Pedigree                                              #
  #####################################################################
  #Ordering individuals with date of birth
  dadosPed<-dadosPed[order(dadosPed$birthDate, na.last = FALSE),]

  #Recoding
  mapaCod<-data.frame(cod = dadosPed$ind, recod = 1: nrow(dadosPed))
  #####################################################################

  #Replacing the original codes in the pedigree object
  for(j in 1:3){
    i<-match(dadosPed[,j], mapaCod[,1])
    dadosPed[,j]<-mapaCod[,2][i]
  }

  #Replacing the original codes in data object
  for(j in colsPedData.isd){
    i<-match(udata[,j], mapaCod[,1])
    udata[,j]<-mapaCod[,2][i]
  }

  #Replacing absent parents in the data object
  udata[,colsPedData.isd]<-replace(udata[,colsPedData.isd], list = is.na(udata[,colsPedData.isd]), values = 0)

  pedDataList<-list(map = mapaCod, ped = dadosPed, data = udata)
  pedDataList
}
