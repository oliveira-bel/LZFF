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
  pedData<-pedObj

  #reorganizing columns
  pedData<-data.frame(pedData[,isdd])
  if(length(isdd) == 4){
    names(pedData)<-c("ind", "sire", "dam", "birthDate")

    #Checking if date of birth column is character
    if(methods::is(pedData$birthDate, "character")){
      pedData$birthDate<-as.Date(pedData$birthDate, tryFormats = c("%d/%m/%Y", "%d/%m/%y",
                                                                     "%d/%B/%Y", "%d/%B/%y",
                                                                     "%d/%b/%Y", "%d/%b/%y",
                                                                     "%m/%d/%Y", "%m/%d/%y",
                                                                     "%B/%d/%Y", "%B/%d/%y",
                                                                     "%Y-%m-%d", "%y-%m-%d",
                                                                     "%d-%b-%y", "%d-%m-%y",
                                                                     "%d-%b-%Y", "%d-%m-%Y"))
    }
  }

  names(pedData)<-c("ind", "sire", "dam")
  #checking if there are duplicate data
  pedData<-unique(pedData)

  #checking if there are duplicate individuals with different information
  dup<-duplicated(pedData$ind)
  if(any(dup)){
    stop("Duplicated animals in the pedigree.")
    }

  #Checking if there are animal who appear in both columns: sire and dam
  bad<-match(pedData$sire, pedData$dam, incomparables = NA)
  if (!all(is.na(bad))){
    stop("Error: An animal appears in both columns: sire and dam. Please, check your pedigree.
         Function aborted!")
  }

  #Ordering the Pedigree

  ###################################################################
  #Ordering the Pedigree                                            #
  ###################################################################
  #Ordering individuals with date of birth
  #pedData<-pedData[order(pedData$birthDate, na.last = FALSE),]

  #Ordenando sem datas de nascimento
  i<-with(pedData,
          !(ind%in%sire | ind%in%dam))
  young<-pedData[i, ]

  parents<-young[is.na(young$sire)&is.na(young$dam), ]
  young<-young[!(is.na(young$sire)&is.na(young$dam)), ]

  orderped<-young

  j<-with(pedData,
          ind%in%sire | ind%in%dam)
  elder<-pedData[j, ]

  fund<-data.frame(ind = c(pedData$sire, pedData$dam), sire = NA, dam = NA)
  parents<-rbind(fund, parents)
  rm(fund)
  parents<-unique(parents, fromLast = TRUE)
  parents<-parents[!is.na(parents$ind), ]

  i<-match(elder$ind, parents$ind)
  parents<-parents[-i, ]
  #dup<-duplicated(elder$idf, fromLast = TRUE)
  #elder<-elder[!dup, ]
  #elder<-elder[!is.na(elder$idf), ]

  while(TRUE){
    i<-with(elder,
            !(ind%in%sire | ind%in%dam))
    young<-elder[i, ]

    orderped<-rbind(young, orderped)
    orderped<-unique(orderped, fromLast = TRUE) #Preciso desse unique?

    j<-with(elder,
            ind%in%sire | ind%in%dam)
    if(sum(j) > 0){
      elder<-elder[j, ]
      #if("datas presentes"){
      # melder[order(pedigree$nascto), ]
      #}
    }else{
      orderped<-rbind(parents, orderped)
      dup<-duplicated(orderped$ind, fromLast = TRUE)
      orderped<-orderped[!dup, ]
      break
    }
  }

  #Recoding
  mapaCod<-data.frame(cod = orderped$ind, recod = 1: nrow(orderped))
  ###################################################################


  #Replacing the original codes in the pedigree object
  for(j in 1:3){
    i<-match(pedData[,j], mapaCod$cod)
    pedData[,j]<-mapaCod$recod[i]
  }

  #Checking if the animal recode is smaller than his parents
  #ifelse(pedData$ind <= pedData$sire | pedData$ind <= pedData$dam,{
   #      smaller<-pedData$ind <= pedData$sire | pedData$ind <= pedData$dam
    #     smaller<-!is.na(smaller)
     #    print(pedData[smaller,])
    #     stop("Animal recode is smaller than parent(s) recode. Check the pedigree file.")},
     #    )


  #Replacing the original codes in data object
  for(j in colsPedData.isd){
    i<-match(udata[[j]], mapaCod[,1])
    udata[[j]]<-mapaCod[,2][i]
  }

  #Replacing absent parents in the data object
  udata[,colsPedData.isd]<-replace(udata[,colsPedData.isd], list = is.na(udata[,colsPedData.isd]), values = 0)

  pedDataList<-list(map = mapaCod, ped = pedData, data = udata)
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
         csv = pedData<-utils::read.csv(local, header = h, sep = s,
                                         strip.white = FALSE, fill = TRUE,
                                         na.strings = md),

         xls = pedData<-as.data.frame(readxl::read_excel(local, na = md, col_names = h)),

         xlsx = pedData<-as.data.frame(readxl::read_excel(local, na = md, col_names = h)),

         ods = pedData<-as.data.frame(readODS::read_ods(local, na = md, col_names = h)),

         gsheet = {googlesheets4::gs4_auth()
           pedData<-as.data.frame(googlesheets4::read_sheet(local, na = md),
                                   col_names = h)},

         txt = pedData<-utils::read.table(local, header = h, sep = s,
                                           strip.white = FALSE, fill = TRUE,
                                           na.strings = md),

         print("I did not detect the type of the file.")
  )

  #reorganizing columns
  pedData<-data.frame(pedData[,isdd])
  names(pedData)<-c("ind", "sire", "dam", "birthDate")

  #Checking if date of birth column is character
  if(methods::is(pedData$birthDate, "character")){
    pedData$birthDate<-as.Date(pedData$birthDate, tryFormats = c("%d/%m/%Y", "%d/%m/%y",
                                                                   "%d/%B/%Y", "%d/%B/%y",
                                                                   "%d/%b/%Y", "%d/%b/%y",
                                                                   "%m/%d/%Y", "%m/%d/%y",
                                                                   "%B/%d/%Y", "%B/%d/%y",
                                                                   "%Y-%m-%d", "%y-%m-%d"))
  }

  #checking if there are duplicate data
  pedData<-unique(pedData)

  #Checking if there are animal who appear in both columns: sire and dam
  bad<-match(pedData[,2], pedData[,3], incomparables = NA)
  if (!all(is.na(bad))){
    stop("Error: An animal appears in both columns: sire and dam. Please, check your pedigree.
         Function aborted!")
  }


  ###################################################################
  #Ordering the Pedigree                                            #
  ###################################################################
  #Ordering individuals with date of birth
  #pedData<-pedData[order(pedData$birthDate, na.last = FALSE),]

  #Ordenando sem datas de nascimento
  i<-with(pedData,
          !(ind%in%sire | ind%in%dam))
  young<-pedData[i, ]

  parents<-young[is.na(young$sire)&is.na(young$dam), ]
  young<-young[!(is.na(young$sire)&is.na(young$dam)), ]

  orderped<-young

  j<-with(pedData,
          ind%in%sire | ind%in%dam)
  elder<-pedData[j, ]

  fund<-data.frame(ind = c(pedData$sire, pedData$dam), sire = NA, dam = NA)
  parents<-rbind(fund, parents)
  rm(fund)
  parents<-unique(parents, fromLast = TRUE)
  parents<-parents[!is.na(parents$ind), ]

  i<-match(elder$ind, parents$ind)
  parents<-parents[-i, ]
  #dup<-duplicated(elder$idf, fromLast = TRUE)
  #elder<-elder[!dup, ]
  #elder<-elder[!is.na(elder$idf), ]

  while(TRUE){
    i<-with(elder,
            !(ind%in%sire | ind%in%dam))
    young<-elder[i, ]

    orderped<-rbind(young, orderped)
    orderped<-unique(orderped, fromLast = TRUE) #Preciso desse unique?

    j<-with(elder,
            ind%in%sire | ind%in%dam)
    if(sum(j) > 0){
      elder<-elder[j, ]
      #if("datas presentes"){
      # melder[order(pedigree$nascto), ]
      #}
    }else{
      orderped<-rbind(parents, orderped)
      dup<-duplicated(orderped$ind, fromLast = TRUE)
      orderped<-orderped[!dup, ]
      break
    }
  }

  #Recoding
  mapaCod<-data.frame(cod = orderped$ind, recod = 1: nrow(orderped))
  ###################################################################


  #Replacing the original codes in the pedigree object
  for(j in 1:3){
    i<-match(pedData[,j], mapaCod$cod)
    pedData[,j]<-mapaCod$recod[i]
  }

  #Replacing the original codes in the pedigree object
  for(j in 1:3){
    i<-match(pedData[,j], mapaCod[,1])
    pedData[,j]<-mapaCod[,2][i]
  }

  #Replacing the original codes in data object
  for(j in colsPedData.isd){
    i<-match(udata[,j], mapaCod[,1])
    udata[,j]<-mapaCod[,2][i]
  }

  #Replacing absent parents in the data object
  udata[,colsPedData.isd]<-replace(udata[,colsPedData.isd], list = is.na(udata[,colsPedData.isd]), values = 0)

  pedDataList<-list(map = mapaCod, ped = pedData, data = udata)
  pedDataList
}
