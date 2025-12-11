rrcDataPed<-function(dataObj = NULL, colsPed = NULL, colsTraits = NULL, localData = NULL,
                     sData = " ", dData = ".", hData = FALSE, md = c(""," ","NA"),
                     pedObj = NULL,isd = c(1, 2, 3), colsPedData.isd = NULL,
                     localPed = NULL, sPed = " ", hPed = FALSE){
  udata<-rrcData(dataObj, colsPed, colsTraits, local = localData,
                 s = sData, d = dData, h = hData, md)

  fDataPed<-rrcPed(pedObj, isd, udata, colsPedData.isd,
                   local = localPed, s = sPed, h = hPed, md)

  fDataPed
}

#Extractor functions
phenotypicData<-function(list){
  list$data
}

pedigreeData<-function(list){
  list$ped
}

codeMap<-function(list){
  list$map
}
