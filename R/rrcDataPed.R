#' Read, recode and check Data and Pedigree
#'
#' @param dataObj
#' @param colsPed
#' @param colsTraits
#' @param localData
#' @param sData
#' @param dData
#' @param hData
#' @param md
#' @param pedObj
#' @param colsPedData.isd
#' @param localPed
#' @param sPed
#' @param hPed
#' @param ped.isd
#'
#' @returns
#' @export
#'
#' @examples
rrcDataPed<-function(dataObj = NULL, colsPed = NULL, colsTraits = NULL, localData = NULL,
                     sData = " ", dData = ".", hData = FALSE, md = c(""," ","NA"),
                     pedObj = NULL,ped.isd = c(1, 2, 3), colsPedDat.isd = NULL,
                     localPed = NULL, sPed = " ", hPed = FALSE){
  udata<-rrcData(datObj = dataObj, colsPdg = colsPed, colsTrts = colsTraits,
                 local = localData, s = sData, d = dData, h = hData, missData = md)

  fDataPed<-rrcPed(pedigreeObj = pedObj, isd = ped.isd, udata,
                   colsPdgDat.isd = colsPedData.isd, local = localPed, s = sPed,
                   h = hPed, missData = md)

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
