#' Read, recode and check Data and Pedigree
#'
#' @param dataObj data frame with data after a consistency analysis
#' @param colsPed identification of columns related to pedigree data in data object/file
#' @param colsTraits identification of columns related to traits
#' @param localData data file path
#' @param sData field/column separator in data file
#' @param dData decimal point used in data file
#' @param hData logical value indicating presence of header in data object/file
#' @param md missing data indicator
#' @param pedObj object with pedigree data
#' @param colsPedData.isd identification of columns with pedigree information in the unformatted data object in the order individual, sire, dam
#' @param localPed data file path
#' @param sPed field/column separator in pedigree file
#' @param hPed logical value indicating presence of header in pedigree file
#' @param ped.isd vector of columns number for individual, sire and dam in pedigree object/file
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
