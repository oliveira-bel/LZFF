#' Read, recode and check Data and Pedigree
#'
#' @param dataObj data frame with data after a consistency analysis
#' @param colsPed identification of columns related to pedigree data in data object/file
#' @param colsTraits identification of columns related to traits
#' @param colsDates identification of columns related to Dates
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
rrcDataPed<-function(dataObj = NULL, pedObj = NULL, localData = NULL,
                     localPed = NULL, colsTraits = NULL, colsDates = NULL,
                     colsPedData.isd = 1:3, ped.isd = 1:3, sData = " ",
                     dData = ".", hData = FALSE, sPed = " ", hPed = FALSE,
                     md = c(""," ","NA"), rm.colsData = NULL, rm.colsPed = NULL){

  udata<-rrcData(datObj = dataObj, colsPdg = colsPedData.isd, colsTrts = colsTraits,
                 colsDts = colsDates, local = localData, s = sData, d = dData,
                 h = hData, missData = md, rm.cols = rm.colsData)

  fDataPed<-rrcPed(pedigreeObj = pedObj, isd = ped.isd, udata,
                   colsPdgDat.isd = colsPedData.isd, local = localPed, s = sPed,
                   h = hPed, missData = md, rm.cols = rm.colsPed)

  fDataPed
}

#' Title
#'
#' @param dObj data frame with data after a consistency analysis
#' @param pObj object with pedigree data
#' @param cTraits identification of columns related to traits
#' @param cPedDat.isd identification of columns with pedigree information in the unformatted data object in the order individual, sire, dam
#' @param pdg.isd vector of columns number for individual, sire and dam in pedigree object/file
#' @param dataFile data file path
#' @param pedFile data file path
#' @param sDat field/column separator in data file
#' @param dDat decimal point used in data file
#' @param hDat logical value indicating presence of header in data object/file
#' @param sPdg field/column separator in pedigree file
#' @param hPdg logical value indicating presence of header in pedigree file
#' @param missingData missing data indicator
#' @param listDataPed list containing data, pedigree and the code map
#' @param dof data output file's name
#' @param omdat missing data value to be written in the output file
#' @param trts vector indicating traits columns to be printed
#' @param width vector specifying the widths of columns in the formatted file
#' @param endOfLine end of line indicator. Unix and Linux uses "\\n", while Windows uses "\\r\\n"
#' @param pof output file's name for pedigree file
#' @param mparents code for missing parents
#' @param sep field/column separator
#' @param printMap logical value indicating if the code map should be printed
#' @param mof map output file's name
#' @param rm.columnsPed identification of the columns to be removed in the pedigree
#'
#' @returns
#' @export
#'
#' @examples
wombat<-function(dObj = NULL, pObj = NULL, cTraits = NULL, cPedDat.isd = NULL,
                 cDates = NULL, pdg.isd = c(1, 2, 3), dataFile = NULL,
                 pedFile = NULL, sDat = " ", dDat = ".", hDat = FALSE,
                 rm.columnsData = NULL, sPdg = " ", hPdg = FALSE, rm.columnsPed = NULL,
                 missingData = c(""," ","NA"), dof = "formatB_data", omdat = "-99999", trts = NULL,
                 width = NULL, endOfLine = "\n", pof = "pedigree.txt", mparents = 0,
                 sep = " ", printMap = FALSE, mof = "map.txt"){

  listDataPed<-rrcDataPed(dataObj = dObj, pedObj = pObj, colsTraits = cTraits, colsDate = cDates,
                          colsPedData.isd = cPedDat.isd, ped.isd = pdg.isd,
                          localData = dataFile, localPed = pedFile, sData = sDat,
                          dData = dDat, hData = hDat, sPed = sPdg, hPed = hPdg,
                          md = missingData, rm.colsData = rm.columnsData,
                          rm.colsPed = rm.columnsPed)

    formatB(dataPed = listDataPed, of = dof, omd = omdat, traits = trts,
            widths = width, EoL = endOfLine)

    formatPed(pedObj = listDataPed, of = pof, mp = mparents, s = sep, EoL = endOfLine,
            map = printMap, mapof = mof, rm.cols = rm.columnsPed)

    listDataPed
}
