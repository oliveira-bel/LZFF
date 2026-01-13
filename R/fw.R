#' Title Read, recod, check and format pedigree and data.
#'
#' @description
#' This wrapper function reads, recodes, and performs checks on the data and pedigree files. In the end, it creates formatted data and pedigree files according to the requirements of the Wombat software for genetic evaluation.
#'
#' @param dObj data frame with data after a consistency analysis
#' @param pObj object with pedigree data
#' @param cTraits identification of columns related to traits
#' @param cPedDat.isd identification of columns with pedigree information in the unformatted data object in the order individual, sire, dam
#' @param cDates columns with dates
#' @param pdg.isd vector of columns number for individual, sire and dam in pedigree object/file
#' @param dataFile data file path
#' @param pedFile data file path
#' @param sDat field/column separator in data file
#' @param dDat decimal point used in data file
#' @param hDat logical value indicating presence of header in data object/file
#' @param sPdg field/column separator in pedigree file
#' @param hPdg logical value indicating presence of header in pedigree file
#' @param missingData missing data indicator
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
#'
#' @returns a list with the following components: recode map, recoded and checked data ane recoded and checked pedigree; formatted data and pedigree files.
#' @export
#'
#' @examples
#'
fw<-function(dObj = NULL, pObj = NULL, cTraits = NULL, cPedDat.isd = NULL,
             cDates = NULL, pdg.isd = c(1, 2, 3), dataFile = NULL,
             pedFile = NULL, sDat = " ", dDat = ".", hDat = FALSE,
             sPdg = " ", hPdg = FALSE, missingData = c(""," ","NA"),
             dof = "formatB_data", omdat = "-99999", trts = NULL,
             width = NULL, endOfLine = "\n", pof = "pedigree.txt", mparents = 0,
             sep = " ", printMap = FALSE, mof = "map.txt"){

  listDataPed<-rrcDataPed(dataObj = dObj, pedObj = pObj, colsTraits = cTraits, colsDate = cDates,
                          colsPedData.isd = cPedDat.isd, ped.isd = pdg.isd,
                          localData = dataFile, localPed = pedFile, sData = sDat,
                          dData = dDat, hData = hDat, sPed = sPdg, hPed = hPdg,
                          md = missingData)

  formatB(dataList = listDataPed, of = dof, omd = omdat, traits = trts,
          widths = width, EoL = endOfLine)

  formatPed(dataList = listDataPed, of = pof, mp = mparents, s = sep, EoL = endOfLine,
            map = printMap, mapof = mof)

  listDataPed
}
