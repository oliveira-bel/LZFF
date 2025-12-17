#' Format Pedigree Files
#'
#' @param ped object of type list with 3 elements: map, ped and data
#' @param of output file's name for pedigree file
#' @param mp code for missing parents
#' @param s field/column separator
#' @param EoL end of line symbol
#' @param map logical value indicating if the code map should be printed
#' @param mapof map output file's name
#' @param rm.col identification of the columns to be removed
#'
#' @returns
#' @export
#'
#' @examples
formatPed<-function(pedObj, of = "pedigree.txt", mp = 0, s = " ", EoL = "\n",
                    map = FALSE, mapof = "map.txt"){

  #Replacing absent parents in the pedigree object
  pedObj$ped<-replace(pedObj$ped, list = is.na(pedObj$ped), values = mp)

  #Checking if the pedigree data is ASCII
  dadosPedTemp<-NULL
  dadosPedTemp<-sapply(pedObj$ped, paste0, collapse = " ")
  stopifnot(all(grepl("^[ -~]+$", dadosPedTemp)))

  if(map){
    utils::write.table(pedObj$ped, of, sep = s, eol = EoL,
                       row.names = FALSE, col.names = FALSE, quote = FALSE)
    utils::write.table(pedObj$map, mapof, sep = s, eol =EoL,
                       row.names = FALSE, col.names = TRUE, quote = FALSE)
  }else{
    utils::write.table(pedObj$ped, of, sep = s, eol = EoL,
                       row.names = FALSE, col.names = FALSE, quote = FALSE)
  }
}
