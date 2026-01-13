#' Format Pedigree Files
#'
#' @description
#' It creates a formatted pedigree file based on the software Wombat's requirements for genetic evaluations.
#'
#' @param dataList list containing recoded pedigree and data.
#' @param of output file's name for pedigree file
#' @param mp code for missing parents
#' @param s field/column separator
#' @param EoL end of line symbol
#' @param map logical value indicating if the code map should be printed
#' @param mapof map output file's name
#'
#' @returns a formatted pedigree file.
#' @export
#'
#' @examples
formatPed<-function(dataList, of = "pedigree.txt", mp = 0, s = " ", EoL = "\n",
                    map = FALSE, mapof = "map.txt"){

  #Replacing absent parents in the pedigree object
  dataList$ped<-replace(dataList$ped, list = is.na(dataList$ped), values = mp)

  #Checking if the pedigree data is ASCII
  dadosPedTemp<-NULL
  dadosPedTemp<-sapply(dataList$ped, paste0, collapse = " ")
  stopifnot(all(grepl("^[ -~]+$", dadosPedTemp)))

  if(map){
    utils::write.table(dataList$ped, of, sep = s, eol = EoL,
                       row.names = FALSE, col.names = FALSE, quote = FALSE)
    utils::write.table(dataList$map, mapof, sep = s, eol =EoL,
                       row.names = FALSE, col.names = TRUE, quote = FALSE)
  }else{
    utils::write.table(dataList$ped, of, sep = s, eol = EoL,
                       row.names = FALSE, col.names = FALSE, quote = FALSE)
  }
}
