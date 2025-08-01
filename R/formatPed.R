

formatPed<-function(ped, of = "pedigree.txt", mp = 0, s = " ", EoL = "\n",
                    d = ".", map = FALSE, mapof = "map.txt", rm.col = 4){

  #Replacing absent parents in the pedigree object
  ped<-replace(ped, list = is.na(ped), values = mp)

  #Checking if the pedigree data is ASCII
  dadosPedTemp<-NULL
  dadosPedTemp<-sapply(ped, paste0, collapse = " ")
  stopifnot(all(grepl("^[ -~]+$", dadosPedTemp)))

  #Removing columns
  dadosPed<-dadosPed[,-rm.col]

  if(map){
    utils::write.table(ped[["ped"]], of, sep = s, eol = EoL, dec = d,
                       row.names = FALSE, col.names = FALSE, quote = FALSE)
    utils::write.table(ped[["map"]], mapof, sep = s, eol =EoL, dec = d,
                       row.names = FALSE, col.names = TRUE, quote = FALSE)
  }else{
    utils::write.table(ped[["ped"]], of, sep = s, eol = EoL, dec = d,
                       row.names = FALSE, col.names = FALSE, quote = FALSE)
  }
}
