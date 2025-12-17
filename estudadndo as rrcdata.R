

dadosPed<-data.frame( ind = c("A13", "A2d", "A3se", "Aee4"), sire = c("Sx1", "Sfff1", "Seee2", "S2rr"), dam = c("Drrt1", "Dcdc2", "Dvf3", "Dgh"),data = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01")))
dadosPed



#reorganizing columns
names(dadosPed)<-c("ind", "sire", "dam", "birthDate")


#Creating Founders    
pedVec<-c(dadosPed$sire, dadosPed$dam, dadosPed$ind)

pedVec<-unique(pedVec, fromLast = TRUE)

pedVec<-pedVec[!is.na(pedVec)]

founders<-data.frame(pedVec[1:(length(pedVec) - length(dadosPed$ind))], NA, NA, as.Date(NA))

names(founders)<-c("ind", "sire", "dam", "birthDate")


#Recreating the Pedigree

dadosPed<-rbind(founders, dadosPed)

dadosPed<-dadosPed[order(dadosPed$birthDate, na.last = FALSE),]

#Recoding
mapaCod<-data.frame(cod = dadosPed$ind, recod = 1: nrow(dadosPed))

#Removing date of birth column
dadosPed$birthDate<-NULL

#Replacing the original codes in the pedigree object
for(j in 1:3){
  i<-match(dadosPed[,j], mapaCod[,1])
  dadosPed[,j]<-mapaCod[,2][i]
}

#Replacing absent parents in the pedigree object
dadosPed<-replace(dadosPed, list = is.na(dadosPed), values = 0)

