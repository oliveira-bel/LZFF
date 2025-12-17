wombat.par<-function(dataObj = NULL, colsTraits = NULL, localData = NULL,
                     sData = " ", dData = ".", hData = FALSE, md = c(""," ","NA"),
                     pedObjl = NULL,ped.isd = c(1, 2, 3), colsPedData.isd = NULL,
                     localPed = NULL, sPed = " ", hPed = FALSE, ofb="formatB_data",
                     omdb = "-99999",traitsb = NULL,widthsb = NULL, EoLb = "\n",
                     ofp = "pedigree.txt",mpp = 0, sp = " ",mapp = FALSE,
                     mapofp = "map.txt",rm.colp = 4
){

  udata <- rrcData(datObj = dataObj, colsPdg = colsPedData.isd,
                   colsTrts = colsTraits, local = localData,
                   s = sData, d = dData, h = hData, missData = md)

  dtt <- rrcPed(pedigreeObj = pedObjl, isd = ped.isd, udata,
                colsPdgDat.isd = colsPedData.isd,
                local = localPed, s = sPed, h = hPed, missData = md)

  formatB(dataPed = dtt, of = ofb, omd = omdb,
          traits = traitsb, widths = widthsb, EoL = EoLb)

  formatPed(pedObj = dtt, of = ofp, mp = mpp,
            s = sp, map = mapp, mapof = mapofp, rm.col = rm.colp)
}

