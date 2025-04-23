rrcPed<-function(local, s, h = FALSE, isdd = c(1, 2, 3, 4), md = c(""," ","NA"), udata){
  #Data reading
  tipo<-stringr::str_extract(local,"(\\w+)$")
  if(tipo == basename(local)){
    tipo<-"txt"
  }
  if(stringr::str_detect(local,"https://docs.google.com/spreadsheets")){
    tipo<-"gsheet"
  }
  switch(tipo,
         csv = dadosPed<-utils::read.csv(local, header = h, sep = s,
                                      strip.white = FALSE, na.strings = md,
                                      fill = TRUE),

         xls = dadosPed<-as.data.frame(readxl::read_excel(local, na = md, col_names = h)),

         xlsx = dadosPed<-as.data.frame(readxl::read_excel(local, na = md, col_names = h)),

         ods = dadosPed<-as.data.frame(readODS::read_ods(local, na = md, col_names = h)),

         gsheet = {googlesheets4::gs4_auth()
           dadosPed<-as.data.frame(googlesheets4::read_sheet(local, na = md),
                                col_names = h)},

         txt = dadosPed<-utils::read.table(local, header = h, sep = s,
                                        strip.white = FALSE, na.strings = md,
                                        fill = TRUE),

         print("I did not detect the type of the file.")
  )

  #Checking if the file is ASCII
  dadosPedTemp<-NULL
  dadosPedTemp<-sapply(dadosPed, paste0, collapse = " ")
  stopifnot(all(grepl("^[ -~]+$", dadosPedTemp)))

  #reorganizing columns
  dadosPed<-data.frame(dadosPed[,isdd])

  #checking if there are duplicate data
  dadosPed<-unique(dadosPed)

}
