#' Read, recode and check
#'
#' @param datObj data frame with data after a consistency analysis
#' @param local data file path
#' @param s field/column separator
#' @param d decimal point used in data file
#' @param h logical value indicating presence of header in data file
#' @param missData missing data indicator
#' @param colsPdg identification of columns related to pedigree data
#' @param colsTrts identification of columns related to traits
#' @param colsDts identification of columns related to Dates
#'
#' @description
#' Simple function for read, recode and perform some checks in a data file.
#'
#' @return a data frame with data file columns read and recoded as needed. Pedigree data are not recoded by this function
#' @export

rrcData<-function(datObj = NULL, colsPdg = NULL, colsTrts = NULL, colsDts = NULL,
                  local = NULL, s = " ", d = ".", h = FALSE, missData = c(""," ","NA"),
                  rm.cols = NULL){

  #Validation
  argTest<-as.character(sum(!is.null(local), !is.null(datObj)))
  switch(argTest,
         '0' = stop("You must provide EXACTLY ONE of the following arguments:\n",
                  "-'local': path to the file (character)\n",
                  "-'datObj': R object (data.frame)\n",
                  "Both cannot be NULL at the same time", call. = FALSE),

         '2' = stop("You must provide ONLY ONE of the following arguments:\n",
                  "-'local': path to the file (character)\n",
                  "-'datObj': R object (data.frame)\n",
                  "Both cannot be provided simultaneously", call. = FALSE))

  if(is.null(local) == FALSE){
    #Data reading
    tipo<-stringr::str_extract(local,"(\\w+)$")
    if(tipo == basename(local)){
      tipo<-"txt"
    }
    if(stringr::str_detect(local,"https://docs.google.com/spreadsheets")){
      tipo<-"gsheet"
    }
    switch(tipo,
           csv = dados<-utils::read.csv(local, header = h, sep = s, dec = d,
                                        strip.white = FALSE, na.strings = missData, fill = TRUE),

           xls = dados<-as.data.frame(readxl::read_excel(local, na = missData, col_names = h)),

           xlsx = dados<-as.data.frame(readxl::read_excel(local, na = missData, col_names = h)),

           ods = dados<-as.data.frame(readODS::read_ods(local, na = missData, col_names = h)),

           gsheet = {googlesheets4::gs4_auth()
             dados<-as.data.frame(googlesheets4::read_sheet(local, na = missData),
                                  col_names = h)},

           txt = dados<-utils::read.table(local, header = h, sep = s, dec = d,
                                          strip.white = FALSE, na.strings = missData, fill = TRUE),

           print("I did not detect the type of the file.")
    )
  }else{
    dados<-as.data.frame(datObj)
    print(dados)
  }

  if(!is.null(rm.cols)){
   dados<-dados[,-rm.cols]
  }

  ##########
  #Recoding#
  ##########
  for(i in 1:length(dados)){
    if(all(i != c(colsPdg, colsTrts, colsDts))){
      tempData<-unique(dados[[i]])
      codes<-1:length(tempData)
      mapa<-data.frame(tempData, codes)
      names(mapa)<-c("Original.Codes", "Recode")
      index<-match(dados[[i]],mapa$Original.Codes)
      dados[,i]<-mapa$Recode[index]
    }
  }

  #Checking if the characters are ASCII
  dadosTemp<-NULL
  dadosTemp<-sapply(dados, paste0, collapse = " ")
  stopifnot(all(grepl("^[ -~]+$", dadosTemp)))

  #checking if all values are positive
  effects<-as.data.frame(dados[,-c(colsTrts, colsPdg, colsDts)])
  for(i in 1:length(effects)){
    if(any(effects[,i] < 0)){
      stop("All values should be positive.")
    }
  }

  #Checking if the integers are within limits
  if(any(effects > 2147483647)){
    stop("Effects values are too big.")
  }

  #Checking if variances of traits are within a reasonable interval
  if(!is.null((colsTrts))){
    varTemp<-sapply(as.data.frame(dados[, colsTrts]), function(x){stats::var(x, na.rm = TRUE)})
    if(any(varTemp < 1e-5 | varTemp > 1e5)){
      warning("Variances of the traits are too small ou too big. You should scale the data.")
    }
  }
  dados
}
