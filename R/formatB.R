formatB<-function(local,s,d,h=TRUE,md=c(""," ","NA"),
                  of="formatA_data",omd = -99999,
                  effectsInt = NULL, traits = NULL, EoL="\n"){

  if(stringr::str_detect(of,"#")){
    stop("File name cannot contain a #. Choose a name without a #")
  }

  tipo<-stringr::str_extract(local,"(\\w+)$")
  if(tipo=="csv"){
    dados<-utils::read.csv(local,header=h,sep=s,dec=d,strip.white=FALSE,
                           na.strings = md)
  } else{
    if(tipo=="xls" || tipo=="xlsx"){
      dados<-as.data.frame(readxl::read_excel(local,na = md),col_names=h)
    } else{
      if(tipo=="ods"){
        dados<-as.data.frame(readODS::read_ods(local,na = md), col_names = h)
      }else{
        if(tipo=="gsheet"|stringr::str_detect(local,
                                              "https://docs.google.com/spreadsheets")){
          googlesheets4::gs4_auth()
          dados<-as.data.frame(googlesheets4::read_sheet(local,na=md),col_names=h)
        } else{
          if(tipo=="txt"|tipo==basename(local)){
            dados<-utils::read.table(local,header=h,sep=s,dec=d,
                                     strip.white=FALSE,na.strings = md)
          } else{
            stop("I did not detect the type of the file.")
          }
        }
      }
    }
  }

  #Checking if the file is ASCII
  dadosTemp<-NULL
  dadosTemp<-sapply(dados, paste0, collapse = " ")
  stopifnot(all(grepl("^[ -~]+$", dadosTemp)))

  #Replacing NAs
  dfnas<-is.na(dados)
  for(i in 1:length(dados)){
    if(sum(dfnas[,i])!=0){
      for(j in 1:nrow(dfnas)){
        if(dfnas[j,i]==TRUE){
          dados[j,i]<-omd
        }
      }
    }
  }

  #Checking if trait values are real and effects value are integer
  if(!all(sapply(data, is.numeric))){
    stop("All the information must be numeric. Alphanumeric data are not allowed")
  }else{
    if(!is.null(effectsInt)){
      if(dados[, effectsInt] %% 1 != 0 && dados[, effectsInt] < 0){
        stop("All data must be a positive integer for the effects.")
      }
      dados[, effectInt]<-sapply(dados[, effectInt],as.integer)
    }
  }

  #Checking if the integers are within limits
  if(dados[, effectsInt] > 2147483647){
    stop("Effects values are too big.")
  }
  #Checking if variances of trais are within a reasonable interval
  if(!is.null((traits))){
    varTemp<-lapply(dados[, traits], var)
    if(varTemp < 1e-5 || varTemp > 1e5){
      stop("Variances of the traits are too small ou too big. You should scale the data.")

    }
  }

  #Formatting columns of data
  #Effects columns
  d1<-dados[, -traits]
  i<-1
  while(i <= length(traits)){
    d1<-rbind(d1,dados[, -traits])
    i <- i + 1
  }

  #Traits columns
  d2<-rep(unlist(lapply(dados[, traits], c)), traits)

  #Ordering
  dados<-data.frame(d1,d2)
  dados<-dados[order(dados[, 2], dados[, 1], na.last = FALSE), ]

  #Adding trait number column
  d3<-rep(1:length(traits), length(d2)/length(traits))
  dados<-data.frame(dados, d3)


  #Ensuring that output file's name is no longer than 30 characters
  length_of<-nchar(of)
  if(length_of > 30){
    of<-stringr::str_sub(of,-30)
  }

  #Writing data with fixed columns format
  write.fwf(dados, file = of, sep = " ", na = omd, colnames = FALSE,
            eol = Eol, scientific = FALSE)
}


