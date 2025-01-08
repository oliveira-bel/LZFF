formatB<-function(local,s,d,h=TRUE,md=c(""," ","NA"),
                  of="formatA_data",omd=0,eol="\n"){

  ok_of<-if(stringr::str_detect(of,"#")){
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
      if(tipo=="txt"|tipo==basename(local)){
        dados<-utils::read.table(local,header=h,sep=s,dec=d,
                                 strip.white=FALSE,na.strings = md)
      } else{
        stop("I did not detect the type of the file.")
      }
    }
  }

  #Detectando se o arquivo contém caracteres não ASCII
  dadosTemp<-NULL
  dadosTemp<-sapply(dados, paste0, collapse = " ")
  stopifnot(all(grepl("^[ -~]+$", dadosTemp)))

  name<-basename(local)
  length_name<-nchar(name)
  if(length_name>30){
    of_name<-stringr::str_sub(name,-30)
  }
}
