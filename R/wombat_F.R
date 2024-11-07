wombat_F<-function(caminho){
  nome<-basename(caminho)
  comp<-nchar(nome)
  if(comp>30){
    nome<-substr(nome,1,30)
    string<-substr(nome,1,30)
    substr(string,start=nchar(string)-30,stop=nchar(string))
    print(string)
  }
  }

