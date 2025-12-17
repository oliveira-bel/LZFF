#--------------------------------------------
#Consistência dos dados da Mundo Novo
#autor: Ricardo da Fonseca
#data:  18/09/2025
#--------------------------------------------

dados<-read_excel("/home/isabel/Downloads/mundo_novo_ricardo.xlsx")
sub<-function(x){
  ifelse(x == "-   -", NA, x)
}
dados<-lapply(dados, sub)
rm(sub)

dados<-as.data.frame(dados)


#Trabalhando com datas----------------------------------------------------------

#Ajustando o locale para leitura correta das datas
Sys.getlocale("LC_TIME")
localeOriginal<-Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "en_US.utf8")
dados$datanas<-as.Date(dados$datanas,format = "%d-%b-%y")

#Acertando o século
dados$datanas<- as.Date(ifelse(dados$datanas > Sys.Date(),
                               format(dados$datanas, "19%y-%m-%d"),
                               format(dados$datanas)))

#Acertando o século de anos menores que a data atual
ano<-format(dados$datanas, "%Y")
dados$datanas<-as.Date(ifelse(ano > 2000 & dados$safra < 2000,
                      format(dados$datanas, "19%y-%m-%d"),
                      format(dados$datanas)))

#Retornando ao locale original
Sys.setlocale("LC_TIME", localeOriginal)
rm(localeOriginal, ano)
#-------------------------------------------------------------------------------

summary(dados$datanas)

#Características trabalhadas: Pesos ao nascimento, desmama e aos 18 meses

with(dados,
     summary(data.frame(idf, idfpai, idfmae, pesnas, pesdes, pes18)))

#Consistência para peso ao nascimento---------------------------------------------
table(dados$pesnas)
dados$pesnas<-ifelse(dados$pesnas < 20 | dados$pesnas > 45, NA, dados$pesnas)
hist(dados$pesnas)
qqnorm(dados$pesnas)
qqline(dados$pesnas, col = "red")
boxplot(dados$pesnas)
summary(dados$pesnas)

#Consistência para peso à desmama-----------------------------------------------
table(dados$pesdes)
dados$pesdes<-ifelse(dados$pesdes < 105 | dados$pesdes > 270, NA, dados$pesdes)
hist(dados$pesdes)
qqnorm(dados$pesdes)
qqline(dados$pesdes, col = "red")
boxplot(dados$pesdes)
summary(dados$pesdes)

#Consistência para peso aos 18 meses--------------------------------------------
table(dados$pes18)
dados$pes18<-ifelse(dados$pes18 < 220 | dados$pes18 > 400, NA, dados$pes18)
hist(dados$pes18)
qqnorm(dados$pes18)
qqline(dados$pes18, col = "red")
boxplot(dados$pes18)
summary(dados$pes18)

#Criando grupos de contemporâneos-----------------------------------------------
#gc = ano e estação de nascimento
dados$ano<-format(dados$datanas, "%Y")

#definindo as estações
#seca: abr-set, aguas: out-mar
dados$mes<-format(dados$datanas, "%m")
dados$mes<-as.numeric(dados$mes)
dados$est<-ifelse(dados$mes >= 04 & dados$mes <= 09, "seca", "aguas")

#criando os grupos de contemporâneos
dados$gc<-paste0(dados$ano, dados$est)
table(dados$gc)
#-------------------------------------------------------------------------------

#Consistência dos dados de grupos de contemporâneos-----------------------------
tabGC<-table(dados$gc)
tabGC<-tabGC[tabGC > 50]
dadosperdidos<-nrow(dados)-sum(tabGC)
dadosperdidos

#Eliminando os registros dos grupos de contemporâneos que foram descartados
ind<-dados$gc%in%names(tabGC)
dados<-dados[ind,]
#-------------------------------------------------------------------------------

#Selecionando somente as colunas a serem utilizadas
dados<-dados[,c(1:3, 8, 12, 18, 44)]
names(dados)<-c("ind", "sire", "dam", "pn", "pd", "p18", "gc")

dup<-duplicated(dados$ind, fromLast = TRUE)
sum(dup)
dados<-dados[!dup,]

saveRDS(dados, "~/Documents/WOMBAT/dadosfinal.RDS")
