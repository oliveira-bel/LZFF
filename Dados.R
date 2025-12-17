#ANÁLISE DE CONSISTENCIA DOS DADOS MUNDO NOVO

dados<-read_excel("/home/isabel/Downloads/BKP/Dados R/mundo_novo_ricardo.xlsx")



#ANÁLISE DE CONSISTENCIA DO PESO AO NASCIMENTO

summary(dados$pesnas)
dados$pesnas[dados$pesnas<=20]<-NA
dados$pesnas[dados$pesnas>=50]<-NA
hist(dados$pesnas)
table(dados$pesnas)

#ANÁLISE DE CONSISTENCIA DO PESO AO 18 MESSES

summary(dados)
dados$pes18[dados$pes18==0]<-NA
dados$pes18[dados$pes18<=235]<-NA
dados$pes18[dados$pes18>=450]<-NA
hist(dados$pes18)
table(dados$pes18)

#ANÁLISE DE CONSISTÊNCIA DO PESO Á DESMAMA

summary(dados)
dados$pesdes[dados$pesdes==0]<-NA
dados$pesdes[dados$pesdes<=80]<-NA
dados$pesdes[dados$pesdes>=270]<-NA
hist(dados$pesdes)
table(dados$pesdes)

#ANÁLISE DE CONSISTÊNCIA DO PESO Á 15 MESSES

summary(dados)
dados$pes15[dados$pes15==0]<-NA
dados$pes15[dados$pes15<=200]<-NA
dados$pes15[dados$pes15>=370]<-NA
hist(dados$pes15)
table(dados$pes15)

#ANÁLISE DE CONSIÊNCIA DO PESO Á 24 MESES

summary(dados$pes24)
dados$pes24[dados$pes24==0]<-NA
dados$pes24[dados$pes24<=300]<-NA
dados$pes24[dados$pes24>=480]<-NA
hist(dados$pes24)
table(dados$pes24)


#TRANSFORMANDO OS DADOS EM DATAS

Sys.getlocale("LC_TIME")
localeOriginal<-Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "en_US.utf8")
dados$datanas<-as.Date(dados$datanas,format = "%d-%b-%y")

#CORRIGINDO DATAS
dados$datanas<- as.Date(ifelse(dados$datanas > Sys.Date(),
                               format(dados$datanas, "19%y-%m-%d"),
                               format(dados$datanas)))
Sys.setlocale("LC_TIME", localeOriginal)


#ESTRAINDO ANO DE NASCIMENTO

dados$ano<-format(dados$datanas,"%Y")

#ESTRAINDO ESTAÇÃO DO ANO

dados$mes<-format(dados$datanas,"%m")

#Criando dados estacacao

dados$mes<-as.numeric(dados$mes)
dados$est<-ifelse(dados$mes<=3|dados$mes>=10,
                  "aguas",
                  "seca")

#CRIANDO GC

dados$gc<-paste0(dados$ano,dados$est)
table(dados$gc)
summary(dados$ano)
dados$ano<-as.numeric(dados$ano)

#EXCLUINDO ANIMAIS PEQUENOS DE GC

tgc<-table(dados$gc)
ts<-tgc[tgc>50]
sum(tgc)
pd<-sum(tgc)-sum(ts)
print(pd)

nts<-names(ts)       #tirando os nomes de gc
i<-dados$gc%in%nts
#indexador
dados<-dados[i,]

#SELECIONANDO OS DADOS FENOTÍPICOS

dadosfinal<-dados[,c("idf","idfpai","idfmae","pesnas","pesdes","pes15", "pes18","pes24","gc")]

#SELECIONANDO OS DADOS DE PEDIGREE

dadosfinalped<-dados[,c("idf","idfpai",'idfmae')]

#consistencia dos dados fenotipicos

dup<-duplicated(dadosfinal$idf)
sum(dup)
dadosfinal<-dadosfinal[!dup,]

#analise de consistencia ped selecacao final

id<-duplicated(dadosfinalped[,c("idf")])
dadosfinalped<-dadosfinalped[!id,]
dadosfinalped[dadosfinalped$idf=="A6403",]

pais<-dadosfinalped$idfpai%in%dadosfinalped$idfmae
sum(pais)


