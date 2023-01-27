library(ggplot2)
library(stringr)
library(tidyverse)
library(reshape2)

db=read.csv("/Users/silvanooliveira/Documents/Pessoal/CEPAL/patentes.csv")

for(i in 1:ncol(db)){
  print(table(is.na(db[,i])))
}


# Avaliação das variáveis ---------------------------------------------------------------


# Variável X --------------------------------------------------------------

table(is.na(as.numeric(db$X)))
View(db[is.na(as.numeric(db$X)),])


# Variável numeroPedido ---------------------------------------------------

table(substr(db$numeroPedido,1,2))

###Possíveis valores
#  ['    AP    BR    C1    C2    C3    Ce    CO    DI    MÉ    MU    Pa    PI    PP    PR    RE    SI    TR

#Excluindo os valores com problema na primeira variável (X) os valores ficam OK
table(substr(db[is.na(as.numeric(db$X))==F,]$numeroPedido,1,2))

###Possíveis valores
# BR    C1    C2    C3    MU    PI    PP

# Variável numeroBusca ----------------------------------------------------------------
db$numeroBusca


# Variável titulo ----------------------------------------------------------------

table(db$titulo=="")
table(db$titulo=="nan")

#Excluindo os valores com problema na primeira variável (X)
table(db[is.na(as.numeric(db$X))==F,]$titulo=="")
table(db[is.na(as.numeric(db$X))==F,]$titulo=="nan")

View(db[db$titulo=="nan",])


# Variável resumo ---------------------------------------------------------

table(db$resumo=="")
table(db$resumo=="nan")

#Excluindo os valores com problema na primeira variável (X)

table(db[is.na(as.numeric(db$X))==F,]$resumo=="")
View(db[db$resumo=="",])

table(db[is.na(as.numeric(db$X))==F,]$resumo=="nan")


# Variável depositante ----------------------------------------------------
table(db$depositantes=="")
table(db$depositantes=="nan")


# Variável inventores -----------------------------------------------------
table(db$inventores=="")
table(db$inventores=="nan")


# Variável paises ---------------------------------------------------------

table(db$paises=="")
table(db$paises=="[]")

### TRANSFORMAÇÃO
db$pais=str_replace_all(db$paises,"\\[","")
db$pais=str_replace_all(db$pais,"\\]","")
db$pais=str_replace_all(db$pais,"'","")
db$pais=str_replace_all(db$pais,"\\.","")

#table(db$pais)
#max(lengths(regmatches(db$pais, gregexpr(",", db$pais))))

db$pais2=db$pais
db <- db %>% 
  separate(pais2, into = c(paste0("pais_",seq(1,max(lengths(regmatches(db$pais, gregexpr(",", db$pais))))))), sep = ",")

table(is.na(db$pais_1))
table(db$pais_1=="")

table(db[is.na(as.numeric(db$X))==F,]$pais_1)

for(i in colnames(db)[str_detect(colnames(db),"pais_")]){
  db[,i]=str_trim(db[,i])
  db[which(nchar(db[,i])>5),i]=NA
  #db[which(str_detect(db[,i],"BR")),i]="BR"
}

table(db$pais_1)
table(db$pais_2)
table(db$pais_3)
table(db$pais_4)
table(db$pais_5)
table(db$pais_6)
table(db$pais_7)
table(db$pais_8)
table(db$pais_9)
table(db$pais_10)

###Valores a excluir
excluir=c("1991","1999","2000","2003","2004","2005","2006","2007","2009","25%","50%","51%","CHUM","CIRAD","CNRS",
          "CSIC","CSIG","Gmbh","GMBH","I.R.D","IAE","ICAT","INDIA","INRA","INTA","IPK","IRD","Iwood", "JSC",
          "Ltd","Ltd.","LTD.","MAG","NO. 2","no.2","NO.2","NTNU","publ","Publ","PUBL","Publ.","RATP","S. A.",
          "S.A","SARL","SAS","SATC","SNCF","SUPSI","UNGDA","Teni","TFRES","UFSC","Vito","VITO","ZWUS","EMBL","CNRS",
          "GROUP","INSTM","IVIA","RDA","UNESP","Cirad","UFMS","ACES","SCRAS","CRVC","CDN","BVI","EFS","EPFL",
          "no2","NO2","NRC","LTD","FUB","GNIS","INSA","UNL","CEA","FIL","CEA","Pty","PTY")


#db[which(str_detect(db$pais,paste(excluir, collapse = "|"))),]$pais=NA
for(i in colnames(db)[str_detect(colnames(db),"pais_")]){
  db[which(str_detect(db[,i],paste(excluir, collapse = "|"))),i]=NA
}

for(i in colnames(db)[str_detect(colnames(db),"pais_")]){
  db[,i]=ifelse(db[,i]=="CHINA","CN",db[,i])
  db[,i]=ifelse(substr(db[,i],1,2)==substr(db[,i],4,6),substr(db[,i],1,2),db[,i])
}

for(i in colnames(db)[str_detect(colnames(db),"pais_")]){
  db[,i]=substr(db[,i],1,2)
}

###UF
db$uf=db$pais
db <- db %>% 
  separate(uf, into = c(paste0("uf_",seq(1,max(lengths(regmatches(db$pais, gregexpr("BR/", db$uf))))))), sep = ",")

for(i in colnames(db)[str_detect(colnames(db),"uf_")]){
  db[which(str_detect(db[,i],"BR")==F),i]=NA
  db[,i]=str_trim(db[,i])
  db[,i]=substr(db[,i],4,6)
}



#################### OBSERVACAO ########################
########################################################
### TENTAR UM SCRIPT AUTOMATIZADO PARA EXECUTAR ISSO ###
########################################################
db$uf_2=ifelse(db$uf_2==db$uf_1,NA,db$uf_2)
db$uf_3=ifelse(db$uf_3==db$uf_1,NA,db$uf_3)
db$uf_4=ifelse(db$uf_4==db$uf_1,NA,db$uf_4)
db$uf_5=ifelse(db$uf_5==db$uf_1,NA,db$uf_5)
db$uf_6=ifelse(db$uf_6==db$uf_1,NA,db$uf_6)
db$uf_7=ifelse(db$uf_7==db$uf_1,NA,db$uf_7)

db$uf_3=ifelse(db$uf_3==db$uf_2,NA,db$uf_3)
db$uf_4=ifelse(db$uf_4==db$uf_2,NA,db$uf_4)
db$uf_5=ifelse(db$uf_5==db$uf_2,NA,db$uf_5)
db$uf_6=ifelse(db$uf_6==db$uf_2,NA,db$uf_6)
db$uf_7=ifelse(db$uf_7==db$uf_2,NA,db$uf_7)

db$uf_4=ifelse(db$uf_4==db$uf_3,NA,db$uf_4)
db$uf_5=ifelse(db$uf_5==db$uf_3,NA,db$uf_5)
db$uf_6=ifelse(db$uf_6==db$uf_3,NA,db$uf_6)
db$uf_7=ifelse(db$uf_7==db$uf_3,NA,db$uf_7)

db$uf_5=ifelse(db$uf_5==db$uf_4,NA,db$uf_5)
db$uf_6=ifelse(db$uf_6==db$uf_4,NA,db$uf_6)
db$uf_7=ifelse(db$uf_7==db$uf_4,NA,db$uf_7)

db$uf_6=ifelse(db$uf_6==db$uf_5,NA,db$uf_6)
db$uf_7=ifelse(db$uf_7==db$uf_5,NA,db$uf_7)

db$uf_7=ifelse(db$uf_7==db$uf_6,NA,db$uf_7)

db$uf_5=NULL
db$uf_6=NULL
db$uf_7=NULL
########################################################
########################################################


db$brasil=0
db[which(db$pais_1=="BR" | db$pais_2=="BR" | db$pais_3=="BR" | 
           db$pais_4=="BR" | db$pais_5=="BR" | db$pais_6=="BR" |
           db$pais_7=="BR"),]$brasil=1
db[which(db$paises=="" | db$paises=="[]"),]$brasil=9

db$brasil2=ifelse(is.na(db$uf_1)==F | is.na(db$uf_2)==F | is.na(db$uf_3)==F | is.na(db$uf_4)==F,1,0)

table(db$brasil2,db$brasil)
prop.table(table(db$brasil))



# Variável  listaClassificacaoInternacional ---------------------------------------------------------------

table(db$listaClassificacaoInternacional=="")



# Variável status ---------------------------------------------------------

table(db$status=="")

db$status2=db$status
db <- db %>% 
  separate(status2, into = c(paste0("status",seq(1,max(lengths(regmatches(db$status, gregexpr(": ", db$status))))))), sep = ": ")


db$status1=str_replace_all(db$status1,"'","")
db$status1=str_replace_all(db$status1,"\\{","")

table(db$status1=="")

table(db$status1)

db[db$status1=="",]$status1="sem informação"



# Ano ---------------------------------------------------------------------

table(is.na(db$ano))
min(db$ano,na.rm=T)
max(db$ano,na.rm=T)


min(db[which(db$ano>min(db$ano,na.rm=T)),]$ano,na.rm=T)



# Criação variável categoria IPC e Categoria IEA ------------------------------------


