library(ggplot2)
library(stringr)
library(tidyverse)
library(reshape2)
library(lubridate)
library(devtools)

#devtools::install_github("ropensci/gender")
#remotes::install_github("lmullen/genderdata")
library(genderBR)
library(gender)

setwd("..")

#install("painel/pacotes/inpietl")
library(inpietl)

db=read.csv("painel/data-raw/patentes.csv",sep="#")


# Paises ---------------------------------------------------------

## Remover caracteres especiais (colchetes, aspas, etc)
db$pais=inpietl::remove_caracter(db$paises)

## Separar os múltiplos paises
db$pais2=db$pais
db <- db %>% 
  separate(pais2, into = c(paste0("pais_",seq(1,max(lengths(regmatches(db$pais, gregexpr(",", db$pais))))))), sep = ",")

## Remover excesso de espaço no começo da variável e remover termos que não se referem a nome sigla de países
## em todas as variáveis país
for(i in colnames(db)[str_detect(colnames(db),"pais_")]){
  db[,i]=str_trim(db[,i])
  db[which(nchar(db[,i])>5),i]=NA
}

## Valores a excluir
excluir=c("1991","1999","2000","2003","2004","2005","2006","2007","2009","25%","50%","51%","CHUM","CIRAD","CNRS",
          "CSIC","CSIG","Gmbh","GMBH","I.R.D","IAE","ICAT","INDIA","INRA","INTA","IPK","IRD","Iwood", "JSC",
          "Ltd","Ltd.","LTD.","MAG","NO. 2","no.2","NO.2","NTNU","publ","Publ","PUBL","Publ.","RATP","S. A.",
          "S.A","SARL","SAS","SATC","SNCF","SUPSI","UNGDA","Teni","TFRES","UFSC","Vito","VITO","ZWUS","EMBL","CNRS",
          "GROUP","INSTM","IVIA","RDA","UNESP","Cirad","UFMS","ACES","SCRAS","CRVC","CDN","BVI","EFS","EPFL",
          "no2","NO2","NRC","LTD","FUB","GNIS","INSA","UNL","CEA","FIL","CEA","Pty","PTY")

## Atribuindo NA para os termos da lista de exclusão para todas as variáveis país
for(i in colnames(db)[str_detect(colnames(db),"pais_")]){
  db[which(str_detect(db[,i],paste(excluir, collapse = "|"))),i]=NA
}

## Corrigindo nomes e removendo as repetições das siglas dos países
for(i in colnames(db)[str_detect(colnames(db),"pais_")]){
  db[,i]=ifelse(db[,i]=="CHINA","CN",db[,i])
  db[,i]=ifelse(substr(db[,i],1,2)==substr(db[,i],4,6),substr(db[,i],1,2),db[,i])
}

## Removendo os caracteres adicionais das variáveis país (acima de 2 caracteres)
for(i in colnames(db)[str_detect(colnames(db),"pais_")]){
  db[,i]=substr(db[,i],1,2)
}


# UF ----------------------------------------------------------------------

## Criação de múltiplas variáveis para UF
db$uf=db$pais
db <- db %>% 
  separate(uf, into = c(paste0("uf_",seq(1,max(lengths(regmatches(db$pais, gregexpr("BR/", db$uf))))))), sep = ",")


## Atribuindo NA para siglas diferentes do Brasil, removendo excesso de espaço no começo das variáveis e extração
## somente dos termos da UF com 2 dígitos
for(i in colnames(db)[str_detect(colnames(db),"uf_")]){
  db[which(str_detect(db[,i],"BR")==F),i]=NA
  db[,i]=str_trim(db[,i])
  db[,i]=substr(db[,i],4,6)
}


#################### OBSERVACAO ########################
########################################################
### TENTAR UM SCRIPT AUTOMATIZADO PARA EXECUTAR ISSO ###
########################################################

## Removendo as repetições de UF nas diferentes variáveis
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


# Nacionalidade -----------------------------------------------------------

## Definindo variável para identificar patentes com inventores do Brasil
db$brasil=+(apply(db[,colnames(db)[str_detect(colnames(db),"pais_")]]=="BR",1,any))
db$brasil=ifelse(is.na(db$brasil),0,db$brasil)
db$brasil=ifelse(db$paises=="" | db$paises=="[]",9,db$brasil)


# América -----------------------------------------------------------------

## Definindo variável para identificar patentes com inventores de algum país das américas
america=c("AI","AG","AR","AW","BS","BB","BZ","BM","BO","XA","VG","BR","CA","KY","CL","CO","CR",
          "CU","CW","DM","DO","EC","SV","FK","GF","GD","GP","GT","GY","HT","HN","JM","MQ","MX",
          "MS","NI","PA","PY","PE","PR","XC","BL","XB","KN","LC","MF","VC","PM","SX","SR","TT",
          "TC","US","UY","VE","VI")

db$america=ifelse(str_detect(db$pais,paste(america,collapse = "|")),1,0)
db$america=ifelse(db$paises=="" | db$paises=="[]",9,db$america)


# Cooperação internacional ------------------------------------------------

## Definindo variável para identificar patentes de inventores do Brasil com cooperação de outros países
db$inter=+(apply(db[,colnames(db)[str_detect(colnames(db),"pais_")]]!="BR",1,any))
db$inter=ifelse(is.na(db$inter)==T,0,db$inter)

db$cooperacao=ifelse(db$brasil==1 & db$inter==1,1,
                     ifelse(db$brasil==1 & db$inter==0,0,NA))


# Status ---------------------------------------------------------

## Separação da variável status em seus componentes
db$status2=db$status
db <- db %>% 
  separate(status2, into = c(paste0("status",seq(1,max(lengths(regmatches(db$status, gregexpr(": ", db$status))))))), sep = ": ")

## Criação da variável status da patente
db$status1=remove_caracter(db$status1)
db$status1=ifelse(db$status1=="","sem informação",db$status1)

# Ano do deferimento ------------------------------------------------------

## Criação da variável ano do deferimento a partir da subdivisão da variável status
db$ano_deferimento=ifelse(str_detect(db$status2,"dataDeferimento"),as.numeric(substr(db$status3,unlist(gregexpr("\\(", db$status3))+1,unlist(gregexpr("\\(", db$status3))+4)),NA)
db$ano_deferimento=ifelse(db$ano_deferimento>1900 & db$ano_deferimento<=year(Sys.Date()),db$ano_deferimento,NA)

# Data da concessao -------------------------------------------------------

## Criação da variável ano da concessão a partir da subdivisão da variável status
db$ano_concessao=ifelse(str_detect(db$status2,"dataConcessao"),as.numeric(substr(db$status3,unlist(gregexpr("\\(", db$status3))+1,unlist(gregexpr("\\(", db$status3))+4)),NA)
db$ano_concessao=ifelse(db$ano_concessao>1900 & db$ano_concessao<=year(Sys.Date()),db$ano_concessao,NA)

# Data indeferimento ------------------------------------------------------

## Criação da variável ano do indeferimento a partir da subdivisão da variável status
db$ano_indeferimento=ifelse(str_detect(db$status2,"dataIndeferimento"),as.numeric(substr(db$status3,unlist(gregexpr("\\(", db$status3))+1,unlist(gregexpr("\\(", db$status3))+4)),NA)
db$ano_indeferimento=ifelse(db$ano_indeferimento>1900 & db$ano_indeferimento<=year(Sys.Date()),db$ano_indeferimento,NA)

# Ano do pedido -----------------------------------------------------------
## Criação da variável ano do pedido a partir da variável ano
db$ano_pedido=db$ano

# Criação classificação categorias WIPO -------------------------------------------

db$cod1=ifelse(str_detect(db$listaClassificacaoInternacional,
                          paste(c("C10B 53/02","C10L 5/4","C10L 9/"),collapse = "|")),1,0)

db$cod2=ifelse(str_detect(db$listaClassificacaoInternacional,
                          paste(c("C10L 1/02","C10L 1/19"),collapse = "|")),1,0)

db$cod3=ifelse(str_detect(db$listaClassificacaoInternacional,
                          paste(c("C07C 67/","C07C 69/","C10G","C10L 1/02",
                                  "C10L 1/19","C11C 3/10","C12P 7/649"),collapse = "|")),1,0)
      
db$cod4=ifelse(str_detect(db$listaClassificacaoInternacional,
                          paste(c("C10L 1/02","C10L 1/182","C12N 9/24","C12P 7/06",
                                  "C12P 7/08","C12P 7/10","C12P 7/12","C12P 7/14"),collapse = "|")),1,0)

db$cod5=ifelse(str_detect(db$listaClassificacaoInternacional,
                          paste(c("C02F 3/28","C02F 11/04","C10L 3/","C12M 1/107","C12P 5/02"),collapse = "|")),1,0)


db$cod6=ifelse(str_detect(db$listaClassificacaoInternacional,
                          paste(c("C12N 1/13","C12N 1/15","C12N 1/21","C12N 5/10",
                                  "C12N 15/","A01H"),collapse = "|")),1,0)

db$cod7=ifelse(str_detect(db$listaClassificacaoInternacional,
                          paste(c("C10L 3/","F02C 3/28"),collapse = "|")),1,0)
                          
db$cod8=ifelse(str_detect(db$listaClassificacaoInternacional,
                          paste(c("H01M 4/86","H01M 4/88","H01M 4/90","H01M 4/92",
                                  "H01M 4/94","H01M 4/96","H01M 4/98","H01M 8/","H01M 12/"),collapse = "|")),1,0)
                      
db$cod9=ifelse(str_detect(db$listaClassificacaoInternacional,
                          paste(c("H01M 4/86","H01M 4/88","H01M 4/90","H01M 4/92",
                                  "H01M 4/94","H01M 4/96","H01M 4/98"),collapse = "|")),1,0)
                      
db$cod10=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("H01M 4/86","H01M 4/88","H01M 4/90","H01M 4/92",
                                   "H01M 4/94","H01M 4/96","H01M 4/98"),collapse = "|")),1,0)

db$cod11=ifelse(str_detect(db$listaClassificacaoInternacional,"H01M 8/")==T,1,0)

db$cod12=ifelse(str_detect(db$listaClassificacaoInternacional,"H01M 12/")==T,1,0)

db$cod13=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("C10B 53/","C10J"),collapse = "|")),1,0)

db$cod14=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("C10L 5/42","C10L 5/44"),collapse = "|")),1,0)

db$cod15=ifelse(str_detect(db$listaClassificacaoInternacional,"F23G 7/10")==T,1,0)

db$cod16=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("C10J 3/02","C10J 3/46","F23B 90/0","F23G 5/027"),collapse = "|")),1,0)

db$cod17=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("B09B 3/","F23G 7/"),collapse = "|")),1,0)

db$cod18=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("C10L 5/48","F23G 5/","F23G 7/"),collapse = "|")),1,0)

db$cod19=ifelse(str_detect(db$listaClassificacaoInternacional,"C21B 5/06")==T,1,0)

db$cod20=ifelse(str_detect(db$listaClassificacaoInternacional,"D21C 11/")==T,1,0)

db$cod21=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("A62D 3/02","C02F 11/04","C02F 11/14"),collapse = "|")),1,0)

db$cod22=ifelse(str_detect(db$listaClassificacaoInternacional,"F23G 7/10")==T,1,0)

db$cod23=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("B09B 3/","F23G 5/"),collapse = "|")),1,0)

db$cod24=ifelse(str_detect(db$listaClassificacaoInternacional,"B09B")==T,1,0)

db$cod25=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("B01D 53/02","B01D 53/04","B01D 53/047",
                                   "B01D 53/14","B01D 53/22","B01D 53/24"),collapse = "|")),1,0)

db$cod26=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("C10L 5/46","F23G 5/"),collapse = "|")),1,0)

db$cod27=ifelse(str_detect(db$listaClassificacaoInternacional,"E02B 9/")==T,1,0)

db$cod28=ifelse(str_detect(db$listaClassificacaoInternacional,"E02B 9/08")==T,1,0)

db$cod29=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("F03B 13/1","F03B 13/2"),collapse = "|")),1,0)

db$cod30=ifelse(str_detect(db$listaClassificacaoInternacional,"F03B 15/")==T,1,0)

db$cod31=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("B63H 19/02","B63H 19/04"),collapse = "|")),1,0)

db$cod32=ifelse(str_detect(db$listaClassificacaoInternacional,"F03G 7/05")==T,1,0)

db$cod33=ifelse(str_detect(db$listaClassificacaoInternacional,"F03D")==T,1,0)

db$cod34=ifelse(str_detect(db$listaClassificacaoInternacional,"H02K 7/18")==T,1,0)

db$cod35=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("B63B 35/","E04H 12/","F03D 13/"),collapse = "|")),1,0)

db$cod36=ifelse(str_detect(db$listaClassificacaoInternacional,"B60K 16/")==T,1,0)

db$cod37=ifelse(str_detect(db$listaClassificacaoInternacional,"B60L 8/")==T,1,0)

db$cod38=ifelse(str_detect(db$listaClassificacaoInternacional,"B63H 13/")==T,1,0)

db$cod39=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("F24S","H02S"),collapse = "|")),1,0)

db$cod40=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("H01L 27/142","H01L 31/02","H01L 31/03",
                                   "H01L 31/04","H01L 31/05","H01L 31/06",
                                   "H01L 31/07","H01G 9/20","H02S 10/"),collapse = "|")),1,0)


db$cod41=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("H01L 27/30","H01L 51/42","H01L 51/43","H01L 51/44",
                                   "H01L 51/45","H01L 51/46","H01L 51/47","H01L 51/48"),collapse = "|")),1,0)

db$cod42=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("H01L 25/03","H01L 25/16","H01L 25/18","H01L 31/042"),collapse = "|")),1,0)

db$cod43=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("C01B 33/02","C23C 14/14","C23C 16/24","C30B 29/06"),collapse = "|")),1,0)

db$cod44=ifelse(str_detect(db$listaClassificacaoInternacional,"G05F 1/67")==T,1,0)

db$cod45=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("F21L 4/0","F21S 9/03"),collapse = "|")),1,0)

db$cod46=ifelse(str_detect(db$listaClassificacaoInternacional,"H02J 7/35")==T,1,0)

db$cod47=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("H01G 9/20","H01M 14/"),collapse = "|")),1,0)

db$cod48=ifelse(str_detect(db$listaClassificacaoInternacional,"F24S")==T,1,0)

db$cod49=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("F24D 17/","F24D 18/"),collapse = "|")),1,0)

db$cod50=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("F24D 3/","F24D 5/","F24D 11/","F24D 19/"),collapse = "|")),1,0)

db$cod51=ifelse(str_detect(db$listaClassificacaoInternacional,"F24S 90/")==T,1,0)

db$cod52=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("F03D 1/04","F03D 9/","F03D 13/20","F03G 6/"),collapse = "|")),1,0)

db$cod53=ifelse(str_detect(db$listaClassificacaoInternacional,"C02F 1/14")==T,1,0)

db$cod54=ifelse(str_detect(db$listaClassificacaoInternacional,"F02C 1/05")==T,1,0)

db$cod55=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("H01L 31/0525","H02S 40/44"),collapse = "|")),1,0)

db$cod56=ifelse(str_detect(db$listaClassificacaoInternacional,"B60K 16/")==T,1,0)

db$cod57=ifelse(str_detect(db$listaClassificacaoInternacional,"B60L 8/")==T,1,0)

db$cod58=ifelse(str_detect(db$listaClassificacaoInternacional,"F03G 6/")==T,1,0)

db$cod59=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("E04D 13/","E04D 13/18"),collapse = "|")),1,0)

db$cod60=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("F22B 1/","F24V 30/"),collapse = "|")),1,0)

db$cod61=ifelse(str_detect(db$listaClassificacaoInternacional,"F25B 27/")==T,1,0)

db$cod62=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("F26B 3/","F26B 3/28"),collapse = "|")),1,0)

db$cod63=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("F24S 23/","G02B 7/183"),collapse = "|")),1,0)

db$cod64=ifelse(str_detect(db$listaClassificacaoInternacional,"F24S 10/10")==T,1,0)

db$cod65=ifelse(str_detect(db$listaClassificacaoInternacional,"F24T")==T,1,0)

db$cod66=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("F01K","F24F 5/","F24T 10/","F24T 10/10","F24T 10/13",
                                   "F24T 10/15","F24T 10/17","F24T 10/20","F24T 10/30",
                                   "F24T 50/","H02N 10/","F25B 30/06"),collapse = "|")),1,0)

db$cod67=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("F03G 4/","F03G 7/04"),collapse = "|")),1,0)

db$cod68=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("F24T 10/","F24T 10/10","F24T 10/13","F24T 10/15",
                                   "F24T 10/17","F24T 10/20","F24T 10/30","F24T 50/",
                                   "F24V 30/","F24V 40/","F24V 40/10","F24V 50/"),collapse = "|")),1,0)

db$cod69=ifelse(str_detect(db$listaClassificacaoInternacional,"F24D 11/02")==T,1,0)

db$cod70=ifelse(str_detect(db$listaClassificacaoInternacional,"F24D 15/04")==T,1,0)

db$cod71=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("F24D 17/02","F24D 18/"),collapse = "|")),1,0)

db$cod72=ifelse(str_detect(db$listaClassificacaoInternacional,"F24H 4/")==T,1,0)

db$cod73=ifelse(str_detect(db$listaClassificacaoInternacional,"F25B 30/")==T,1,0)

db$cod74=ifelse(str_detect(db$listaClassificacaoInternacional,"F01K 27/")==T,1,0)

db$cod75=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("F01K 23/06","F01K 23/08","F01K 23/10",
                                   "F01N 5/","F02G 5/","F02G 5/02","F02G 5/04","F25B 27/02"),collapse = "|")),1,0)


db$cod76=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("F01K 17/","F01K 23/04"),collapse = "|")),1,0)


db$cod77=ifelse(str_detect(db$listaClassificacaoInternacional,"F02C 6/18")==T,1,0)

db$cod78=ifelse(str_detect(db$listaClassificacaoInternacional,"F25B 27/02")==T,1,0)

db$cod79=ifelse(str_detect(db$listaClassificacaoInternacional,"C02F 1/16")==T,1,0)

db$cod80=ifelse(str_detect(db$listaClassificacaoInternacional,"D21F 5/20")==T,1,0)

db$cod81=ifelse(str_detect(db$listaClassificacaoInternacional,"F22B 1/02")==T,1,0)

db$cod82=ifelse(str_detect(db$listaClassificacaoInternacional,"F23G 5/46")==T,1,0)

db$cod83=ifelse(str_detect(db$listaClassificacaoInternacional,"F24F 12/")==T,1,0)

db$cod84=ifelse(str_detect(db$listaClassificacaoInternacional,"F27D 17/")==T,1,0)

db$cod85=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("F28D 17/","F28D 19/","F28D 20/"),collapse = "|")),1,0)

db$cod86=ifelse(str_detect(db$listaClassificacaoInternacional,"C10J 3/86")==T,1,0)

db$cod87=ifelse(str_detect(db$listaClassificacaoInternacional,"B60K 6/")==T,1,0)

db$cod88=ifelse(str_detect(db$listaClassificacaoInternacional,"B60W 20/")==T,1,0)

db$cod89=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("F16H 3/","F16H 48/"),collapse = "|")),1,0)

db$cod90=ifelse(str_detect(db$listaClassificacaoInternacional,"H02K 29/08")==T,1,0)

db$cod91=ifelse(str_detect(db$listaClassificacaoInternacional,"H02K 49/10")==T,1,0)

db$cod92=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("B60L 7/1","B60L 7/2"),collapse = "|")),1,0)

db$cod93=ifelse(str_detect(db$listaClassificacaoInternacional,"B60L 8/")==T,1,0)

db$cod94=ifelse(str_detect(db$listaClassificacaoInternacional,"B60L 9/")==T,1,0)

db$cod95=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("B60L 50/","B60L 53/","B60L 55/","B60L 58/"),collapse = "|")),1,0)

db$cod96=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("F02B 43/","F02M 21/02","F02M 27/02"),collapse = "|")),1,0)

db$cod97=ifelse(str_detect(db$listaClassificacaoInternacional,"B60K 16/")==T,1,0)

db$cod98=ifelse(str_detect(db$listaClassificacaoInternacional,"H02J 7/")==T,1,0)

db$cod99=ifelse(str_detect(db$listaClassificacaoInternacional,
                           paste(c("B62D 35/","B63B 1/34","B63B 1/36","B63B 1/38","B63B 1/40"),collapse = "|")),1,0)

db$cod100=ifelse(str_detect(db$listaClassificacaoInternacional,"B61")==T,1,0)

db$cod101=ifelse(str_detect(db$listaClassificacaoInternacional,"B61D 17/02")==T,1,0)

db$cod102=ifelse(str_detect(db$listaClassificacaoInternacional,"B63H 9/")==T,1,0)

db$cod103=ifelse(str_detect(db$listaClassificacaoInternacional,"B63H 13/")==T,1,0)

db$cod104=ifelse(str_detect(db$listaClassificacaoInternacional,
                            paste(c("B63H 19/02","B63H 19/04"),collapse = "|")),1,0)

db$cod105=ifelse(str_detect(db$listaClassificacaoInternacional,"B63H 16/")==T,1,0)

db$cod106=ifelse(str_detect(db$listaClassificacaoInternacional,"B63H 21/18")==T,1,0)

db$cod107=ifelse(str_detect(db$listaClassificacaoInternacional,"B64G 1/44")==T,1,0)

db$cod108=ifelse(str_detect(db$listaClassificacaoInternacional,
                            paste(c("B60K 6/28","B60W 10/26","H01M 10/44","H01M 10/46",
                                    "H01G 11/","H02J 3/28","H02J 7/","H02J 15/"),collapse = "|")),1,0)

db$cod109=ifelse(str_detect(db$listaClassificacaoInternacional,"H02J")==T,1,0)

db$cod110=ifelse(str_detect(db$listaClassificacaoInternacional,"H02J 9/")==T,1,0)

db$cod111=ifelse(str_detect(db$listaClassificacaoInternacional,
                            paste(c("B60L 3/","G01R"),collapse = "|")),1,0)

db$cod112=ifelse(str_detect(db$listaClassificacaoInternacional,
                            paste(c("C09K 5/","F24H 7/","F28D 20/","F28D 20/02"),collapse = "|")),1,0)

db$cod113=ifelse(str_detect(db$listaClassificacaoInternacional,
                            paste(c("F21K 99/","F21L 4/02","H01L 33/","H01L  51/50","H05B 33/"),collapse = "|")),1,0)

db$cod114=ifelse(str_detect(db$listaClassificacaoInternacional,
                            paste(c("E04B 1/62","E04B 1/74","E04B 1/76","E04B 1/78","E04B 1/80",
                                    "E04B 1/88","E04B 1/90"),collapse = "|")),1,0)

db$cod115=ifelse(str_detect(db$listaClassificacaoInternacional,
                            paste(c("E04C 1/40","E04C 1/41","E04C  2/2"),collapse = "|")),1,0)

db$cod116=ifelse(str_detect(db$listaClassificacaoInternacional,"E06B 3/263")==T,1,0)

db$cod117=ifelse(str_detect(db$listaClassificacaoInternacional,
                            paste(c("E04B 2/","E04F 13/08"),collapse = "|")),1,0)

db$cod118=ifelse(str_detect(db$listaClassificacaoInternacional,
                            paste(c("E04B 5/","E04F 15/18"),collapse = "|")),1,0)

db$cod119=ifelse(str_detect(db$listaClassificacaoInternacional,
                            paste(c("E04B 7/","E04D 1/28","E04D 3/35","E04D 13/16"),collapse = "|")),1,0)

db$cod120=ifelse(str_detect(db$listaClassificacaoInternacional,
                            paste(c("E04B 9/","E04F 13/08"),collapse = "|")),1,0)

db$cod121=ifelse(str_detect(db$listaClassificacaoInternacional,"F03G 7/08")==T,1,0)

db$cod122=ifelse(str_detect(db$listaClassificacaoInternacional,
                            paste(c("B60K 6/10","B60K 6/30","B60L 50/30"),collapse = "|")),1,0)

db$cod123=ifelse(str_detect(db$listaClassificacaoInternacional,"G21")==T,1,0)

db$cod124=ifelse(str_detect(db$listaClassificacaoInternacional,"G21B")==T,1,0)

db$cod125=ifelse(str_detect(db$listaClassificacaoInternacional,"G21C")==T,1,0)

db$cod126=ifelse(str_detect(db$listaClassificacaoInternacional,"G21D")==T,1,0)

db$cod127=ifelse(str_detect(db$listaClassificacaoInternacional,"F02C 1/05")==T,1,0)


## Identificando a frequência de classificações das categorias IEA nível 2
db$sum=rowSums(db[,c(colnames(db)[str_detect(colnames(db),"cod")])])

## Selecionando os registros que foram classificados em pelo menos uma categoria IEA nível 2
## Removidos 1452 registros que não foram classificados em nenhuma categoria
## Até 9 classificações nível 2 uma mesma patente

db=db[which(db$sum>0),]

## Removendo a variável para verificar a frequência
db$sum=NULL


# Classificação nível 2 ---------------------------------------------------


## Criando variáveis para classificação Nível 2

db$iea11=+(apply(db[,c("cod19","cod20","cod25")]==1,1,any))

db$iea12=+(apply(db[,c("cod113","cod114","cod115","cod116","cod117","cod118","cod119","cod120","cod121")]==1,1,any))

db$iea13=+(apply(db[,c("cod74","cod87","cod88","cod89","cod90","cod91","cod92","cod93","cod94","cod95","cod96",
                       "cod97","cod98","cod99","cod100","cod101","cod102","cod103","cod104","cod105","cod106",
                       "cod107","cod122")]==1,1,any))

db$iea14=+(apply(db[,c("cod69","cod70","cod71","cod72","cod73","cod75","cod76","cod77","cod78","cod79","cod80",
                       "cod81","cod82","cod83","cod84","cod85","cod86","cod109","cod110","cod111")]==1,1,any))

db$iea31=+(apply(db[,c("cod39","cod40","cod41","cod42","cod43","cod44","cod45","cod46","cod47","cod48",
                       "cod49","cod50","cod51","cod52","cod53","cod54","cod55","cod56","cod57","cod58",
                       "cod59","cod60","cod61","cod62","cod63","cod64")]==1,1,any))

db$iea32=+(apply(db[,c("cod33","cod34","cod35","cod36","cod37","cod38")]==1,1,any))

db$iea33=ifelse(db$cod32==1,1,0)

db$iea34=+(apply(db[,c("cod2","cod3","cod4","cod5","cod6","cod13","cod14","cod15","cod16",
                       "cod17","cod18","cod21","cod22","cod23","cod24","cod26")]==1,1,any))

db$iea35=+(apply(db[,c("cod65","cod66","cod67","cod68")]==1,1,any))

db$iea36=+(apply(db[,c("cod27","cod28","cod29","cod30","cod31")]==1,1,any))

db$iea41=ifelse(db$cod124==1,1,0)

db$iea42=ifelse(db$cod125==1,1,0)

db$iea49=+(apply(db[,c("cod123","cod126")]==1,1,any))

db$iea52=+(apply(db[,c("cod8","cod9","cod10","cod11","cod12")]==1,1,any))

db$iea61=ifelse(db$cod127==1,1,0)

db$iea63=+(apply(db[,c("cod108","cod112")]==1,1,any))

## Criando variável para cálcuo da frequência de classificações Nível 1
db$sum=rowSums(db[,c(colnames(db)[str_detect(colnames(db),"iea")])])

## 72 registros que não apresentam classificação Nível 1
## Até 6 classificações nível 1 uma mesma patente

# Tipo de inventor ------------------------------------------------------

## Removendo caracteres especiais da variável depositante
db$depositantes2=remove_caracter(db$depositantes)

## Removendo os termos excedentes do nome do depositante que estão em parênteses
db$depositantes2=sub(" \\(.*", "",db$depositantes2)

## Removendo caracteres especiais da variável inventor
db$inventores2=remove_caracter(db$inventores)

## Separando a variável inventores em múltiplas entradas
db=db %>% separate(inventores2,into="inventores2",sep=", ")

## Criando variável para classificar se trata de pessoa física, considerando o nome do depositante vs inventor
db$tp_pessoa=ifelse(db$depositantes2==db$inventores2,1,2)

## Atribuindo valor ignorado onde não há preenchido o nome do depositante ou inventor
db$tp_pessoa=ifelse(db$depositantes=="" | db$inventores2=="" | db$depositantes=="nan",9,db$tp_pessoa)


# Inventor feminino ------------------------------------------------------

## Removendo caracteres especiais da variável inventores
db$inventores2=remove_caracter(db$inventores)

## Separando a variável em inventores nas múltiplas entradas
db=db %>% separate(inventores2,into=c(paste0("invent_",seq(1,max(lengths(regmatches(db$inventores2, gregexpr(",", db$inventores2))))))),sep=", ")

## Separando somente o primeiro nome de cada variável inventores
for(i in colnames(db)[str_detect(colnames(db),"invent_")]){
  db[,i]=sub(" .*", "",db[,i])
}

## Calculando o gênero em relação ao primeiro nome dos inventores para cada variável inventor
## Utilizado o pacote gender do Brasil (genderBR) para os registros com inventores brasileiros e gender para os demais
for(i in colnames(db)[str_detect(colnames(db),"invent_")]){
  db[which(db$brasil==1),i]=ifelse(is.na(db[which(db$brasil==1),i])==F,get_gender(db[which(db$brasil==1),i]),db[which(db$brasil==1),i])
  db[which(db$brasil!=1),i]=ifelse(is.na(db[which(db$brasil!=1),i])==F,gender(db[which(db$brasil!=1),i])$gender,db[which(db$brasil!=1),i])
}

## Criando a variável presença de depositante feminino com base em cada uma das variáveis inventores
db$feminino=+(apply(db[,colnames(db)[str_detect(colnames(db),"invent_")]]=="female" | 
                      db[,colnames(db)[str_detect(colnames(db),"invent_")]]=="Female",1,any))
db$feminino=ifelse(is.na(db$feminino) & db$inventores!="",0,db$feminino)
db$feminino=ifelse(is.na(db$feminino),9,db$feminino)


# Database Final ----------------------------------------------------------

## Definição das variáveis que irão compor a base de dados
vars=c("numeroBusca","brasil","america","cooperacao","status1","pais","ano_pedido","ano_deferimento","ano_concessao","ano_indeferimento",
       colnames(db)[str_detect(colnames(db),"iea")],"tp_pessoa","feminino")

db2=db[,vars]

write.csv(db2,"painel/data/database.csv",row.names = F)


# 
# names(df)[names(df) == 'old.var.name'] <- 'new.var.name'
# 
# names(cat)[names(cat) == c("nivel1","nivel2")] <- c("level1","level2")
# 
# names(cat)



# Old ---------------------------------------------------------------------

# Categoria IEA Nivel 1 ---------------------------------------------------

# db$iea1=+(apply(db[,colnames(db)[str_detect(colnames(db),"iea1")]]==1,1,any))
# db$iea3=+(apply(db[,colnames(db)[str_detect(colnames(db),"iea3")]]==1,1,any))
# db$iea4=+(apply(db[,colnames(db)[str_detect(colnames(db),"iea4")]]==1,1,any))
# db$iea5=db$iea52
# db$iea6=+(apply(db[,colnames(db)[str_detect(colnames(db),"iea6")]]==1,1,any))


#names(db2[,colnames(db2)[str_detect(colnames(db2),"iea")==F]])

db2=melt(db[,vars],id=names(db2[,colnames(db2)[str_detect(colnames(db2),"iea")==F]]))
db2$count=ifelse(duplicated(db2$numeroBusca)==F,1,0)

db2$nivel1=substr(db2$variable,1,4)

db2$nivel1=ifelse(db2$nivel1=="iea1","Eficiência Energética",
                  ifelse(db2$nivel1=="iea2","Energias Fósseis: Petróleo, Gás Natural e Carvão Mineral",
                         ifelse(db2$nivel1=="iea3","Fontes de Energia Renováveis",
                                ifelse(db2$nivel1=="iea4","Fissão e Fusão Nuclear",
                                       ifelse(db2$nivel1=="iea5","Hidrogênio e Células a Combustível",
                                              ifelse(db2$nivel1=="iea6","Outras Tecnologias de Geração e Armazenamento de Energia",
                                                     ifelse(db2$nivel1=="iea7","Outras Tecnologias e Pesquisas Transversais",NA)))))))

db2$nivel2=factor(db2$variable,levels = unique(db2$variable),labels=c("Tecnologias de Eficiência Energética aplicadas à Industria",
                                                                  "Tecnologias de Eficiência Energética aplicada a residências e estabelecimentos comerciais",
                                                                  "Tecnologias de Eficiência Energética aplicadas ao setor de transporte rodoviário",
                                                                  "Outras Tecnologias de Eficiência Energética","Energia solar","Energia Eólica",
                                                                  "Energia dos Oceanos","Biocombustíveis","Energia Geotérmica","Hidroeletricidade",
                                                                  "Fissão Nuclear","Fusão Nuclear","Outros fusão e fissão não alocados","Células a Combustível",
                                                                  "Outras Tecnologias de Geração","Armazenamento de Energia"))



db2$variable=NULL
write.csv(db2,"database.csv",row.names = F)



# Choices - Categoria nivel 1 e 2 -----------------------------------------

# cat=db2[duplicated(db2$variable)==F,c("nivel1","variable")]
# names(cat)=c("nivel1","nivel2")
# 
# cat$nivel2=factor(cat$nivel2,levels = unique(cat$nivel2),labels=c("Tecnologias de Eficiência Energética aplicadas à Industria",
#                                                                   "Tecnologias de Eficiência Energética aplicada a residências e estabelecimentos comerciais",
#                                                                   "Tecnologias de Eficiência Energética aplicadas ao setor de transporte rodoviário",
#                                                                   "Outras Tecnologias de Eficiência Energética","Energia solar","Energia Eólica",
#                                                                   "Energia dos Oceanos","Biocombustíveis","Energia Geotérmica","Hidroeletricidade",
#                                                                   "Fissão Nuclear","Fusão Nuclear","Outros fusão e fissão não alocados","Células a Combustível",
#                                                                   "Outras Tecnologias de Geração","Armazenamento de Energia"))
# 
# cat$nivel1=ifelse(cat$nivel1=="iea1","Eficiência Energética",
#                   ifelse(cat$nivel1=="iea2","Energias Fósseis: Petróleo, Gás Natural e Carvão Mineral",
#                          ifelse(cat$nivel1=="iea3","Fontes de Energia Renováveis",
#                                 ifelse(cat$nivel1=="iea4","Fissão e Fusão Nuclear",
#                                        ifelse(cat$nivel1=="iea5","Hidrogênio e Células a Combustível",
#                                               ifelse(cat$nivel1=="iea6","Outras Tecnologias de Geração e Armazenamento de Energia",
#                                                      ifelse(cat$nivel1=="iea7","Outras Tecnologias e Pesquisas Transversais",NA)))))))
# 
# write.csv(cat,"categorias_iea.csv",row.names = F)
