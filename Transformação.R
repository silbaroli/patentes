library(ggplot2)
library(stringr)
library(tidyverse)
library(reshape2)
library(lubridate)

#devtools::install_github("ropensci/gender")
#remotes::install_github("lmullen/genderdata")

library(genderBR)
library(gender)

#library(gender)

db=read.csv("patentes.csv",sep="#")


# Paises ---------------------------------------------------------

db$pais=str_replace_all(db$paises,"\\[","")
db$pais=str_replace_all(db$pais,"\\]","")
db$pais=str_replace_all(db$pais,"'","")
db$pais=str_replace_all(db$pais,"\\.","")

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


# UF ----------------------------------------------------------------------
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


# Nacionalidade -----------------------------------------------------------

db$brasil=0
db[which(db$pais_1=="BR" | db$pais_2=="BR" | db$pais_3=="BR" | 
           db$pais_4=="BR" | db$pais_5=="BR" | db$pais_6=="BR" |
           db$pais_7=="BR"),]$brasil=1
db[which(db$paises=="" | db$paises=="[]"),]$brasil=9

#db$brasil2=ifelse(is.na(db$uf_1)==F | is.na(db$uf_2)==F | is.na(db$uf_3)==F | is.na(db$uf_4)==F,1,0)


# Cooperação internacional ------------------------------------------------

db$inter=+(apply(db[,colnames(db)[str_detect(colnames(db),"pais_")]]!="BR",1,any))
db$inter=ifelse(is.na(db$inter)==T,0,db$inter)

db$cooperacao=ifelse(db$brasil==1 & db$inter==1,1,
                     ifelse(db$brasil==1 & db$inter==0,0,NA))


# Status ---------------------------------------------------------

db$status2=db$status
db <- db %>% 
  separate(status2, into = c(paste0("status",seq(1,max(lengths(regmatches(db$status, gregexpr(": ", db$status))))))), sep = ": ")


db$status1=str_replace_all(db$status1,"'","")
db$status1=str_replace_all(db$status1,"\\{","")

db[db$status1=="",]$status1="sem informação"


# Ano do deferimento ------------------------------------------------------

db$ano_deferimento=ifelse(str_detect(db$status2,"dataDeferimento"),as.numeric(substr(db$status3,unlist(gregexpr("\\(", db$status3))+1,unlist(gregexpr("\\(", db$status3))+4)),NA)
db$ano_deferimento=ifelse(db$ano_deferimento>1900 & db$ano_deferimento<=year(Sys.Date()),db$ano_deferimento,NA)

# Data da concessao -------------------------------------------------------

db$ano_concessao=ifelse(str_detect(db$status2,"dataConcessao"),as.numeric(substr(db$status3,unlist(gregexpr("\\(", db$status3))+1,unlist(gregexpr("\\(", db$status3))+4)),NA)
db$ano_concessao=ifelse(db$ano_concessao>1900 & db$ano_concessao<=year(Sys.Date()),db$ano_concessao,NA)


# Data indeferimento ------------------------------------------------------

db$ano_indeferimento=ifelse(str_detect(db$status2,"dataIndeferimento"),as.numeric(substr(db$status3,unlist(gregexpr("\\(", db$status3))+1,unlist(gregexpr("\\(", db$status3))+4)),NA)
db$ano_indeferimento=ifelse(db$ano_indeferimento>1900 & db$ano_indeferimento<=year(Sys.Date()),db$ano_indeferimento,NA)



# Criação classificação categorias IEA -------------------------------------------

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

db$sum=rowSums(db[,c(colnames(db)[str_detect(colnames(db),"cod")])])

db=db[which(db$sum>0),]
db$sum=NULL

#Nível 1
#db$iea1=ifelse(db$cod19==1 | db$cod20==1 | db$cod25==1| 
#                 db$cod113==1| db$cod114==1| db$cod115==1| 
#                 db$cod116==1| db$cod117==1| db$cod118==1| 
#                 db$cod119==1| db$cod120==1| db$cod121==1|
#                 db$cod74==1| db$cod87==1| db$cod88==1| 
#                 db$cod89==1| db$cod90==1| db$cod91==1| 
#                 db$cod92==1| db$cod93==1| db$cod94==1| 
#                 db$cod95==1| db$cod96==1| db$cod97==1| 
#                 db$cod98==1| db$cod99==1| db$cod100==1| 
#                 db$cod101==1| db$cod102==1| db$cod103==1| 
#                 db$cod104==1| db$cod105==1| db$cod106==1| 
#                 db$cod107==1| db$cod122==1| db$cod69==1| 
#                 db$cod70==1| db$cod71==1| db$cod72==1| 
#                 db$cod73==1| db$cod75==1| db$cod76==1| 
#                 db$cod77==1| db$cod78==1| db$cod79==1| 
#                 db$cod80==1| db$cod81==1| db$cod82==1| 
#                 db$cod83==1| db$cod84==1| db$cod85==1| 
#                 db$cod86==1| db$cod109==1| db$cod110==1| db$cod111==1,1,0)
#
#
#db$iea3=ifelse(db$cod39==1 |db$cod40==1 |db$cod41==1 |db$cod42==1 |db$cod43==1 |db$cod44==1 |db$cod45==1 |db$cod46==1 |
#                 db$cod47==1 |db$cod48==1 |db$cod49==1 |db$cod50==1 |db$cod51==1 |db$cod52==1 |db$cod53==1 |db$cod54==1 |
#                 db$cod55==1 |db$cod56==1 |db$cod57==1 |db$cod58==1 |db$cod59==1 |db$cod60==1 |db$cod61==1 |db$cod62==1 |
#                 db$cod63==1 |db$cod64==1 |db$cod33==1 | db$cod34==1 | db$cod35==1 | db$cod36==1 | db$cod37==1 | 
#                 db$cod38==1 |db$cod32==1 |db$cod1==1 | db$cod2==1 | db$cod3==1 | db$cod4==1 | db$cod5==1 | db$cod6==1 | 
#                 db$cod13==1 | db$cod14==1 | db$cod15==1 | db$cod16==1 | db$cod17==1 | db$cod18==1 | db$cod21==1 | 
#                 db$cod22==1 | db$cod23==1 | db$cod24==1 | db$cod26==1 | db$cod65==1 | db$cod66==1 | db$cod67==1 | 
#                 db$cod68==1 | db$cod27==1 | db$cod28==1 | db$cod29==1 | db$cod30==1 | db$cod31==1,1,0)
#
#db$iea4=ifelse(db$cod124==1 | db$cod125==1 | db$cod123==1 | db$cod126==1,1,0)
#
#db$iea5=ifelse(db$cod8==1 | db$cod9==1 | db$cod10==1 | db$cod11==1 | db$cod12==1,1,0)
#
#db$iea6=ifelse(db$cod127==1 | db$cod108==1 | db$cod112==1,1,0)

#db$nivel1=paste(db$iea1,db$iea3,db$iea4,db$iea5,db$iea6,sep = ",")
#db$nivel1=str_replace_all(db$nivel1,"0,","")
#db$nivel1=str_replace_all(db$nivel1,",0","")

#Nível 2

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

#db$sum=rowSums(db[,c(colnames(db)[str_detect(colnames(db),"iea")])])

#db$iea11=ifelse(db$iea11==1,1.1,0)
#db$iea12=ifelse(db$iea12==1,1.2,0)
#db$iea13=ifelse(db$iea13==1,1.3,0)
#db$iea14=ifelse(db$iea14==1,1.4,0)
#db$iea31=ifelse(db$iea31==1,3.1,0)
#db$iea32=ifelse(db$iea32==1,3.2,0)
#db$iea33=ifelse(db$iea33==1,3.3,0)
#db$iea34=ifelse(db$iea34==1,3.4,0)
#db$iea35=ifelse(db$iea35==1,3.5,0)
#db$iea36=ifelse(db$iea36==1,3.6,0)
#db$iea41=ifelse(db$iea41==1,4.1,0)
#db$iea42=ifelse(db$iea42==1,4.2,0)
#db$iea49=ifelse(db$iea49==1,4.9,0)
#db$iea52=ifelse(db$iea52==1,5.2,0)
#db$iea61=ifelse(db$iea61==1,6.1,0)
#db$iea63=ifelse(db$iea63==1,6.3,0)
#
#db$nivel2=paste(db$iea11,db$iea12,db$iea13,db$iea14,db$iea31,db$iea32,db$iea33,db$iea34,db$iea35,db$iea36,db$iea41,db$iea42,db$iea49,db$iea52,db$iea61,db$iea63,sep=",")
#db$nivel2=str_replace_all(db$nivel2,"0,","")
#db$nivel2=str_replace_all(db$nivel2,",0","")


# Tipo de inventor ------------------------------------------------------

db$depositantes2=str_replace_all(db$depositantes,"\\[","")
db$depositantes2=str_replace_all(db$depositantes2,"\\]","")
db$depositantes2=str_replace_all(db$depositantes2,"'","")
db$depositantes2=str_replace_all(db$depositantes2,'"',"")
db$depositantes2=sub(" \\(.*", "",db$depositantes2)

db$inventores2=str_replace_all(db$inventores,"\\[","")
db$inventores2=str_replace_all(db$inventores2,"\\]","")
db$inventores2=str_replace_all(db$inventores2,"'","")
db$inventores2=str_replace_all(db$inventores2,'"',"")
db=db %>% separate(inventores2,into="inventores2",sep=", ")

db$tp_pessoa=ifelse(db$depositantes2==db$inventores2,1,2)
db$tp_pessoa=ifelse(db$depositantes=="" | db$inventores2=="",9,db$tp_pessoa)

#View(db[,c("depositantes","depositantes2","inventores","inventores2","tp_pessoa")])


# Inventor feminino ------------------------------------------------------

db$inventores2=str_replace_all(db$inventores,"\\[","")
db$inventores2=str_replace_all(db$inventores2,"\\]","")
db$inventores2=str_replace_all(db$inventores2,"'","")
db$inventores2=str_replace_all(db$inventores2,'"',"")
db=db %>% separate(inventores2,into=c(paste0("invent_",seq(1,max(lengths(regmatches(db$inventores2, gregexpr(",", db$inventores2))))))),sep=", ")

for(i in colnames(db)[str_detect(colnames(db),"invent_")]){
  db[,i]=sub(" .*", "",db[,i])
}

for(i in colnames(db)[str_detect(colnames(db),"invent_")]){
  db[which(db$brasil==1),i]=ifelse(is.na(db[which(db$brasil==1),i])==F,get_gender(db[which(db$brasil==1),i]),db[which(db$brasil==1),i])
  db[which(db$brasil!=1),i]=ifelse(is.na(db[which(db$brasil!=1),i])==F,gender(db[which(db$brasil!=1),i])$gender,db[which(db$brasil!=1),i])
}


db$feminino=+(apply(db[,colnames(db)[str_detect(colnames(db),"invent_")]]=="female" | 
                      db[,colnames(db)[str_detect(colnames(db),"invent_")]]=="Female",1,any))
db$feminino=ifelse(is.na(db$feminino) & db$inventores!="",0,db$feminino)
db$feminino=ifelse(is.na(db$feminino),9,db$feminino)



# Database Final ----------------------------------------------------------
db$ano_pedido=db$ano
vars=c("numeroBusca","brasil","cooperacao","status1","pais","ano_pedido","ano_deferimento","ano_concessao","ano_indeferimento",
       colnames(db)[str_detect(colnames(db),"iea")],"tp_pessoa","feminino")

db2=db[,vars]
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


america=c("AI","AG","AR","AW","BS","BB","BZ","BM","BO","XA","VG","BR","CA","KY","CL","CO","CR",
          "CU","CW","DM","DO","EC","SV","FK","GF","GD","GP","GT","GY","HT","HN","JM","MQ","MX",
          "MS","NI","PA","PY","PE","PR","XC","BL","XB","KN","LC","MF","VC","PM","SX","SR","TT",
          "TC","US","UY","VE","VI")

db2$america=ifelse(str_detect(db2$pais,paste(america,collapse = "|")) & db2$brasil!=9,1,0)

db2$variable=NULL
write.csv(db2,"database.csv",row.names = F)



# Choices - Categoria nivel 1 e 2 -----------------------------------------

cat=db2[duplicated(db2$variable)==F,c("nivel1","variable")]
names(cat)=c("nivel1","nivel2")

cat$nivel2=factor(cat$nivel2,levels = unique(cat$nivel2),labels=c("Tecnologias de Eficiência Energética aplicadas à Industria",
                                                                  "Tecnologias de Eficiência Energética aplicada a residências e estabelecimentos comerciais",
                                                                  "Tecnologias de Eficiência Energética aplicadas ao setor de transporte rodoviário",
                                                                  "Outras Tecnologias de Eficiência Energética","Energia solar","Energia Eólica",
                                                                  "Energia dos Oceanos","Biocombustíveis","Energia Geotérmica","Hidroeletricidade",
                                                                  "Fissão Nuclear","Fusão Nuclear","Outros fusão e fissão não alocados","Células a Combustível",
                                                                  "Outras Tecnologias de Geração","Armazenamento de Energia"))

cat$nivel1=ifelse(cat$nivel1=="iea1","Eficiência Energética",
                  ifelse(cat$nivel1=="iea2","Energias Fósseis: Petróleo, Gás Natural e Carvão Mineral",
                         ifelse(cat$nivel1=="iea3","Fontes de Energia Renováveis",
                                ifelse(cat$nivel1=="iea4","Fissão e Fusão Nuclear",
                                       ifelse(cat$nivel1=="iea5","Hidrogênio e Células a Combustível",
                                              ifelse(cat$nivel1=="iea6","Outras Tecnologias de Geração e Armazenamento de Energia",
                                                     ifelse(cat$nivel1=="iea7","Outras Tecnologias e Pesquisas Transversais",NA)))))))

write.csv(cat,"categorias_iea.csv",row.names = F)
