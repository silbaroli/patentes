library(devtools)
library(roxygen2)

setwd("/Users/silvanooliveira/Google Drive/Meu Drive/Consultoria/CEPAL/painel/pacotes/")
install("inpietl")

db$pais=inpietl::remove_caracter(db$paises)
table(db$pais)

?inpietl::hello()
