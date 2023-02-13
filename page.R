source("tab_evolucao.R")
source("tab_categoria.R")
source("tab_status.R")
source("tab_depositante1.R")
source("tab_depositante2.R")
source("tab_inventor.R")
source("tab_cooperacao.R")
source("tab_explorar.R")
source("countries.R")

setwd("/Users/silvanooliveira/Google Drive/Meu Drive/Consultoria/CEPAL/painel/")

cat=read.csv("categorias_iea.csv")

page <- dashboardBody(
  fluidPage(
    fluidRow(
      h1(htmlOutput("title")),
      hr(),
      column(width = 3,
        h4(pickerInput("nivel1","Tecnologia energética nível 1",choices = unique(cat$label1),
                       options = list(`actions-box` = TRUE,`deselect-all-text` = "Desmarcar todas",`select-all-text` = "Marcar todas",size = 10,`selected-text-format` = "count > 3"),multiple = T,selected = unique(cat$label1)))
      ),
      column(width = 3,
        h4(pickerInput("nivel2","Tecnologia energética nível 2",choices = unique(cat$label2),
                       options = list(`actions-box` = TRUE,`deselect-all-text` = "Desmarcar todas",`select-all-text` = "Marcar todas",size = 10,`selected-text-format` = "count > 3"),multiple = T,selected = unique(cat$label2)))  
      ),
      column(width = 2,
        h4(pickerInput("tp_ano","Ano",choices = c("Pedido"="pedido","Concessão"="concessão","Deferimento"="deferimento","Indeferimento"="indeferimento"),
                       options = list(`actions-box` = TRUE,`deselect-all-text` = "Desmarcar todas",`select-all-text` = "Marcar todas",size = 10,`selected-text-format` = "count > 3"),multiple = F,selected = "pedido"))
      ),
      column(width = 2,
        h4(pickerInput("status","Situação",choices = c("Deferida"="deferida","Concedida"="concedida",
                                                       "Publicada"="publicada","PCT"="PCT",
                                                       "Indevida"="indeferida","Oferta"="oferta",
                                                       "SubJudice"="subJudice","Exigência pedido"="exigenciaPedido",
                                                       "Anulada"="anulada","Sem informação"="sem informação"),
                       options = list(`actions-box` = TRUE,`deselect-all-text` = "Desmarcar todas",`select-all-text` = "Marcar todas",size = 10,`selected-text-format` = "count > 3"),multiple = T,selected = c("deferida","concedida","publicada")))
      ),
      column(width = 2,
        conditionalPanel(condition = "input.tab!='colaboracao'",
          h4(pickerInput("nacionalidade","Origem",choices = countries,
                         options = list(`actions-box` = TRUE,`deselect-all-text` = "Desmarcar todas",`select-all-text` = "Marcar todas",size = 10,`selected-text-format` = "count > 3"),multiple = T,selected = countries))
        )
      )
    ),
    fluidRow(
      column(width = 6,
        setSliderColor("Teal", 1),
        h4(sliderInput("date","Período",min=2000,max=year(Sys.Date()),value = c(2010,2020),sep=""))
      ),
      column(width = 6,
             h4(
               conditionalPanel(condition = "input.tab=='evolucao'",
                                radioGroupButtons(
                                  inputId = "tp_plot",
                                  label = NULL,
                                  choiceNames = list(icon("chart-column"),icon("chart-line"),icon("table")),
                                  choiceValues = c("Barras", "Linhas", "Tabela"),
                                  status = "primary"
                                )
               ),
               conditionalPanel(condition = "input.tab!='evolucao' & input.tab!='explorar'",
                                radioGroupButtons(
                                  inputId = "tp_plot1",
                                  label = NULL,
                                  choiceNames = list(icon("chart-column"),icon("chart-line"),icon("chart-pie"),icon("table")),
                                  choiceValues = c("Barras", "Linhas","Setor", "Tabela"),
                                  status = "primary"
                                )
               ),align="right"
             )
      )
    ),
    tabItems(
      evolucao,
      categoria,
      status,
      localizacao,
      tp_pessoa,
      inventor,
      cooperacao,
      explorar
    )
  )
)
