
tp_pessoa<-tabItem(tabName = "tp_pessoa",
  box(width = 12,
    column(width = 5,
      conditionalPanel(condition = "input.tp_plot1=='Barras'",
        h5(radioButtons("select4.2","Indicador",choices = c("Número absoluto","Proporção"),inline = T),align="left")
      )
    ),
    column(width = 7,align="right",
      h4(downloadButton("data4.2", label = NULL, class = NULL,icon = icon("download")))
    ),
    fluidRow(
      box(width = 12,
        h4(htmlOutput("title_plot4.2", align = "center")),
        conditionalPanel(condition = "input.tp_plot1!='Tabela'",
          withSpinner(plotlyOutput("plot4.2", width = 'auto', height = 600), type = 2)
        ),
        conditionalPanel(condition = "input.tp_plot1=='Tabela'",
          div(style = "overflow:scroll;header:fixed", DT::dataTableOutput("tab4.2",height=400))
        )
      )
    )
  )
)