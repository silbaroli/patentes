inventor<-tabItem(tabName = "inventor",
  box(width = 12,
    column(width = 6,
      conditionalPanel(condition = "input.tp_plot1=='Barras'",
        h5(radioButtons("select5","Indicador",choices = c("Número absoluto","Proporção"),inline = T),align="left")
      )
    ),
    column(width = 6,align="right",
        h4(downloadButton("data5", label = NULL, class = NULL,icon = icon("download")))
    ),
    fluidRow(
      box(width = 12,
        h4(htmlOutput("title_plot5", align = "center")),
        conditionalPanel(condition = "input.tp_plot1!='Tabela'",
          withSpinner(plotlyOutput("plot5", width = 'auto', height = 600), type = 2)
        ),
        conditionalPanel(condition = "input.tp_plot1=='Tabela'",
          div(style = "overflow:scroll;header:fixed", DT::dataTableOutput("tab5",height=400))
        )
      )
    )
  )
)