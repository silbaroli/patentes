inventor<-tabItem(tabName = "inventor",
  box(width = 12,
    conditionalPanel(condition = "input.tp_plot1=='Barras'",
      column(width = 6,
        h5(radioButtons("select5","Indicador",choices = c("Número absoluto","Proporção"),inline = T),align="left")
      )
    ),
    column(width = 6,align="right",
        h4(downloadButton("data5", label = NULL, class = NULL,icon = icon("download")))
    ),
    fluidRow(
      box(width = 12,
        h4(htmlOutput("title_plot5", align = "center")),
        withSpinner(plotlyOutput("plot5", width = 'auto', height = 600), type = 2)
      )
    )
  )
)