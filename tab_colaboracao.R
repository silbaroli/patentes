colaboracao<-tabItem(tabName = "colaboracao",
  box(width = 12,
    conditionalPanel(condition = "input.tp_plot1=='Barras'",
      column(width = 6,
        h5(radioButtons("select6","Indicador",choices = c("Número absoluto","Proporção"),inline = T),align="left")
      )
    ),
    column(width = 6,align="right",
      h4(downloadButton("data6", label = NULL, class = NULL,icon = icon("download")))
    ),
    fluidRow(
      box(width = 12,
        h4(htmlOutput("title_plot6", align = "center")),
        withSpinner(plotlyOutput("plot6", width = 'auto', height = 600), type = 2)
      )
    )
  )
)