
depositante<-tabItem(tabName = "depositante",
  box(width = 12,
    conditionalPanel(condition = "input.tp_plot1=='Barras'",
      column(width = 5,
        h5(radioButtons("select4","Indicador",choices = c("Número absoluto","Proporção"),inline = T),align="left")
      )
    ),
    column(width = 5,
      h5(radioButtons("local","Localização",choices = c("América","Brasil"),inline = T,selected = "Brasil"),align="left")
    ),
    column(width = 2,align="right",
      h4(downloadButton("data4", label = NULL, class = NULL,icon = icon("download")))
    ),
    fluidRow(
      box(width = 12,
        h4(htmlOutput("title_plot4", align = "center")),
        withSpinner(plotlyOutput("plot4", width = 'auto', height = 600), type = 2)
      )
    )
  )
)