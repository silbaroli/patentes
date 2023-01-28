depositante<-tabItem(tabName = "depositante",
  box(width = 12,
    conditionalPanel(condition = "input.tp_plot1=='Barras'",
      column(width = 6,
        h5(radioButtons("select4","Indicador",choices = c("Número absoluto","Proporção"),inline = T),align="left")
      )
    ),
    column(width = 6,
      h5(radioButtons("local","Localização",choices = c("América","Brasil"),inline = T,selected = "Brasil"),align="left")
    ),
    fluidRow(
      box(width = 12,
        h4(htmlOutput("title_plot4", align = "center")),
        withSpinner(plotlyOutput("plot4", width = 'auto', height = 600), type = 2)
      )
    )
  )
)