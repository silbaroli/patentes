colaboracao<-tabItem(tabName = "colaboracao",
  box(width = 12,
    conditionalPanel(condition = "input.tp_plot1=='Barras'",
      h5(radioButtons("select6","Indicador",choices = c("Número absoluto","Proporção"),inline = T),align="left")
    ),
    fluidRow(
      box(width = 12,
        h4(htmlOutput("title_plot6", align = "center")),
        withSpinner(plotlyOutput("plot6", width = 'auto', height = 600), type = 2)
      )
    )
  )
)