

evolucao<-tabItem(tabName = "evolucao",
  box(width = 12,
    fluidRow(
      box(width = 12,
        h4(htmlOutput("title_plot1", align = "center")),
        conditionalPanel(condition = "input.tp_plot!='Tabela'",
          withSpinner(plotlyOutput("plot1", width = 'auto', height = 600), type = 2)
        ),
        conditionalPanel(condition = "input.tp_plot=='Tabela'",
          div(style = "overflow:scroll;header:fixed", DT::dataTableOutput("tab1",height=600))
        )
      )
    )
  )
)