

categoria<-tabItem(tabName = "categoria",
  box(width = 12,
    conditionalPanel(condition = "input.tp_plot1=='Barras'",
      column(width = 6,
        h5(radioButtons("select2","Indicador",choices = c("Número absoluto","Proporção"),inline = T))
      )
    ),
    conditionalPanel(condition = "input.tp_plot1=='Barras' | input.tp_plot1=='Linhas' | input.tp_plot1=='Setor'",
      column(width = 6,
        h5(radioButtons("nivel","Agrupamento",choices = c("Nível 1","Nível 2"),inline = T))
      )
    ),
    fluidRow(
      box(width = 12,
        h4(htmlOutput("title_plot2", align = "center")),
        withSpinner(plotlyOutput("plot2", width = 'auto', height = 600), type = 2)
      )
    )
  )
)




            
            
            
            

                  
                  