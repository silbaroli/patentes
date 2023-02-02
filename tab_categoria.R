

categoria<-tabItem(tabName = "categoria",
  box(width = 12,
    column(width = 5,
      conditionalPanel(condition = "input.tp_plot1=='Barras'",
        h5(radioButtons("select2","Indicador",choices = c("Número absoluto","Proporção"),inline = T))
      )
    ),
    column(width = 5,
      h5(radioButtons("nivel","Agrupamento",choices = c("Nível 1","Nível 2"),inline = T))
    ),
    column(width = 2,align="right",
      h4(downloadButton("data2", label = NULL, class = NULL,icon = icon("download")))
    ),
    fluidRow(
      box(width = 12,
        h4(htmlOutput("title_plot2", align = "center")),
        conditionalPanel(condition = "input.tp_plot1!='Tabela'",
          withSpinner(plotlyOutput("plot2", width = 'auto', height = 600), type = 2)
        ),
        conditionalPanel(condition = "input.tp_plot1=='Tabela'",
          div(style = "overflow:scroll;header:fixed", DT::dataTableOutput("tab2",height=400))
        )
      )
    )
  )
)




            
            
            
            

                  
                  