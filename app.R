library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjs)
library(DT)

library(ggplot2)
library(plotly)
library(stringr)
library(gtable)
library(gridExtra)
library(lubridate)
library(dplyr)
library(forecast)

setwd("~/Google Drive/Meu Drive/Consultoria/CEPAL/painel/")

source("sidebar.R")
source("header.R")
source("page.R")


ui <- dashboardPage(header,sidebar,page)

server <- function(input, output, session) {
  
  observeEvent(input$nivel1, {
    cat=cat %>% filter(nivel1 %in% c(input$nivel1))
    
    updatePickerInput(session = session, inputId = "nivel2",
                      choices = unique(cat$nivel2))
    
  }, ignoreInit = TRUE)
  
  database <- reactive({
    df=read.csv("database.csv")
    
    df$ano=switch(input$tp_ano,
      "Pedido" = df$ano_pedido,
      "Concessão" = df$ano_concessao,
      "Deferimento" = df$ano_deferimento,
      "Indeferimento" = df$ano_indeferimento
    )
    
    
    df=df[which(df$ano>=min(input$date) & df$ano<=max(input$date)),]
    df=df %>% filter(brasil %in% c(input$nacionalidade))
    df=df %>% filter(status1 %in% c(input$status))
    df=df %>% filter(nivel2 %in% c(input$nivel2))
    
  })
  
  output$title <- renderUI({
    
    if(input$tab=="evolucao"){
      htmlText = paste("<div style='margin-left: 0.2px'!important;>","Evolução temporal dos pedidos de patentes","</div>")
    } else if(input$tab=="categoria"){
      htmlText = paste("<div style='margin-left: 0.2px'!important;>","Pedidos de patentes por categorias","</div>")
    } else if(input$tab=="status"){
      htmlText = paste("<div style='margin-left: 0.2px'!important;>","Situação atual dos pedidos de patentes","</div>")
    } else if(input$tab=="depositante"){
      htmlText = paste("<div style='margin-left: 0.2px'!important;>","Perfil dos depositantes","</div>")
    } else if(input$tab=="inventor"){
      htmlText = paste("<div style='margin-left: 0.2px'!important;>","Perfil dos inventores","</div>")
    } else if(input$tab=="colaboracao"){
      htmlText = paste("<div style='margin-left: 0.2px'!important;>","Pedidos de patentes com cooperação internacional","</div>")
    }
    
    HTML(htmlText)
  })
  
  output$title_plot1 <- renderUI({
    htmlText = paste0("Número de patentes depositadas por ano", ifelse(input$tp_ano=="Concessão"," da "," do "),input$tp_ano,", ", min(input$date)," a ",max(input$date))
    
    HTML(htmlText)
  })
  
  output$title_plot2 <- renderUI({
    if(input$tp_plot1!="Setor"){
      htmlText = paste0("Número de patentes depositadas segundo classificação tecnológica por ano", ifelse(input$tp_ano=="Concessão"," da "," do "),input$tp_ano," ,",min(input$date)," a ",max(input$date))
    } else {
      htmlText = paste0("Distribuição proporcional das patentes depositadas segundo classificação tecnológica, ",min(input$date)," a ",max(input$date))
    }
    
    HTML(htmlText)
  })
  
  output$title_plot3 <- renderUI({
    if(input$tp_plot1!="Setor"){
      htmlText = paste0("Número de patentes depositadas segundo status por ano",ifelse(input$tp_ano=="Concessão"," da "," do "),input$tp_ano,", ",min(input$date)," a ",max(input$date))
    } else{
      htmlText = paste0("Distribuição proporcional das patentes depositadas segundo status, ",min(input$date)," a ",max(input$date))
    }
    HTML(htmlText)
  })
  
  output$title_plot4 <- renderUI({
    if(input$tp_plot1=="Barras" | input$tp_plot1=="Linhas"){
      htmlText = paste0(ifelse(input$select4=="Número absoluto","Número de patentes ","Distribuição proporcional das patentes "),
                        "depositadas por inventores com nacionalidade ",
                        ifelse(input$local=="Brasil","brasileira","de algum país das Américas")," por ano ",
                        ifelse(input$tp_ano=="Concessão","da ","do "),input$tp_ano,", ",min(input$date)," a ",max(input$date))
    } else if(input$tp_plot1=="Tabela"){
      htmlText = paste0("Número de patentes depositadas por inventores com nacionalidade ",
                        ifelse(input$local=="Brasil","brasileira","de algum país das Américas")," por ano ",
                        ifelse(input$tp_ano=="Concessão","da ","do "),input$tp_ano,", ",min(input$date)," a ",max(input$date))
    } else if(input$tp_plot1=="Setor"){
      htmlText = paste0("Distribuição proporcional das patentes depositadas por inventores com nacionalidade ",
                        ifelse(input$local=="Brasil","brasileira","de algum país das Américas"),", ",min(input$date)," a ",max(input$date))
    }
    
    HTML(htmlText)
  })
  
  output$title_plot5 <- renderUI({
    if(input$tp_plot1=="Barras" | input$tp_plot1=="Linhas"){
      htmlText = paste0(ifelse(input$select5=="Proporção",
                               "Distribuição proporcional das patentes depositadas segundo presença feminina entre os inventores por ano ",
                               "Número de patentes depositadas com algum inventor do sexo feminino por ano "),
                        ifelse(input$tp_ano=="Concessão","da ","do "),input$tp_ano,", ",min(input$date)," a ",max(input$date))
    } else if(input$tp_plot1=="Tabela"){
      htmlText = paste0("Número de patentes depositadas com algum inventor do sexo feminino por ano ",
                        ifelse(input$tp_ano=="Concessão","da ","do "),input$tp_ano,", ",min(input$date)," a ",max(input$date))
    } else if(input$tp_plot1=="Setor"){
      htmlText = paste0("Distribuição proporcional das patentes depositadas segundo presença feminina entre os inventores, ",min(input$date)," a ",max(input$date))
    }
    
    HTML(htmlText)
  })
  
  output$title_plot6 <- renderUI({
    if(input$tp_plot1=="Barras" | input$tp_plot1=="Linhas"){
      htmlText = paste0(ifelse(input$select6=="Proporção",
                               "Distribuição proporcional das patentes de inventores brasileiros segundo cooperação internacional por ano ",
                               "Número de patentes depositadas  por inventores brasileiros com cooperação internacional por ano "),
                        ifelse(input$tp_ano=="Concessão","da ","do "),input$tp_ano,", ",min(input$date)," a ",max(input$date))
    } else if(input$tp_plot1=="Tabela"){
      htmlText = paste0("Número de patentes depositadas por inventores brasileiros com cooperação internacional por ano ",
                        ifelse(input$tp_ano=="Concessão","da ","do "),input$tp_ano,", ",min(input$date)," a ",max(input$date))
    } else if(input$tp_plot1=="Setor"){
      htmlText = paste0("Distribuição proporcional das patentes de inventores brasileiros segundo cooperação internacional, ",min(input$date)," a ",max(input$date))
    }
    
    HTML(htmlText)
  })
  
  output$plot1 <- renderPlotly({
    mrg <- list(l = 50, r = 50,
                b = 50, t = 10,
                pad = 5)
    
    db=database()
    db=db[which(duplicated(db$numeroBusca)==F),]
    
    db1=aggregate(db$count,list(db$ano),sum,na.rm=T)
    names(db1)=c("date","count")
    
    xTitle=switch (input$tp_ano,
      "Pedido" = "Ano do pedido",
      "Concessão" = "Ano da concessão",
      "Deferimento" = "Ano do deferimento",
      "Indeferimento" = "Ano do indeferimento"
    )
    
    
    yTitle="Número de patentes"
    
    if(input$tp_plot=="Barras"){
      plot_ly(db1, x = ~date, y = ~count, type = 'bar',marker = list(color = 'rgb(0,82,102)')) %>%
        layout(xaxis = list(title = xTitle, showgrid = FALSE),
               yaxis = list(title = yTitle, showgrid = FALSE, showline = T, linewidth = 1.1, linecolor = 'black'), barmode = 'stack') %>%
        layout(xaxis = list(type = 'date',tickformat = "%Y"), margin = mrg, font = list(family = "Calibri", size = 14)) %>% 
        layout(yaxis = list(tickformat = ".0f"))
    } else if(input$tp_plot=="Linhas"){
      plot_ly(db1, x = ~date, y = ~count, type = 'scatter', mode = 'lines',line = list(color = 'rgb(0,82,102)')) %>%
        layout(xaxis = list(title = xTitle, showgrid = FALSE, showline = T, linewidth = 1.1, linecolor = 'black'),
               yaxis = list(title = yTitle, showgrid = FALSE, showline = T, linewidth = 1.1, linecolor = 'black')) %>%
        layout(xaxis = list(type = 'date',tickformat = "%Y"), margin = mrg, font = list(family = "Calibri", size = 14)) %>% 
        layout(yaxis = list(tickformat = ".0f"))
    }
  })
  
  output$plot2 <- renderPlotly({
    mrg <- list(l = 50, r = 50,
                b = 50, t = 10,
                pad = 5)
    
    db=database()
    
    db$classif=switch(input$nivel,
      "Nível 1" = db$nivel1,
      "Nível 2" = db$nivel2
    )
    
    colors=switch (input$nivel,
      "Nível 1" = c("Eficiência Energética"="#E49B64","Fontes de Energia Renováveis"="#498399",
                    "Fissão e Fusão Nuclear"="#8A3D39","Hidrogênio e Células a Combustível"="#B0A3C4",
                    "Outras Tecnologias de Geração e Armazenamento de Energia"="#2B651F"),
      
      "Nível 2" = c("Tecnologias de Eficiência Energética aplicadas à Industria"="#2f1335",                             
                    "Tecnologias de Eficiência Energética aplicada a residências e estabelecimentos comerciais"="#a6cee3",
                    "Tecnologias de Eficiência Energética aplicadas ao setor de transporte rodoviário"="#1f78b4",
                    "Outras Tecnologias de Eficiência Energética"="#b2df8a",
                    "Energia solar"="#33a02c",
                    "Energia Eólica"="#fb9a99",
                    "Energia dos Oceanos"="#e31a1c",
                    "Biocombustíveis"="#5c1b35",
                    "Energia Geotérmica"="#fdbf6f",
                    "Hidroeletricidade"="#cab2d6",
                    "Fissão Nuclear"="#6a3d9a",
                    "Fusão Nuclear"="#c59538",
                    "Outros fusão e fissão não alocados"="#ffff99",
                    "Células a Combustível"="#b15928",
                    "Outras Tecnologias de Geração"="#fddf2f",
                    "Armazenamento de Energia"="#71dbd2")
    )
    
    
       
    
    db1=aggregate(db$value,list(db$ano,db$classif),sum,na.rm=T)
    names(db1)=c("date","cat","count")
    
    db1$per=NA
    for(i in unique(db1$date)){
      db1[db1$date==i,]$per=round(db1[db1$date==i,]$count/sum(db1[db1$date==i,]$count)*100,1)
    }
    
    xTitle=switch (input$tp_ano,
                   "Pedido" = "Ano do pedido",
                   "Concessão" = "Ano da concessão",
                   "Deferimento" = "Ano do deferimento",
                   "Indeferimento" = "Ano do indeferimento"
    )
    
    if(input$tp_plot1=="Barras"){
      if(input$select2=="Número absoluto"){
        
        
        yTitle="Número de patentes"
        
        plot_ly(db1, x = ~date, y = ~count,color = ~cat, type = 'bar',colors = colors) %>%
          layout(xaxis = list(title = xTitle, showgrid = FALSE),
                 yaxis = list(title = yTitle, showgrid = FALSE, showline = T, linewidth = 1.1, linecolor = 'black'), barmode = 'stack') %>%
          layout(legend = list(orientation = "h")) %>%
          layout(xaxis = list(type = 'date',tickformat = "%Y"), margin = mrg, font = list(family = "Calibri", size = 14)) %>% 
          layout(yaxis = list(tickformat = ".0f"))
      } else if(input$select2=="Proporção"){
        yTitle="Proporção"
        plot_ly(db1, x = ~date, y = ~per,color = ~cat, type = 'bar',colors = colors) %>%
          layout(xaxis = list(title = xTitle, showgrid = FALSE),
                 yaxis = list(title = yTitle, showgrid = FALSE, showline = T, linewidth = 1.1, linecolor = 'black'), barmode = 'stack') %>%
          layout(legend = list(orientation = "h")) %>%
          layout(xaxis = list(type = 'date',tickformat = "%Y"), margin = mrg, font = list(family = "Calibri", size = 14)) %>% 
          layout(yaxis = list(tickformat = ".0f"))
      }
    } else if(input$tp_plot1=="Linhas"){
      yTitle="Número de patentes"
      
      plot_ly(db1, x = ~date, y = ~count, color= ~cat,type = 'scatter', mode = 'lines',colors = colors) %>%
        layout(xaxis = list(title = xTitle, showgrid = FALSE, showline = T, linewidth = 1.1, linecolor = 'black'),
               yaxis = list(title = yTitle, showgrid = FALSE, showline = T, linewidth = 1.1, linecolor = 'black')) %>%
        layout(legend = list(orientation = "h")) %>%
        layout(xaxis = list(type = 'date',tickformat = "%Y"), margin = mrg, font = list(family = "Calibri", size = 14)) %>% 
        layout(yaxis = list(tickformat = ".0f"))
    } else if(input$tp_plot1=="Setor"){
      
      plot_ly(data=db,labels = ~ classif, values = ~value) %>% 
        add_pie(hole = 0.6)%>% 
        layout(showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
  })
  
  output$plot3 <- renderPlotly({
    mrg <- list(l = 50, r = 50,
                b = 50, t = 10,
                pad = 5)
    
    db=database()
    db=db[which(duplicated(db$numeroBusca)==F),]
    
    db1=aggregate(db$value,list(db$ano,db$status1),sum,na.rm=T)
    names(db1)=c("date","cat","count")
    
    db1$per=NA
    for(i in unique(db1$date)){
      db1[db1$date==i,]$per=round(db1[db1$date==i,]$count/sum(db1[db1$date==i,]$count)*100,1)
    }
    
    colors=c("deferida"="#a6cee3","concedida"="#1f78b4","publicada"="#b2df8a","PCT"="#33a02c",
             "indeferida"="#fb9a99","oferta"="#e31a1c","subJudice"="#fdbf6f","exigenciaPedido"="#ff7f00",
             "anulada"="#cab2d6","sem informação"="#999999")
    
    xTitle=switch (input$tp_ano,
                   "Pedido" = "Ano do pedido",
                   "Concessão" = "Ano da concessão",
                   "Deferimento" = "Ano do deferimento",
                   "Indeferimento" = "Ano do indeferimento"
    )
    
    if(input$tp_plot1=="Barras"){
      if(input$select3=="Número absoluto"){
        yTitle="Número de patentes"
        
        plot_ly(db1, x = ~date, y = ~count,color = ~cat, type = 'bar',colors=colors) %>%
          layout(xaxis = list(title = xTitle, showgrid = FALSE),
                 yaxis = list(title = yTitle, showgrid = FALSE, showline = T, linewidth = 1.1, linecolor = 'black'), barmode = 'stack') %>%
          layout(legend = list(orientation = "h")) %>%
          layout(xaxis = list(type = 'date',tickformat = "%Y"), margin = mrg, font = list(family = "Calibri", size = 14)) %>% 
          layout(yaxis = list(tickformat = ".0f"))
      } else if(input$select3=="Proporção"){
        yTitle="Proporção"
        
        plot_ly(db1, x = ~date, y = ~per,color = ~cat, type = 'bar',colors=colors) %>%
          layout(xaxis = list(title = xTitle, showgrid = FALSE),
                 yaxis = list(title = yTitle, showgrid = FALSE, showline = T, linewidth = 1.1, linecolor = 'black'), barmode = 'stack') %>%
          layout(legend = list(orientation = "h")) %>%
          layout(xaxis = list(type = 'date',tickformat = "%Y"), margin = mrg, font = list(family = "Calibri", size = 14)) %>% 
          layout(yaxis = list(tickformat = ".0f"))
      }
    } else if(input$tp_plot1=="Linhas"){
      yTitle="Número de patentes"
      
      plot_ly(db1, x = ~date, y = ~count, color= ~cat,type = 'scatter', mode = 'lines',colors=colors) %>%
        layout(xaxis = list(title = xTitle, showgrid = FALSE, showline = T, linewidth = 1.1, linecolor = 'black'),
               yaxis = list(title = yTitle, showgrid = FALSE, showline = T, linewidth = 1.1, linecolor = 'black')) %>%
        layout(legend = list(orientation = "h")) %>%
        layout(xaxis = list(type = 'date',tickformat = "%Y"), margin = mrg, font = list(family = "Calibri", size = 14)) %>% 
        layout(yaxis = list(tickformat = ".0f"))
    } else if(input$tp_plot1=="Setor"){
      plot_ly(data=db,labels = ~ status1, values = ~value) %>% 
        add_pie(hole = 0.6)%>% 
        layout(showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
  })
  
  output$plot4 <- renderPlotly({
    mrg <- list(l = 50, r = 50,
                b = 50, t = 10,
                pad = 5)
    
    db=database()
    db=db[which(duplicated(db$numeroBusca)==F),]
    
    db$cat=switch(input$local,
      "Brasil" = db$brasil,
      "América" = db$america
    )
    
    db1=aggregate(db$value,list(db$ano,db$cat),sum,na.rm=T)
    names(db1)=c("date","cat","count")
    
    db1$per=NA
    for(i in unique(db1$date)){
      db1[db1$date==i,]$per=round(db1[db1$date==i,]$count/sum(db1[db1$date==i,]$count)*100,1)
    }
    
    xTitle=switch (input$tp_ano,
                   "Pedido" = "Ano do pedido",
                   "Concessão" = "Ano da concessão",
                   "Deferimento" = "Ano do deferimento",
                   "Indeferimento" = "Ano do indeferimento"
    )
    
    if(input$tp_plot1=="Barras"){
      if(input$select4=="Número absoluto"){
        yTitle="Número de patentes"
        
        plot_ly(db1[which(db1$cat==1),], x = ~date, y = ~count, type = 'bar',marker = list(color = 'rgb(0,82,102)')) %>%
          layout(xaxis = list(title = xTitle, showgrid = FALSE),
                 yaxis = list(title = yTitle, showgrid = FALSE, showline = T, linewidth = 1.1, linecolor = 'black'), barmode = 'stack') %>%
          layout(xaxis = list(type = 'date',tickformat = "%Y"), margin = mrg, font = list(family = "Calibri", size = 14)) %>% 
          layout(yaxis = list(tickformat = ".0f"))
      } else if(input$select4=="Proporção"){
        
        db1$cat=factor(db1$cat,levels=c(1,0,9),labels=c(
          ifelse(input$local=="Brasil","Brasil","América"),
          ifelse(input$local=="Brasil","Outros países","Outros continentes"),
          "Sem informação de país"
        ))
        
        yTitle="Proporção"
        
        plot_ly(db1, x = ~date, y = ~per, type = 'bar',color = ~cat) %>%
          layout(xaxis = list(title = xTitle, showgrid = FALSE),
                 yaxis = list(title = yTitle, showgrid = FALSE, showline = T, linewidth = 1.1, linecolor = 'black'), barmode = 'stack') %>%
          layout(legend = list(orientation = "h")) %>%
          layout(xaxis = list(type = 'date',tickformat = "%Y"), margin = mrg, font = list(family = "Calibri", size = 14)) %>% 
          layout(yaxis = list(tickformat = ".0f%"))
      } 
    } else if(input$tp_plot1=="Linhas"){
      yTitle="Número de patentes"
      
      plot_ly(db1[which(db1$cat==1),], x = ~date, y = ~count, type = 'scatter', mode = 'lines',line = list(color = 'rgb(0,82,102)')) %>%
        layout(xaxis = list(title = xTitle, showgrid = FALSE, showline = T, linewidth = 1.1, linecolor = 'black'),
               yaxis = list(title = yTitle, showgrid = FALSE, showline = T, linewidth = 1.1, linecolor = 'black')) %>%
        layout(xaxis = list(type = 'date',tickformat = "%Y"), margin = mrg, font = list(family = "Calibri", size = 14)) %>% 
        layout(yaxis = list(tickformat = ".0f"))
    } else if(input$tp_plot1=="Setor"){
      db1$cat=factor(db1$cat,levels=c(1,0,9),labels=c(
        ifelse(input$local=="Brasil","Brasil","América"),
        ifelse(input$local=="Brasil","Outros países","Outros continentes"),
        "Sem informação de país"
      ))
      
      plot_ly(data=db1,labels = ~ cat, values = ~count) %>% 
        add_pie(hole = 0.6)%>% 
        layout(showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      }
  })
  
  output$plot5 <- renderPlotly({
    mrg <- list(l = 50, r = 50,
                b = 50, t = 10,
                pad = 5)
    
    db=database()
    db=db[which(duplicated(db$numeroBusca)==F),]
    
    db1=aggregate(db$value,list(db$ano,db$feminino),sum,na.rm=T)
    names(db1)=c("date","cat","count")
    
    db1$per=NA
    for(i in unique(db1$date)){
      db1[db1$date==i,]$per=round(db1[db1$date==i,]$count/sum(db1[db1$date==i,]$count)*100,1)
    }
    
    xTitle=switch (input$tp_ano,
                   "Pedido" = "Ano do pedido",
                   "Concessão" = "Ano da concessão",
                   "Deferimento" = "Ano do deferimento",
                   "Indeferimento" = "Ano do indeferimento"
    )
    
    if(input$tp_plot1=="Barras"){
      if(input$select5=="Número absoluto"){
        yTitle="Número de patentes"
        
        plot_ly(db1[which(db1$cat==1),], x = ~date, y = ~count, type = 'bar',marker = list(color = 'rgb(0,82,102)')) %>%
          layout(xaxis = list(title = xTitle, showgrid = FALSE),
                 yaxis = list(title = yTitle, showgrid = FALSE, showline = T, linewidth = 1.1, linecolor = 'black'), barmode = 'stack') %>%
          layout(xaxis = list(type = 'date',tickformat = "%Y"), margin = mrg, font = list(family = "Calibri", size = 14)) %>% 
          layout(yaxis = list(tickformat = ".0f"))
      } else if(input$select5=="Proporção"){
        
        db1$cat=factor(db1$cat,levels=c(1,0,9),labels=c("Presença feminina","Ausência feminina","Sem informação de sexo"))
        
        yTitle="Proporção"
        
        plot_ly(db1, x = ~date, y = ~per, type = 'bar',color = ~cat) %>%
          layout(xaxis = list(title = xTitle, showgrid = FALSE),
                 yaxis = list(title = yTitle, showgrid = FALSE, showline = T, linewidth = 1.1, linecolor = 'black'), barmode = 'stack') %>%
          layout(legend = list(orientation = "h")) %>%
          layout(xaxis = list(type = 'date',tickformat = "%Y"), margin = mrg, font = list(family = "Calibri", size = 14)) %>% 
          layout(yaxis = list(tickformat = ".0f%"))
      } 
    } else if(input$tp_plot1=="Linhas"){
      yTitle="Número de patentes"
      
      plot_ly(db1[which(db1$cat==1),], x = ~date, y = ~count, type = 'scatter', mode = 'lines',line = list(color = 'rgb(0,82,102)')) %>%
        layout(xaxis = list(title = xTitle, showgrid = FALSE, showline = T, linewidth = 1.1, linecolor = 'black'),
               yaxis = list(title = yTitle, showgrid = FALSE, showline = T, linewidth = 1.1, linecolor = 'black')) %>%
        layout(xaxis = list(type = 'date',tickformat = "%Y"), margin = mrg, font = list(family = "Calibri", size = 14)) %>% 
        layout(yaxis = list(tickformat = ".0f"))
    } else if(input$tp_plot1=="Setor"){
      db1$cat=factor(db1$cat,levels=c(1,0,9),labels=c("Presença feminina","Ausência feminina","Sem informação de sexo"))
      
      plot_ly(data=db1,labels = ~ cat, values = ~count) %>% 
        add_pie(hole = 0.6)%>% 
        layout(showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
  })
  
  output$plot6 <- renderPlotly({
    mrg <- list(l = 50, r = 50,
                b = 50, t = 10,
                pad = 5)
    
    db=database()
    db=db[which(db$brasil==1),]
    
    db1=aggregate(db$value,list(db$ano,db$cooperacao),sum,na.rm=T)
    names(db1)=c("date","cat","count")
    
    db1$per=NA
    for(i in unique(db1$date)){
      db1[db1$date==i,]$per=round(db1[db1$date==i,]$count/sum(db1[db1$date==i,]$count)*100,1)
    }
    
    xTitle=switch (input$tp_ano,
                   "Pedido" = "Ano do pedido",
                   "Concessão" = "Ano da concessão",
                   "Deferimento" = "Ano do deferimento",
                   "Indeferimento" = "Ano do indeferimento"
    )
    
    if(input$tp_plot1=="Barras"){
      if(input$select6=="Número absoluto"){
        yTitle="Número de patentes"
        
        plot_ly(db1[which(db1$cat==1),], x = ~date, y = ~count, type = 'bar',marker = list(color = 'rgb(0,82,102)')) %>%
          layout(xaxis = list(title = xTitle, showgrid = FALSE),
                 yaxis = list(title = yTitle, showgrid = FALSE, showline = T, linewidth = 1.1, linecolor = 'black'), barmode = 'stack') %>%
          layout(xaxis = list(type = 'date',tickformat = "%Y"), margin = mrg, font = list(family = "Calibri", size = 14)) %>% 
          layout(yaxis = list(tickformat = ".0f"))
      } else if(input$select6=="Proporção"){
        
        db1$cat=factor(db1$cat,levels=c(1,0,9),labels=c("Cooperação internacional","Sem cooperação","Sem informação"))
        
        yTitle="Proporção"
        
        plot_ly(db1, x = ~date, y = ~per, type = 'bar',color = ~cat) %>%
          layout(xaxis = list(title = xTitle, showgrid = FALSE),
                 yaxis = list(title = yTitle, showgrid = FALSE, showline = T, linewidth = 1.1, linecolor = 'black'), barmode = 'stack') %>%
          layout(legend = list(orientation = "h")) %>%
          layout(xaxis = list(type = 'date',tickformat = "%Y"), margin = mrg, font = list(family = "Calibri", size = 14)) %>% 
          layout(yaxis = list(tickformat = ".0f%"))
      } 
    } else if(input$tp_plot1=="Linhas"){
      yTitle="Número de patentes"
      
      plot_ly(db1[which(db1$cat==1),], x = ~date, y = ~count, type = 'scatter', mode = 'lines',line = list(color = 'rgb(0,82,102)')) %>%
        layout(xaxis = list(title = xTitle, showgrid = FALSE, showline = T, linewidth = 1.1, linecolor = 'black'),
               yaxis = list(title = yTitle, showgrid = FALSE, showline = T, linewidth = 1.1, linecolor = 'black')) %>%
        layout(xaxis = list(type = 'date',tickformat = "%Y"), margin = mrg, font = list(family = "Calibri", size = 14)) %>% 
        layout(yaxis = list(tickformat = ".0f"))
    } else if(input$tp_plot1=="Setor"){
      db1$cat=factor(db1$cat,levels=c(1,0,9),labels=c("Cooperação internacional","Sem cooperação","Sem informação"))
      
      plot_ly(data=db1,labels = ~ cat, values = ~count) %>% 
        add_pie(hole = 0.6)%>% 
        layout(showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
  })
  
  output$tab1 <- DT::renderDataTable({
    db=database()
    db=db[which(duplicated(db$numeroBusca)==F),]
    
    
    tab=aggregate(db$count,list(db$ano),sum,na.rm=T)
    
    
    my.options <- list(autoWidth = FALSE,
                       searching = FALSE,
                       ordering = TRUE,
                       lengthChange = FALSE,
                       lengthMenu = FALSE,
                       pageLength = FALSE,
                       paging = FALSE,
                       info = FALSE,
                       buttons = c('copy', 'csv', 'excel', 'pdf'),
                       rowsGroup = list(0))
    
    header.style <- "th { font-family: 'Calibri'; font-size:16px ;font-weight: bold; color: white; background-color: #ACBED4;}"
    
    header.names <- c(paste0("Ano",ifelse(input$tp_ano=="Concessão"," da "," do "),input$tp_ano),"n")
    
    my.container <- withTags(table(
      style(type = "text/css", header.style),
      thead(
        tr(
          lapply(header.names, th, style = "text-align: center; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
        )
      )
    ))
    
    
    
    my.table <- datatable(tab, options = my.options, container = my.container, rownames = F, width = '100%', extensions = 'Buttons') %>%
      formatStyle(columns = c(2),
                  width = '100px',
                  fontFamily = "Calibri",
                  fontSize = "14px",
                  borderRightWidth = "1px",
                  borderRightStyle = "solid",
                  borderRightColor = "white",
                  borderBottomColor = "#ffffff",
                  borderBottomStyle = "solid",
                  borderBottomWidth = "1px",
                  borderCollapse = "collapse",
                  verticalAlign = "middle",
                  textAlign = "center",
                  wordWrap = "break-word") %>%
      formatStyle(columns = c(1),
                  width = '100px',
                  fontFamily = "Calibri",
                  fontSize = "14px",
                  borderRightWidth = "1px",
                  borderRightStyle = "solid",
                  borderRightColor = "white",
                  borderBottomColor = "#ffffff",
                  borderBottomStyle = "solid",
                  borderBottomWidth = "1px",
                  borderCollapse = "collapse",
                  verticalAlign = "middle",
                  textAlign = "left",
                  wordWrap = "break-word")
    
    
    print(my.table)
  })
  
}

shinyApp(ui, server)




