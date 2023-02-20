library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjs)
#library(shinycustomloader)
library(DT)
library(ggplot2)
library(ggiraph)
library(plotly)
library(stringr)
library(lubridate)
library(dplyr)
library(reshape2)
library(openxlsx)


options(spinner.color = "#7197A4", spinner.color.background = "#ffffff", spinner.size = 2, shiny.reactlog=TRUE)


setwd("/Users/silvanooliveira/Google Drive/Meu Drive/Consultoria/CEPAL/painel/")
source("sidebar.R")
source("header.R")
source("page.R")

ui <- dashboardPage(header,sidebar,page)

server <- function(input, output, session) {
  
  observeEvent(input$nivel1, {
    cat<-cat %>% filter(label1 %in% c(input$nivel1))
    
    updatePickerInput(session = session, inputId = "nivel2",
                      choices = unique(cat$label2))
    
  }, ignoreInit = TRUE)
  
  observeEvent(input$tp_plot1,{
    req(input$tp_plot1=="Download")
    
    showModal(modalDialog(
      selectInput("format1","Formato:",c(".csv",".xlsx")),
      downloadButton("data1", label = "Download", class = NULL,icon = icon("download")),
      easyClose = TRUE,footer = NULL
    ))}
  )
  
  observeEvent(input$tp_plot2,{
    req(input$tp_plot2=="Download")
    
    showModal(modalDialog(
      selectInput("format2","Formato:",c(".csv",".xlsx")),
      downloadButton("data2", label = "Download", class = NULL,icon = icon("download")),
      easyClose = TRUE,footer = NULL
    ))}
  )
  
  observeEvent(input$tp_plot3,{
    req(input$tp_plot3=="Download")
    
    showModal(modalDialog(
      selectInput("format3","Formato:",c(".csv",".xlsx")),
      downloadButton("data3", label = "Download", class = NULL,icon = icon("download")),
      easyClose = TRUE,footer = NULL
    ))}
  )
  
  observeEvent(input$tp_plot41,{
    req(input$tp_plot41=="Download")

    showModal(modalDialog(
      selectInput("format4.1","Formato:",c(".csv",".xlsx")),
      downloadButton("data4.1", label = "Download", class = NULL,icon = icon("download")),
      easyClose = TRUE,footer = NULL
    ))}
  )
  
  observeEvent(input$tp_plot42,{
    req(input$tp_plot42=="Download")

    showModal(modalDialog(
      selectInput("format4.2","Formato:",c(".csv",".xlsx")),
      downloadButton("data4.2", label = "Download", class = NULL,icon = icon("download")),
      easyClose = TRUE,footer = NULL
    ))}
  )
  
  observeEvent(input$tp_plot5,{
    req(input$tp_plot5=="Download")
    
    showModal(modalDialog(
      selectInput("format5","Formato:",c(".csv",".xlsx")),
      downloadButton("data5", label = "Download", class = NULL,icon = icon("download")),
      easyClose = TRUE,footer = NULL
    ))}
  )
  
  observeEvent(input$tp_plot6,{
    req(input$tp_plot6=="Download")
    
    showModal(modalDialog(
      selectInput("format6","Formato:",c(".csv",".xlsx")),
      downloadButton("data6", label = "Download", class = NULL,icon = icon("download")),
      easyClose = TRUE,footer = NULL
    ))}
  )
  
  database <- reactive({
    df<-read.csv("data/database.csv")
    
    df <- rename(df,'Tecnologias de Eficiência Energética aplicadas à Industria'='iea11',
                'Tecnologias de Eficiência Energética aplicada a residências e estabelecimentos comerciais'='iea12',
                'Tecnologias de Eficiência Energética aplicadas ao setor de transporte rodoviário'='iea13',
                'Outras Tecnologias de Eficiência Energética'='iea14',
                'Energia solar'='iea31',
                'Energia Eólica'='iea32',
                'Energia dos Oceanos'='iea33',
                'Biocombustíveis'='iea34',
                'Energia Geotérmica'='iea35',
                'Hidroeletricidade'='iea36',
                'Fissão Nuclear'='iea41',
                'Fusão Nuclear'='iea42',
                'Outros fusão e fissão não alocados'='iea49',
                'Células a Combustível'='iea52',
                'Outras Tecnologias de Geração'='iea61',
                'Armazenamento de Energia'='iea63')
    
    if(length(input$nivel2)>1){
      df$count<-as.numeric(+(apply(df[,c(input$nivel2)]==1,1,any)))
    } else{
      df$count<-ifelse(df[,input$nivel2]==1,1,0)
    }
    
    df$ano<-switch(input$tp_ano,
      "pedido" = df$ano_pedido,
      "concessão" = df$ano_concessao,
      "deferimento" = df$ano_deferimento,
      "indeferimento" = df$ano_indeferimento
    )
    
    df<-df[which(df$ano>=min(input$date) & df$ano<=max(input$date)),]
    df<-df %>% filter(status1 %in% c(input$status))
    
    if(input$tp_origem=="Nacional"){
      df<-df %>% filter(str_detect(uf,paste(c(input$uf),collapse = "|")))
    } else if(input$tp_origem=="Internacional"){
      df<-df %>% filter(str_detect(pais,paste(c(input$nacionalidade),collapse = "|")))
    }

  })
  
  database2 <- reactive({
    df2<-read.csv("data/database2.csv")
    
    df2 <- rename(df2,'Tecnologias de Eficiência Energética aplicadas à Industria'='iea11',
                 'Tecnologias de Eficiência Energética aplicada a residências e estabelecimentos comerciais'='iea12',
                 'Tecnologias de Eficiência Energética aplicadas ao setor de transporte rodoviário'='iea13',
                 'Outras Tecnologias de Eficiência Energética'='iea14',
                 'Energia solar'='iea31',
                 'Energia Eólica'='iea32',
                 'Energia dos Oceanos'='iea33',
                 'Biocombustíveis'='iea34',
                 'Energia Geotérmica'='iea35',
                 'Hidroeletricidade'='iea36',
                 'Fissão Nuclear'='iea41',
                 'Fusão Nuclear'='iea42',
                 'Outros fusão e fissão não alocados'='iea49',
                 'Células a Combustível'='iea52',
                 'Outras Tecnologias de Geração'='iea61',
                 'Armazenamento de Energia'='iea63')
    
    if(length(input$nivel2)>1){
      df2$count<-as.numeric(+(apply(df2[,c(input$nivel2)]==1,1,any)))
    } else{
      df2$count<-ifelse(df2[,input$nivel2]==1,1,0)
    }
    
    df2$ano<-switch(input$tp_ano,
                   "pedido" = df2$ano_pedido,
                   "concessão" = df2$ano_concessao,
                   "deferimento" = df2$ano_deferimento,
                   "indeferimento" = df2$ano_indeferimento
    )
    
    df2<-df2[which(df2$ano>=min(input$date) & df2$ano<=max(input$date)),]
    df2<-df2 %>% filter(status1 %in% c(input$status))
    
    if(input$tp_origem=="Nacional"){
      df2<-df2 %>% filter(str_detect(uf,paste(c(input$uf),collapse = "|")))
    } else if(input$tp_origem=="Internacional"){
      df2<-df2 %>% filter(str_detect(pais,paste(c(input$nacionalidade),collapse = "|")))
    }
    
    
  })
  
  output$title <- renderUI({

    if(input$tab=="evolucao"){
      htmlText = paste("<div style='margin-left: 0.2px'!important;>","Evolução temporal dos pedidos de patentes","</div>")
    } else if(input$tab=="categoria"){
      htmlText = paste("<div style='margin-left: 0.2px'!important;>","Pedidos de patentes por classificação tecnológica","</div>")
    } else if(input$tab=="status"){
      htmlText = paste("<div style='margin-left: 0.2px'!important;>","Situação dos pedidos de patentes","</div>")
    } else if(input$tab=="origem" | input$tab=="tp_pessoa"){
      htmlText = paste("<div style='margin-left: 0.2px'!important;>","Perfil do depositante","</div>")
    } else if(input$tab=="inventor"){
      htmlText = paste("<div style='margin-left: 0.2px'!important;>","Perfil do inventor","</div>")
    } else if(input$tab=="cooperacao"){
      htmlText = paste("<div style='margin-left: 0.2px'!important;>","Cooperação entre os pedidos de patentes","</div>")
    } else if(input$tab=="explorar"){
      htmlText = paste("<div style='margin-left: 0.2px'!important;>","Explorar os pedidos de patentes","</div>")
    }

    HTML(htmlText)
  })
  
  output$title_plot1 <- renderUI({
    if(input$tp_plot1!="Download"){
      htmlText = paste0("Número de patentes depositadas por ano", 
                        ifelse(input$tp_ano=="concessão"," da "," do "),
                        input$tp_ano,", ", min(input$date)," a ",max(input$date))
    }
    
    HTML(htmlText)
  })
  
  output$title_plot2 <- renderUI({
    if(input$tp_plot2!="Download"){
      if(input$tp_plot2=="Barras" | input$tp_plot2=="Linhas" | input$tp_plot2=="Tabela"){
        htmlText = paste0("Número de patentes depositadas segundo classificação tecnológica por ano", 
                          ifelse(input$tp_ano=="concessão"," da "," do "),
                          input$tp_ano,", ",min(input$date)," a ",max(input$date))
        
      } else if(input$tp_plot2=="Setor"){
        htmlText = paste0("Distribuição proporcional das patentes depositadas segundo classificação tecnológica, ",
                          min(input$date)," a ",max(input$date))
        
      }
      HTML(htmlText)
    }
  })
  
  output$title_plot3 <- renderUI({
    if(input$tp_plot3!="Download"){
      if(input$tp_plot3=="Barras" | input$tp_plot3=="Linhas" | input$tp_plot3=="Tabela"){
        htmlText = paste0("Número de patentes depositadas segundo status por ano",ifelse(input$tp_ano=="concessão"," da "," do "),input$tp_ano,", ",min(input$date)," a ",max(input$date))
      } else if(input$tp_plot3=="Setor"){
        htmlText = paste0("Distribuição proporcional das patentes depositadas segundo status, ",min(input$date)," a ",max(input$date))
      }
      HTML(htmlText)
    }
  })
  
  output$title_plot41 <- renderUI({
    if(input$tp_plot41!="Download"){
      if(input$tp_plot41=="Barras" | input$tp_plot41=="Linhas"){
        htmlText = paste0(ifelse(input$select41=="Número absoluto","Número de patentes ","Distribuição proporcional das patentes "),
                          "depositadas por inventores com nacionalidade ",
                          ifelse(input$local=="Brasil","brasileira","de algum país das Américas")," por ano ",
                          ifelse(input$tp_ano=="concessão","da ","do "),input$tp_ano,", ",min(input$date)," a ",max(input$date))
      } else if(input$tp_plot41=="Tabela"){
        htmlText = paste0("Número de patentes depositadas por inventores com nacionalidade ",
                          ifelse(input$local=="Brasil","brasileira","de algum país das Américas")," por ano ",
                          ifelse(input$tp_ano=="concessão","da ","do "),input$tp_ano,", ",min(input$date)," a ",max(input$date))
      } else if(input$tp_plot41=="Setor"){
        htmlText = paste0("Distribuição proporcional das patentes depositadas por inventores com nacionalidade ",
                          ifelse(input$local=="Brasil","brasileira","de algum país das Américas"),", ",min(input$date)," a ",max(input$date))
      }
      
      HTML(htmlText)
    }
    
  })
  
  output$title_plot42 <- renderUI({
    if(input$tp_plot42!="Download"){
      if(input$tp_plot42=="Barras"){
        htmlText = paste0(ifelse(input$select42=="Número absoluto","Número de patentes depositadas por pessoa física por ano ",
                                 "Distribuição proporcional das patentes depositadas por tipo de pessoa (física e não física) por ano "),
                          ifelse(input$tp_ano=="concessão","da ","do "),input$tp_ano,", ",min(input$date)," a ",max(input$date))
      } else if(input$tp_plot42=="Linhas"){
        htmlText = paste0("Número de patentes depositadas por pessoa física por ano ",
                          ifelse(input$tp_ano=="concessão","da ","do "),input$tp_ano,", ",min(input$date)," a ",max(input$date))
      } else if(input$tp_plot42=="Tabela"){
        htmlText = paste0("Número de patentes depositadas por tipo de pessoa (física e não física) por ano ",
                          ifelse(input$tp_ano=="concessão","da ","do "),input$tp_ano,", ",min(input$date)," a ",max(input$date))
      } else if(input$tp_plot42=="Setor"){
        htmlText = paste0("Distribuição proporcional das patentes depositadas por tipo de pessoa (física e não física), ",min(input$date)," a ",max(input$date))
      }

      HTML(htmlText)
    }

  })
  
  output$title_plot5 <- renderUI({
    if(input$tp_plot5!="Download"){
      if(input$tp_plot5=="Barras"){
        htmlText = paste0(ifelse(input$select5=="Proporção",
                                 "Distribuição proporcional das patentes depositadas segundo presença feminina entre os inventores por ano ",
                                 "Número de patentes depositadas com a presença de inventor do sexo feminino por ano "),
                          ifelse(input$tp_ano=="concessão","da ","do "),input$tp_ano,", ",min(input$date)," a ",max(input$date))
      } else if(input$tp_plot5=="Tabela" | input$tp_plot5=="Linhas"){
        htmlText = paste0("Número de patentes depositadas com a presença de inventor do sexo feminino por ano ",
                          ifelse(input$tp_ano=="concessão","da ","do "),input$tp_ano,", ",min(input$date)," a ",max(input$date))
      } else if(input$tp_plot5=="Setor"){
        htmlText = paste0("Distribuição proporcional das patentes depositadas segundo presença feminina entre os inventores, ",min(input$date)," a ",max(input$date))
      }
      
      HTML(htmlText)
    }
    
  })
  
  output$title_plot6 <- renderUI({
    if(input$tp_plot6!="Download"){
      if(input$tp_plot6=="Barras"){
        htmlText = paste0(ifelse(input$select6=="Proporção",
                                 paste0("Distribuição proporcional das patentes depositadas por inventores brasileiros com cooperação ",input$coopera," por ano "),
                                 paste0("Número de patentes depositadas por inventores brasileiros com cooperação ",input$coopera," por ano ")),
                          ifelse(input$tp_ano=="Concessão","da ","do "),input$tp_ano,", ",min(input$date)," a ",max(input$date))
      } else if(input$tp_plot6=="Tabela" | input$tp_plot6=="Linhas"){
        htmlText = paste0("Número de patentes depositadas por inventores brasileiros com cooperação ",input$coopera," por ano ",
                          ifelse(input$tp_ano=="Concessão","da ","do "),input$tp_ano,", ",min(input$date)," a ",max(input$date))
      } else if(input$tp_plot6=="Setor"){
        htmlText = paste0("Distribuição proporcional das patentes depositadas por inventores brasileiros com cooperação ", input$coopera,", ",min(input$date)," a ",max(input$date))
      }
      
      HTML(htmlText)
    }
    
  })
  
  output$plot1 <- renderGirafe({
    
    db1=database() %>%
      group_by(date=ano) %>%
      summarise(count=sum(count))
    
    
    xTitle=switch (input$tp_ano,
      "pedido" = "Ano do pedido",
      "concessão" = "Ano da concessão",
      "deferimento" = "Ano do deferimento",
      "indeferimento" = "Ano do indeferimento"
    )
    
    
    yTitle="Número de patentes"
    
    if(input$tp_plot1=="Barras"){
      p=ggplot(db1,aes(x=date,y=count))+
        geom_bar_interactive(stat='identity',aes(tooltip=count),fill="#005266")+
        labs(x=xTitle,y=yTitle)+
        theme_classic()+
        theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = .5, face = "plain"),
              axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 1, vjust = 0, face = "plain"),
              axis.title.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
              axis.title.y = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
              legend.text=element_text(size = 16),
              text=element_text(size = 16),
              title = element_text(size = 16),
              plot.caption = element_text(size=12),
              strip.text = element_text(size=16))+
        guides(fill=guide_legend(nrow=1,byrow=TRUE))+
        scale_y_continuous(labels = scales::number,expand=c(0,0.05))+
        scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))
      
      girafe(ggobj = p,width_svg = 14,height_svg = 8)
      
    } else if(input$tp_plot1=="Linhas"){
      p=ggplot(db1,aes(x=date,y=count))+
        geom_line(color="#005266",size=1.1)+
        geom_point_interactive(aes(tooltip=count),color="#005266")+
        labs(x=xTitle,y=yTitle)+
        theme_classic()+
        theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = .5, face = "plain"),
              axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
              axis.title.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
              axis.title.y = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
              legend.text=element_text(size = 16),
              text=element_text(size = 16),
              title = element_text(size = 16),
              plot.caption = element_text(size=12),
              strip.text = element_text(size=16))+
        guides(fill=guide_legend(nrow=1,byrow=TRUE))+
        scale_y_continuous(labels = scales::number,expand=c(0,0.05),limits = c(0,max(db1$count)))+
        scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))
      
      girafe(ggobj = p,width_svg = 14,height_svg = 8)
      
    }
  })
  
  output$plot2 <- renderGirafe({
    
    db=database()
    db=melt(db[,c("ano",input$nivel2)],id="ano")
    db=merge(db,cat[,c("label1","label2")],by.x="variable",by.y="label2",all.x=T)
    
    db$classif=switch(input$nivel,
      "Nível 1" = db$label1,
      "Nível 2" = db$variable
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
    
    
    db1=db %>%
      group_by(date=ano,cat=classif) %>%
      summarise(count=sum(value))
    
    
    db1$per=NA
    for(i in unique(db1$date)){
      db1[db1$date==i,]$per=round(db1[db1$date==i,]$count/sum(db1[db1$date==i,]$count)*100,1)
    }
    
    xTitle=switch (input$tp_ano,
                   "pedido" = "Ano do pedido",
                   "concessão" = "Ano da concessão",
                   "deferimento" = "Ano do deferimento",
                   "indeferimento" = "Ano do indeferimento"
    )
    
    
    if(input$tp_plot2=="Barras"){
      yTitle=switch (input$select2,
        "Número absoluto" = "Número de patentes",
        "Proporção" = "Proporção"
      )
          
      p=ggplot(db1,aes(x=date,y=count,fill=cat))+
        geom_bar_interactive(stat='identity',position=ifelse(input$select2=="Número absoluto","stack","fill"),aes(tooltip=paste0(cat,": ",count)))+
        labs(x=xTitle,y=yTitle,fill="")+
        theme_classic()+
        theme(legend.position = "bottom")+
        theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = .5, face = "plain"),
              axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
              axis.title.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
              axis.title.y = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
              legend.text=element_text(size = 12),
              text=element_text(size = 16),
              title = element_text(size = 16),
              plot.caption = element_text(size=12),
              strip.text = element_text(size=16))+
        guides(fill=guide_legend(nrow=ifelse(input$nivel=="Nível 1",2,8),byrow=TRUE))+
        scale_fill_manual("",values = colors)+
        scale_y_continuous(labels = ifelse(input$select2=="Número absoluto",scales::number,scales::percent),expand=c(0,0.05))+
        scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))
      
      girafe(ggobj = p,width_svg = 14,height_svg = 8)
      
    } else if(input$tp_plot2=="Linhas"){
      yTitle="Número de patentes"
      
      p=ggplot(db1,aes(x=date,y=count,color=cat))+
        geom_line(size=1.1)+
        geom_point_interactive(aes(tooltip=count))+
        labs(x=xTitle,y=yTitle,color=NULL)+
        theme_classic()+
        theme(legend.position = "bottom")+
        theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = .5, face = "plain"),
              axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
              axis.title.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
              axis.title.y = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
              legend.text=element_text(size = 12),
              text=element_text(size = 16),
              title = element_text(size = 16),
              plot.caption = element_text(size=12),
              strip.text = element_text(size=16))+
        guides(color=guide_legend(nrow=ifelse(input$nivel=="Nível 1",2,8),byrow=TRUE))+
        scale_color_manual("",values = colors)+
        scale_y_continuous(labels = scales::number,expand=c(0,0.05),limits = c(0,max(db1$count)))+
        scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))
      
      girafe(ggobj = p,width_svg = 14,height_svg = 8)
      
    } else if(input$tp_plot2=="Setor"){
      
      db1=db %>%
        group_by(cat=classif) %>%
        summarise(count=sum(value))
      
      db1$per=db1$count/sum(db1$count)
      
      db1$ymax=cumsum(db1$per)
      db1$ymin=c(0,head(db1$ymax,n=-1))
      
      p=ggplot(db1, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=cat))+
        geom_rect_interactive(aes(tooltip=paste0(cat,": ",round(per*100,1),"%")))+
        scale_color_manual("",values = colors)+
        scale_fill_manual("",values = colors)+
        coord_polar(theta="y")+
        xlim(c(2, 4))+
        theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = .5, face = "plain"),
              axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
              axis.title.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
              axis.title.y = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
              legend.text=element_text(size = 16),
              text=element_text(size = 16),
              title = element_text(size = 16),
              plot.caption = element_text(size=12),
              strip.text = element_text(size=16))+
        guides(color=guide_legend(nrow=ifelse(input$nivel=="Nível 1",2,8),byrow=TRUE))+
        theme_void()+
        theme(legend.position = "bottom")
        
      
      girafe(ggobj = p,width_svg = 14,height_svg = 8)
      
    }
  })
  
  output$plot3 <- renderGirafe({
    
    db1=database() %>% 
      group_by(date=ano,cat=status1) %>% 
      summarise(count=sum(count))
    
    
    db1$per=NA
    for(i in unique(db1$date)){
      db1[db1$date==i,]$per=round(db1[db1$date==i,]$count/sum(db1[db1$date==i,]$count)*100,1)
    }
    
    colors=c("deferida"="#a6cee3","concedida"="#1f78b4","publicada"="#b2df8a","PCT"="#33a02c",
             "indeferida"="#fb9a99","oferta"="#e31a1c","subJudice"="#fdbf6f","exigenciaPedido"="#ff7f00",
             "anulada"="#cab2d6","sem informação"="#999999")
    
    xTitle=switch (input$tp_ano,
                   "pedido" = "Ano do pedido",
                   "concessão" = "Ano da concessão",
                   "deferimento" = "Ano do deferimento",
                   "indeferimento" = "Ano do indeferimento"
    )
    
    if(input$tp_plot3=="Barras"){
      
      yTitle=switch (input$select3,
        "Número absoluto" = "Número de patentes",
        "Proporção" = "Proporção"
      )
      
      p=ggplot(db1,aes(x=date,y=count,fill=cat))+
        geom_bar_interactive(stat='identity',position=ifelse(input$select3=="Número absoluto","stack","fill"),aes(tooltip=paste0(cat,": ",count)))+
        labs(x=xTitle,y=yTitle,fill="")+
        theme_classic()+
        theme(legend.position = "bottom")+
        theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = .5, face = "plain"),
              axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
              axis.title.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
              axis.title.y = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
              legend.text=element_text(size = 12),
              text=element_text(size = 16),
              title = element_text(size = 16),
              plot.caption = element_text(size=16),
              strip.text = element_text(size=16))+
        guides(fill=guide_legend(nrow=1,byrow=TRUE))+
        scale_fill_manual("",values = colors)+
        scale_y_continuous(labels = ifelse(input$select3=="Número absoluto",scales::number,scales::percent),expand=c(0,0.05))+
        scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))
      
      girafe(ggobj = p,width_svg = 14,height_svg = 8)
      

    } else if(input$tp_plot3=="Linhas"){
      yTitle="Número de patentes"
      
      p=ggplot(db1,aes(x=date,y=count,color=cat))+
        geom_line(size=1.1)+
        geom_point_interactive(aes(tooltip=count))+
        labs(x=xTitle,y=yTitle,color=NULL)+
        theme_classic()+
        theme(legend.position = "bottom")+
        theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = .5, face = "plain"),
              axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
              axis.title.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
              axis.title.y = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
              legend.text=element_text(size = 12),
              text=element_text(size = 16),
              title = element_text(size = 16),
              plot.caption = element_text(size=16),
              strip.text = element_text(size=16))+
        guides(color=guide_legend(nrow=1,byrow=TRUE))+
        scale_color_manual("",values = colors)+
        scale_y_continuous(labels = scales::number,expand=c(0,0.05),limits = c(0,max(db1$count)))+
        scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))
      
      girafe(ggobj = p,width_svg = 14,height_svg = 8)
      
    } else if(input$tp_plot3=="Setor"){
      db=database() %>% 
        group_by(cat=status1) %>% 
        summarise(count=sum(count))
      
      db$per=db$count/sum(db$count)
      
      db$ymax=cumsum(db$per)
      db$ymin=c(0,head(db$ymax,n=-1))
      
      p=ggplot(db, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=cat))+
        geom_rect_interactive(aes(tooltip=paste0(cat,": ",round(per*100,1),"%")))+
        scale_color_manual("",values = colors)+
        scale_fill_manual("",values = colors)+
        coord_polar(theta="y")+
        xlim(c(2, 4))+
        theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = .5, face = "plain"),
              axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
              axis.title.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
              axis.title.y = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
              legend.text=element_text(size = 16),
              text=element_text(size = 16),
              title = element_text(size = 16),
              plot.caption = element_text(size=16),
              strip.text = element_text(size=16))+
        guides(color=guide_legend(nrow=1,byrow=TRUE))+
        theme_void()+
        theme(legend.position = "bottom")
      
      
      girafe(ggobj = p,width_svg = 14,height_svg = 8)
    }
  })
  
  output$plot41 <- renderGirafe({
    
    db=database()
    
    db$cat=switch(input$local,
                  "Brasil" = db$brasil,
                  "América" = db$america
    )
    
    db1<-db %>% 
      group_by(date=ano,cat=cat) %>% 
      summarise(count=sum(count))
    
    
    db1$per=NA
    for(i in unique(db1$date)){
      db1[db1$date==i,]$per=round(db1[db1$date==i,]$count/sum(db1[db1$date==i,]$count)*100,1)
    }
    
    xTitle=switch (input$tp_ano,
                   "pedido" = "Ano do pedido",
                   "concessão" = "Ano da concessão",
                   "deferimento" = "Ano do deferimento",
                   "indeferimento" = "Ano do indeferimento"
    )
    
    if(input$tp_plot41=="Barras"){
      if(input$select41=="Número absoluto"){
        yTitle="Número de patentes"
        
        p=ggplot(db1[which(db1$cat==1),],aes(x=date,y=count))+
          geom_bar_interactive(stat='identity',fill="#005266",aes(tooltip=count))+
          labs(x=xTitle,y=yTitle,fill="")+
          theme_classic()+
          theme(legend.position = "none")+
          theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = .5, face = "plain"),
                axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
                axis.title.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
                axis.title.y = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
                legend.text=element_text(size = 12),
                text=element_text(size = 16),
                title = element_text(size = 16),
                plot.caption = element_text(size=16),
                strip.text = element_text(size=16))+
          guides(fill=guide_legend(nrow=1,byrow=TRUE))+
          scale_y_continuous(labels = scales::number,expand=c(0,0.05))+
          scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))
        
        
        
      } else if(input$select41=="Proporção"){
        yTitle="Proporção"
        
        db1$cat=factor(db1$cat,levels=c(1,0),labels=c(ifelse(input$local=="Brasil","Brasil","América"),
                                                      ifelse(input$local=="Brasil","Outros países","Outros continentes")))
        
        p=ggplot(db1,aes(x=date,y=count,fill=cat))+
          geom_bar_interactive(stat='identity',position=position_fill(reverse = T),aes(tooltip=paste0(cat,": ",per,"%")))+
          labs(x=xTitle,y=yTitle,fill="")+
          theme_classic()+
          theme(legend.position = "bottom")+
          theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = .5, face = "plain"),
                axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
                axis.title.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
                axis.title.y = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
                legend.text=element_text(size = 12),
                text=element_text(size = 16),
                title = element_text(size = 16),
                plot.caption = element_text(size=16),
                strip.text = element_text(size=16))+
          guides(fill=guide_legend(nrow=1,byrow=TRUE))+
          scale_fill_manual("",values=c("#005266","#7197A4"))+
          scale_y_continuous(labels = scales::percent,expand=c(0,0.05))+
          scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))
      }
      
      
    } else if(input$tp_plot41=="Linhas"){
      yTitle="Número de patentes"
      
      p=ggplot(db1[which(db1$cat==1),],aes(x=date,y=count))+
        geom_line(color="#005266",size=1.1)+
        geom_point_interactive(aes(tooltip=count),color="#005266")+
        labs(x=xTitle,y=yTitle,color=NULL)+
        theme_classic()+
        theme(legend.position = "bottom")+
        theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = .5, face = "plain"),
              axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
              axis.title.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
              axis.title.y = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
              legend.text=element_text(size = 12),
              text=element_text(size = 16),
              title = element_text(size = 16),
              plot.caption = element_text(size=16),
              strip.text = element_text(size=16))+
        guides(color=guide_legend(nrow=1,byrow=TRUE))+
        scale_color_manual("",values = colors)+
        scale_y_continuous(labels = scales::number,expand=c(0,0.05),limits = c(0,max(db1[which(db1$cat==1),]$count)))+
        scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))
      
    } else if(input$tp_plot41=="Setor"){
      db1=db %>%
        group_by(cat) %>% 
        summarise(count=sum(count))
      
      db1$per=db1$count/sum(db1$count)
      
      db1$ymax=cumsum(db1$per)
      db1$ymin=c(0,head(db1$ymax,n=-1))
      
      db1$cat=factor(db1$cat,levels=c(1,0),labels=c(input$local,ifelse(input$local=="Brasil","Outros países","Outros continentes")))
      
      p=ggplot(db1, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=cat))+
        geom_rect_interactive(aes(tooltip=paste0(cat,": ",round(per*100,1),"%")))+
        scale_color_manual("",values = c("#005266","#7197A4"))+
        scale_fill_manual("",values = c("#005266","#7197A4"))+
        coord_polar(theta="y")+
        labs(fill="",color="")+
        xlim(c(2, 4))+
        theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = .5, face = "plain"),
              axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
              axis.title.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
              axis.title.y = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
              legend.text=element_text(size = 18),
              text=element_text(size = 16),
              title = element_text(size = 16),
              plot.caption = element_text(size=16),
              strip.text = element_text(size=16))+
        guides(color=guide_legend(nrow=1,byrow=TRUE))+
        theme_void()+
        theme(legend.position = "bottom")
      
    }
    
    girafe(ggobj = p,width_svg = 14,height_svg = 8)
  })
  
  output$plot42 <- renderGirafe({
    
    db1=database() %>% 
      group_by(date=ano,cat=tp_pessoa) %>% 
      summarise(count=sum(count))
    
    db1$per=NA
    for(i in unique(db1$date)){
      db1[db1$date==i,]$per=round(db1[db1$date==i,]$count/sum(db1[db1$date==i,]$count)*100,1)
    }
    
    xTitle=switch (input$tp_ano,
                   "pedido" = "Ano do pedido",
                   "concessão" = "Ano da concessão",
                   "deferimento" = "Ano do deferimento",
                   "indeferimento" = "Ano do indeferimento"
    )
    
    if(input$tp_plot42=="Barras"){
      if(input$select42=="Número absoluto"){
        yTitle="Número de patentes"
        
        p=ggplot(db1[which(db1$cat==1),],aes(x=date,y=count))+
          geom_bar_interactive(stat='identity',fill="#005266",aes(tooltip=count))+
          labs(x=xTitle,y=yTitle,fill="")+
          theme_classic()+
          theme(legend.position = "none")+
          theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = .5, face = "plain"),
                axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
                axis.title.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
                axis.title.y = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
                legend.text=element_text(size = 12),
                text=element_text(size = 16),
                title = element_text(size = 16),
                plot.caption = element_text(size=16),
                strip.text = element_text(size=16))+
          guides(fill=guide_legend(nrow=1,byrow=TRUE))+
          scale_y_continuous(labels = scales::number,expand=c(0,0.05))+
          scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))
        
        
        
      } else if(input$select42=="Proporção"){
        yTitle="Proporção"
        
        db1$cat=factor(db1$cat,levels=c(1,0,9),labels=c("Pessoa física","Não pessoa física","Sem informação de país"))
        
        p=ggplot(db1,aes(x=date,y=count,fill=cat))+
          geom_bar_interactive(stat='identity',position=position_fill(reverse = F),aes(tooltip=paste0(cat,": ",per,"%")))+
          labs(x=xTitle,y=yTitle,fill="")+
          theme_classic()+
          theme(legend.position = "bottom")+
          theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = .5, face = "plain"),
                axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
                axis.title.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
                axis.title.y = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
                legend.text=element_text(size = 12),
                text=element_text(size = 16),
                title = element_text(size = 16),
                plot.caption = element_text(size=16),
                strip.text = element_text(size=16))+
          guides(fill=guide_legend(nrow=1,byrow=TRUE))+
          scale_fill_manual("",values=c("#005266","#7197A4","grey70"))+
          scale_y_continuous(labels = scales::percent,expand=c(0,0.05))+
          scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))
      }
      
      
    } else if(input$tp_plot42=="Linhas"){
      yTitle="Número de patentes"
      
      p=ggplot(db1[which(db1$cat==1),],aes(x=date,y=count))+
        geom_line(color="#005266",size=1.1)+
        geom_point_interactive(aes(tooltip=count),color="#005266")+
        labs(x=xTitle,y=yTitle,color=NULL)+
        theme_classic()+
        theme(legend.position = "bottom")+
        theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = .5, face = "plain"),
              axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
              axis.title.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
              axis.title.y = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
              legend.text=element_text(size = 12),
              text=element_text(size = 16),
              title = element_text(size = 16),
              plot.caption = element_text(size=16),
              strip.text = element_text(size=16))+
        guides(color=guide_legend(nrow=1,byrow=TRUE))+
        scale_color_manual("",values = colors)+
        scale_y_continuous(labels = scales::number,expand=c(0,0.05),limits = c(0,max(db1[which(db1$cat==1),]$count)))+
        scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))
      
    } else if(input$tp_plot42=="Setor"){
      
      db2=database() %>% 
        group_by(cat=tp_pessoa) %>% 
        summarise(count=sum(count))
      
      db2$per=db2$count/sum(db2$count)
      
      db2$ymax=cumsum(db2$per)
      db2$ymin=c(0,head(db2$ymax,n=-1))
      
      db2$cat=factor(db2$cat,levels=c(1,0,9),labels=c("Pessoa física","Não pessoa física","Sem informação de país"))
      
      p=ggplot(db2, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=cat))+
        geom_rect_interactive(aes(tooltip=paste0(cat,": ",round(per*100,1),"%")))+
        scale_color_manual("",values = c("#005266","#7197A4","grey70"))+
        scale_fill_manual("",values = c("#005266","#7197A4","grey70"))+
        coord_polar(theta="y")+
        labs(fill="",color="")+
        xlim(c(2, 4))+
        theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = .5, face = "plain"),
              axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
              axis.title.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
              axis.title.y = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
              legend.text=element_text(size = 18),
              text=element_text(size = 16),
              title = element_text(size = 16),
              plot.caption = element_text(size=16),
              strip.text = element_text(size=16))+
        guides(color=guide_legend(nrow=1,byrow=TRUE))+
        theme_void()+
        theme(legend.position = "bottom")
      
    }
    
    girafe(ggobj = p,width_svg = 14,height_svg = 8)
  })
  
  output$plot5 <- renderGirafe({
    
    db1=database() %>% 
      group_by(date=ano,cat=feminino) %>% 
      summarise(count=sum(count))
    
    db1$per=NA
    for(i in unique(db1$date)){
      db1[db1$date==i,]$per=round(db1[db1$date==i,]$count/sum(db1[db1$date==i,]$count)*100,1)
    }
    
    xTitle=switch (input$tp_ano,
                   "pedido" = "Ano do pedido",
                   "concessão" = "Ano da concessão",
                   "deferimento" = "Ano do deferimento",
                   "indeferimento" = "Ano do indeferimento"
    )
    
    if(input$tp_plot5=="Barras"){
      if(input$select5=="Número absoluto"){
        yTitle="Número de patentes"
        
        p=ggplot(db1[which(db1$cat==1),],aes(x=date,y=count))+
          geom_bar_interactive(stat='identity',fill="#005266",aes(tooltip=count))+
          labs(x=xTitle,y=yTitle,fill="")+
          theme_classic()+
          theme(legend.position = "none")+
          theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = .5, face = "plain"),
                axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
                axis.title.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
                axis.title.y = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
                legend.text=element_text(size = 12),
                text=element_text(size = 16),
                title = element_text(size = 16),
                plot.caption = element_text(size=16),
                strip.text = element_text(size=16))+
          guides(fill=guide_legend(nrow=1,byrow=TRUE))+
          scale_y_continuous(labels = scales::number,expand=c(0,0.05))+
          scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))
        
        
        
      } else if(input$select5=="Proporção"){
        yTitle="Proporção"
        
        db1$cat=factor(db1$cat,levels=c(1,0,9),labels=c("Presença feminina","Ausência feminina","Sem informação"))
        
        p=ggplot(db1,aes(x=date,y=count,fill=cat))+
          geom_bar_interactive(stat='identity',position=position_fill(reverse = F),aes(tooltip=paste0(cat,": ",per,"%")))+
          labs(x=xTitle,y=yTitle,fill="")+
          theme_classic()+
          theme(legend.position = "bottom")+
          theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = .5, face = "plain"),
                axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
                axis.title.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
                axis.title.y = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
                legend.text=element_text(size = 12),
                text=element_text(size = 16),
                title = element_text(size = 16),
                plot.caption = element_text(size=16),
                strip.text = element_text(size=16))+
          guides(fill=guide_legend(nrow=1,byrow=TRUE))+
          scale_fill_manual("",values=c("#005266","#7197A4","grey70"))+
          scale_y_continuous(labels = scales::percent,expand=c(0,0.05))+
          scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))
      }
      
      
    } else if(input$tp_plot5=="Linhas"){
      yTitle="Número de patentes"
      
      p=ggplot(db1[which(db1$cat==1),],aes(x=date,y=count))+
        geom_line(color="#005266",size=1.1)+
        geom_point_interactive(aes(tooltip=count),color="#005266")+
        labs(x=xTitle,y=yTitle,color=NULL)+
        theme_classic()+
        theme(legend.position = "bottom")+
        theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = .5, face = "plain"),
              axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
              axis.title.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
              axis.title.y = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
              legend.text=element_text(size = 12),
              text=element_text(size = 16),
              title = element_text(size = 16),
              plot.caption = element_text(size=16),
              strip.text = element_text(size=16))+
        guides(color=guide_legend(nrow=1,byrow=TRUE))+
        scale_color_manual("",values = colors)+
        scale_y_continuous(labels = scales::number,expand=c(0,0.05),limits = c(0,max(db1[which(db1$cat==1),]$count)))+
        scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))
      
    } else if(input$tp_plot5=="Setor"){
      
      db2=database() %>% 
        group_by(cat=feminino) %>% 
        summarise(count=sum(count))
      
      db2$per=db2$count/sum(db2$count)
      
      db2$ymax=cumsum(db2$per)
      db2$ymin=c(0,head(db2$ymax,n=-1))
      
      db2$cat=factor(db2$cat,levels=c(1,0,9),labels=c("Presença feminina","Ausência feminina","Sem informação"))
      
      p=ggplot(db2, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=cat))+
        geom_rect_interactive(aes(tooltip=paste0(cat,": ",round(per*100,1),"%")))+
        scale_color_manual("",values = c("#005266","#7197A4","grey70"))+
        scale_fill_manual("",values = c("#005266","#7197A4","grey70"))+
        coord_polar(theta="y")+
        labs(fill="",color="")+
        xlim(c(2, 4))+
        theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = .5, face = "plain"),
              axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
              axis.title.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
              axis.title.y = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
              legend.text=element_text(size = 18),
              text=element_text(size = 16),
              title = element_text(size = 16),
              plot.caption = element_text(size=16),
              strip.text = element_text(size=16))+
        guides(color=guide_legend(nrow=1,byrow=TRUE))+
        theme_void()+
        theme(legend.position = "bottom")
      
    }
    
    girafe(ggobj = p,width_svg = 14,height_svg = 8)
  })
  
  output$plot6 <- renderGirafe({
    
    db=database()
    db$coopera=switch(input$coopera,
                      "nacional" = db$cooper_nac,
                      "internacional" = db$cooper_int
    )
    
    db1=db %>% 
      filter(brasil==1) %>% 
      group_by(date=ano,cat=coopera) %>% 
      summarise(count=sum(count))
    
    db1$per=NA
    for(i in unique(db1$date)){
      db1[db1$date==i,]$per=round(db1[db1$date==i,]$count/sum(db1[db1$date==i,]$count)*100,1)
    }
    
    xTitle=switch (input$tp_ano,
                   "pedido" = "Ano do pedido",
                   "concessão" = "Ano da concessão",
                   "deferimento" = "Ano do deferimento",
                   "indeferimento" = "Ano do indeferimento"
    )
    
    if(input$tp_plot6=="Barras"){
      if(input$select6=="Número absoluto"){
        yTitle="Número de patentes"
        
        p=ggplot(db1[which(db1$cat==1),],aes(x=date,y=count))+
          geom_bar_interactive(stat='identity',fill="#005266",aes(tooltip=count))+
          labs(x=xTitle,y=yTitle,fill="")+
          theme_classic()+
          theme(legend.position = "none")+
          theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = .5, face = "plain"),
                axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
                axis.title.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
                axis.title.y = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
                legend.text=element_text(size = 12),
                text=element_text(size = 16),
                title = element_text(size = 16),
                plot.caption = element_text(size=16),
                strip.text = element_text(size=16))+
          guides(fill=guide_legend(nrow=1,byrow=TRUE))+
          scale_y_continuous(labels = scales::number,expand=c(0,0.05))+
          scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))
        
        
        
      } else if(input$select6=="Proporção"){
        yTitle="Proporção"
        
        db1$cat=factor(db1$cat,levels=c(1,0,9),labels=c(paste0("Com cooperação ",input$coopera),paste0("Sem cooperação ",input$coopera),"Sem informação"))
        
        p=ggplot(db1,aes(x=date,y=count,fill=cat))+
          geom_bar_interactive(stat='identity',position=position_fill(reverse = T),aes(tooltip=paste0(cat,": ",per,"%")))+
          labs(x=xTitle,y=yTitle,fill="")+
          theme_classic()+
          theme(legend.position = "bottom")+
          theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = .5, face = "plain"),
                axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
                axis.title.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
                axis.title.y = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
                legend.text=element_text(size = 12),
                text=element_text(size = 16),
                title = element_text(size = 16),
                plot.caption = element_text(size=16),
                strip.text = element_text(size=16))+
          guides(fill=guide_legend(nrow=1,byrow=TRUE))+
          scale_fill_manual("",values=c("#005266","#7197A4","grey70"))+
          scale_y_continuous(labels = scales::percent,expand=c(0,0.05))+
          scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))
      }
      
      
    } else if(input$tp_plot6=="Linhas"){
      yTitle="Número de patentes"
      
      p=ggplot(db1[which(db1$cat==1),],aes(x=date,y=count))+
        geom_line(color="#005266",size=1.1)+
        geom_point_interactive(aes(tooltip=count),color="#005266")+
        labs(x=xTitle,y=yTitle,color=NULL)+
        theme_classic()+
        theme(legend.position = "bottom")+
        theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = .5, face = "plain"),
              axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
              axis.title.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
              axis.title.y = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
              legend.text=element_text(size = 12),
              text=element_text(size = 16),
              title = element_text(size = 16),
              plot.caption = element_text(size=16),
              strip.text = element_text(size=16))+
        guides(color=guide_legend(nrow=1,byrow=TRUE))+
        scale_color_manual("",values = colors)+
        scale_y_continuous(labels = scales::number,expand=c(0,0.05),limits = c(0,max(db1[which(db1$cat==1),]$count)))+
        scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))
      
    } else if(input$tp_plot6=="Setor"){
      
      db2=db %>% 
        filter(brasil==1) %>% 
        group_by(cat=coopera) %>% 
        summarise(count=sum(count))
      
      db2$per=db2$count/sum(db2$count)
      
      db2$ymax=cumsum(db2$per)
      db2$ymin=c(0,head(db2$ymax,n=-1))
      
      db2$cat=factor(db2$cat,levels=c(1,0,9),labels=c(paste0("Com cooperação ",input$coopera),paste0("Sem cooperação ",input$coopera),"Sem informação"))
      
      p=ggplot(db2, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=cat))+
        geom_rect_interactive(aes(tooltip=paste0(cat,": ",round(per*100,1),"%")))+
        scale_color_manual("",values = c("#005266","#7197A4","grey70"))+
        scale_fill_manual("",values = c("#005266","#7197A4","grey70"))+
        coord_polar(theta="y")+
        labs(fill="",color="")+
        xlim(c(2, 4))+
        theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = .5, face = "plain"),
              axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
              axis.title.x = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
              axis.title.y = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
              legend.text=element_text(size = 18),
              text=element_text(size = 16),
              title = element_text(size = 16),
              plot.caption = element_text(size=16),
              strip.text = element_text(size=16))+
        guides(color=guide_legend(nrow=1,byrow=TRUE))+
        theme_void()+
        theme(legend.position = "bottom")
      
    }
    
    girafe(ggobj = p,width_svg = 14,height_svg = 8)
  })
  
  output$tab1 <- DT::renderDataTable({
    tab=database() %>% 
      group_by(ano) %>% 
      summarise(count=sum(count))
    
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
    
    header.style <- "th { font-family: 'Calibri'; font-size:16px ;font-weight: bold; color: white; background-color: #005266;}"
    
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
  
  output$tab2 <- DT::renderDataTable({
    db=database()
    db=melt(db[,c("ano",input$nivel2)],id="ano")
    db=merge(db,cat[,c("label1","label2")],by.x="variable",by.y="label2",all.x=T)
    
    db$classif=switch(input$nivel,
                      "Nível 1" = db$label1,
                      "Nível 2" = db$variable
    )
    
    tab=db %>%
      group_by(date=ano,cat=classif) %>%
      summarise(count=sum(value))
    
    tab=reshape2::dcast(tab,date~cat,value.var = "count")
    
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
    
    header.style <- "th { font-family: 'Calibri'; font-size:16px ;font-weight: bold; color: white; background-color: #005266;}"
    
    header.names <- c(paste0("Ano",ifelse(input$tp_ano=="Concessão"," da "," do "),input$tp_ano),
                      names(tab)[-1])
    
    my.container <- withTags(table(
      style(type = "text/css", header.style),
      thead(
        tr(
          lapply(header.names, th, style = "text-align: center; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
        )
      )
    ))
    
    
    
    my.table <- datatable(tab, options = my.options, container = my.container, rownames = F, width = '100%', extensions = 'Buttons') %>%
      formatStyle(columns = c(2:ncol(tab)),
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
  
  output$tab3 <- DT::renderDataTable({
    tab=database() %>% 
      group_by(date=ano,cat=status1) %>% 
      summarise(count=sum(count))
    
    tab=reshape2::dcast(tab,date~cat,value.var = "count")
    
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
    
    header.style <- "th { font-family: 'Calibri'; font-size:16px ;font-weight: bold; color: white; background-color: #005266;}"
    
    header.names <- c(paste0("Ano",ifelse(input$tp_ano=="Concessão"," da "," do "),input$tp_ano),
                      names(tab)[-1])
    
    my.container <- withTags(table(
      style(type = "text/css", header.style),
      thead(
        tr(
          lapply(header.names, th, style = "text-align: center; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
        )
      )
    ))
    
    
    
    my.table <- datatable(tab, options = my.options, container = my.container, rownames = F, width = '100%', extensions = 'Buttons') %>%
      formatStyle(columns = c(2:ncol(tab)),
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
  
  output$tab41 <- DT::renderDataTable({
    db=database()
    db$cat=switch(input$local,
                  "Brasil" = db$brasil,
                  "América" = db$america
    )
    
    tab=db %>% 
      group_by(date=ano,cat=cat) %>% 
      summarise(count=sum(count))
    
    tab$cat=switch(input$local,
                   "Brasil" = ifelse(tab$cat==1,"Brasil",ifelse(tab$cat==0,"Outro país","Sem informação")),
                   "América" = ifelse(tab$cat==1,"América",ifelse(tab$cat==0,"Outro continente","Sem informação"))
    )
    
    tab=reshape2::dcast(tab,date~cat,value.var = "count")
    
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
    
    header.style <- "th { font-family: 'Calibri'; font-size:16px ;font-weight: bold; color: white; background-color: #005266;}"
    
    header.names <- c(paste0("Ano",ifelse(input$tp_ano=="Concessão"," da "," do "),input$tp_ano),
                      names(tab)[-1])
    
    my.container <- withTags(table(
      style(type = "text/css", header.style),
      thead(
        tr(
          lapply(header.names, th, style = "text-align: center; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
        )
      )
    ))
    
    
    
    my.table <- datatable(tab, options = my.options, container = my.container, rownames = F, width = '100%', extensions = 'Buttons') %>%
      formatStyle(columns = c(2:ncol(tab)),
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
  
  output$tab42 <- DT::renderDataTable({
    
    tab=database() %>% 
      group_by(date=ano,cat=tp_pessoa) %>% 
      summarise(count=sum(count))
    
    tab$cat=factor(tab$cat,levels=c(1,0,9),labels=c("Pessoa física","Não pessoa física","Sem informação"))
    
    tab=reshape2::dcast(tab,date~cat,value.var = "count")
    
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
    
    header.style <- "th { font-family: 'Calibri'; font-size:16px ;font-weight: bold; color: white; background-color: #005266;}"
    
    header.names <- c(paste0("Ano",ifelse(input$tp_ano=="Concessão"," da "," do "),input$tp_ano),
                      names(tab)[-1])
    
    my.container <- withTags(table(
      style(type = "text/css", header.style),
      thead(
        tr(
          lapply(header.names, th, style = "text-align: center; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
        )
      )
    ))
    
    
    
    my.table <- datatable(tab, options = my.options, container = my.container, rownames = F, width = '100%', extensions = 'Buttons') %>%
      formatStyle(columns = c(2:ncol(tab)),
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
  
  output$tab5 <- DT::renderDataTable({
    
    tab=database() %>% 
      group_by(date=ano,cat=feminino) %>% 
      summarise(count=sum(count))
    
    tab$cat=factor(tab$cat,levels=c(1,0,9),labels=c("Presença Feminina","Ausência Feminina","Sem informação"))
    
    tab=reshape2::dcast(tab,date~cat,value.var = "count")
    
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
    
    header.style <- "th { font-family: 'Calibri'; font-size:16px ;font-weight: bold; color: white; background-color: #005266;}"
    
    header.names <- c(paste0("Ano",ifelse(input$tp_ano=="Concessão"," da "," do "),input$tp_ano),
                      names(tab)[-1])
    
    my.container <- withTags(table(
      style(type = "text/css", header.style),
      thead(
        tr(
          lapply(header.names, th, style = "text-align: center; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
        )
      )
    ))
    
    
    
    my.table <- datatable(tab, options = my.options, container = my.container, rownames = F, width = '100%', extensions = 'Buttons') %>%
      formatStyle(columns = c(2:ncol(tab)),
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
  
  output$tab6 <- DT::renderDataTable({
    
    db=database()
    db$coopera=switch(input$coopera,
      "nacional" = db$cooper_nac,
      "internacional" = db$cooper_int
    )
    
    tab=db %>% 
      filter(brasil==1) %>% 
      group_by(date=ano,cat=coopera) %>% 
      summarise(count=sum(count))
    
    tab$cat=factor(tab$cat,levels=c(1,0,9),labels=c(paste0("Com cooperação ",input$coopera),paste0("Sem cooperação ",input$coopera),"Sem informação"))
    
    tab=reshape2::dcast(tab,date~cat,value.var = "count")
    
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
    
    header.style <- "th { font-family: 'Calibri'; font-size:16px ;font-weight: bold; color: white; background-color: #005266;}"
    
    header.names <- c(paste0("Ano",ifelse(input$tp_ano=="Concessão"," da "," do "),input$tp_ano),
                      names(tab)[-1])
    
    my.container <- withTags(table(
      style(type = "text/css", header.style),
      thead(
        tr(
          lapply(header.names, th, style = "text-align: center; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
        )
      )
    ))
    
    
    
    my.table <- datatable(tab, options = my.options, container = my.container, rownames = F, width = '100%', extensions = 'Buttons') %>%
      formatStyle(columns = c(2:ncol(tab)),
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
  
  output$tab7 <- DT::renderDataTable({
    
    tab=database2()
    tab=tab[which(tab$count==1),c("numeroBusca","titulo","ano_pedido","status1")]
    
    
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
    
    header.style <- "th { font-family: 'Calibri'; font-size:16px ;font-weight: bold; color: white; background-color: #005266;}"
    
    header.names <- c("ID","Título","Ano de pedido","Situação atual")
    
    my.container <- withTags(table(
      style(type = "text/css", header.style),
      thead(
        tr(
          lapply(header.names, th, style = "text-align: left; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
        )
      )
    ))
    
    
    
    my.table <- datatable(tab, options = my.options, container = my.container, rownames = F, width = '100%', extensions = 'Buttons') %>%
      formatStyle(columns = c(1:ncol(tab)),
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
  
  output$data1 <- downloadHandler(
    
    filename = function() {
      paste0("data",input$format1)
    },
    
    content = function(file) {
      
      df=database() %>%
        group_by(ano) %>%
        summarise(n=sum(count))
      
      switch (input$format1,
        ".csv" = write.csv2(df, file,row.names = FALSE),
        ".xlsx" = write.xlsx(df,file)
      )
      
    }
  )
  
  output$data2 <- downloadHandler(
    
    filename = function() {
      paste0("data",input$format2)
    },
    
    content = function(file) {
      
      db=database()
      db=melt(db[,c("ano",input$nivel2)],id="ano")
      db=merge(db,cat[,c("label1","label2")],by.x="variable",by.y="label2",all.x=T)
      
      db$classif=switch(input$nivel,
                        "Nível 1" = db$label1,
                        "Nível 2" = db$variable
      )
      
      df=db %>%
        group_by(Ano=ano,Categoria=classif) %>%
        summarise(n=sum(value))
      
      switch (input$format2,
              ".csv" = write.csv2(df, file,row.names = FALSE),
              ".xlsx" = write.xlsx(df,file)
      )
    }
  )
  
  output$data3 <- downloadHandler(
    
    filename = function() {
      paste0("data",input$format3)
    },
    
    content = function(file) {
      
      df=database() %>% 
        group_by(Ano=ano,Status=status1) %>% 
        summarise(n=sum(count))
      
      switch (input$format3,
              ".csv" = write.csv2(df, file,row.names = FALSE),
              ".xlsx" = write.xlsx(df,file)
      )
    }
  )
  
  output$data4.1 <- downloadHandler(
    
    filename = function() {
      paste0("data",input$format4.1)
    },
    
    content = function(file) {
      
      db=database()
      
      db$cat=switch(input$local,
                    "Brasil" = db$brasil,
                    "América" = db$america
      )
      
      df=db %>% 
        group_by(Ano=ano,Categoria=cat) %>% 
        summarise(n=sum(count))
      
      df$Categoria=switch(input$local,
                          "Brasil" = ifelse(df$Categoria==1,"Brasil",ifelse(df$Categoria==0,"Outro país","Sem informação")),
                          "América" = ifelse(df$Categoria==1,"América",ifelse(df$Categoria==0,"Outro continente","Sem informação"))
      )
      
      switch (input$format4.1,
              ".csv" = write.csv2(df, file,row.names = FALSE),
              ".xlsx" = write.xlsx(df,file)
      )
    }
  )
  
  output$data4.2 <- downloadHandler(
    
    filename = function() {
      paste0("data",input$format4.2)
    },
    
    content = function(file) {
      
      df=database() %>% 
        group_by(Ano=ano,Categoria=tp_pessoa) %>% 
        summarise(n=sum(count))
      
      df$Categoria=ifelse(df$Categoria==1,"Pessoa física",ifelse(df$Categoria==0,"Não pessoa física","Sem informação"))
      
      switch (input$format4.2,
              ".csv" = write.csv2(df, file,row.names = FALSE),
              ".xlsx" = write.xlsx(df,file)
      )
    }
  )
  
  output$data5 <- downloadHandler(
    
    filename = function() {
      paste0("data",input$format5)
    },
    
    content = function(file) {
      
      df=database() %>% 
        group_by(Ano=ano,`Presença Feminina`=feminino) %>% 
        summarise(n=sum(count))
      
      df$`Presença Feminina`=ifelse(df$`Presença Feminina`==1,"Sim",
                                    ifelse(df$`Presença Feminina`==0,"Não","Sem informação"))
      
      switch (input$format5,
              ".csv" = write.csv2(df, file,row.names = FALSE),
              ".xlsx" = write.xlsx(df,file)
      )
    }
  )
  
  output$data6 <- downloadHandler(
    
    filename = function() {
      paste0("data",input$format6)
    },
    
    content = function(file) {
      db=database()
      db$coopera=switch(input$coopera,
                        "nacional" = db$cooper_nac,
                        "internacional" = db$cooper_int
      )
      
      df=db %>% 
        filter(brasil==1) %>% 
        group_by(Ano=ano,Cooperação=coopera) %>% 
        summarise(count=sum(count))
      
      df$Cooperação=factor(df$Cooperação,levels=c(1,0,9),labels=c(paste0("Com cooperação ",input$coopera),paste0("Sem cooperação ",input$coopera),"Sem informação"))
      
      
      switch (input$format6,
              ".csv" = write.csv2(df, file,row.names = FALSE),
              ".xlsx" = write.xlsx(df,file)
      )
    }
  )
  
}

shinyApp(ui, server)




