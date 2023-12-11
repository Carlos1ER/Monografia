library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(htmlwidgets)
library(sf)
library(shinyWidgets)
library(shinyjs)
library(spdep)


ui <- dashboardPage(
  dashboardHeader(title = "Analise Homicídios"),
  dashboardSidebar(
    tags$style(
      "#sidebarItemExpanded {
            overflow: auto;
            max-height: 95vh;
        }"
    ),
    tags$script('
                        var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth * .75;
                        dimension[1] = window.innerHeight * .8;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth * .75;
                        dimension[1] = window.innerHeight * .8;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        '),
    sidebarMenu(
      menuItem("Gráficos", tabName = "graficos"),
      menuItem("Mapa", tabName = "mapa"),
      menuItem("Guia de utilização", tabName = "guia"),
      selectInput(
        inputId = "xcol",
        label = "Variável de referência do Gráfico",
        choices = c("Porte", "Capital", "SEXO", "FaixaEtaria", "RACACOR","RACACOR2"),
        selected = "Porte"
      ),
      
      pickerInput(
        inputId = "v8", label = "Estado",
        choices = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", 
                    "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"),
        selected = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS",
                     "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"),
        options = list(
          `actions-box` = TRUE),
        multiple = TRUE
      ),
      
      pickerInput(
        inputId = "v6", label = "Regiao do Pais",
        choices = c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul"),
        selected = c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul"),
        options = list(
          `actions-box` = TRUE),
        multiple = TRUE
      ),
      
      pickerInput(
        inputId = "v1", label = "Porte da cidade",
        choices = c("Metrópole", "Grande", "Médio", "Pequeno I", "Pequeno II"),
        selected = c("Metrópole", "Grande", "Médio", "Pequeno I", "Pequeno II"),
        options = list(
          `actions-box` = TRUE
        ),
        multiple = TRUE
      ),
      
      
      pickerInput(
        inputId = "v2", label = "Capitais",
        choices = c("Capital", "Não Capital"),
        selected = c("Capital", "Não Capital"),
        options = list(
          `actions-box` = TRUE
        ),
        multiple = TRUE
      ),
      
      pickerInput(
        inputId = "v3", label = "Sexo do indivíduo:",
        choices = c("Masculino", "Feminino"),
        options = list(
          `actions-box` = TRUE),
        selected = c("Masculino", "Feminino"),
        multiple = TRUE
      ),
      
      pickerInput(
        inputId = "v4", label = "Idade do indivíduo",
        choices = c("0-4", "5-9", "10-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75 ou mais"),
        selected = c("0-4", "5-9", "10-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75 ou mais"),
        options = list(
          `actions-box` = TRUE
        ),
        multiple = TRUE
      ),
      pickerInput(
        inputId = "v5", label = "Raça/Cor do indivíduo",
        choices = c("Amarela", "Branca", "Indígena", "Parda", "Preta"),
        selected = c("Amarela", "Branca", "Indígena", "Parda", "Preta"),
        options = list(
          `actions-box` = TRUE
        ),
        multiple = TRUE
      ),
      
      pickerInput(
        inputId = "v7", label = "Grupo Raça/Cor do indivíduo",
        choices = c("Branca", "Não Branca"),
        selected = c("Branca", "Não Branca"),
        options = list(
          `actions-box` = TRUE
        ),
        multiple = TRUE
      )
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "graficos",
        fluidRow(
          tabBox(
            width = 12,
            tabPanel("Absoluto", plotlyOutput("plot_absoluto",height = "100%", width = "100%",fill = T)),
            tabPanel("Relativo", plotlyOutput("plot_relativo",height = "100%",width = "100%" , fill = T)),
            tabPanel("Taxa de homicídio",plotlyOutput("plot_taxahom"),height = "100%",width = "100%" , fill = T)
          )
        )
      ),
      tabItem(
        tabName = "mapa",
        fluidRow(align = "center",
          sliderTextInput(
            inputId = "ANO",
            label = "Escolha o Ano:", 
            choices = c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012,
                        2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021),
            selected = 2021,
            grid = TRUE
          ),
          tabBox(
            width = 12,
            tabPanel("Padrão", plotOutput("mapa_basico")),
            tabPanel("Intervalar", plotOutput("mapa_int")),
            tabPanel("Estimador Local", plotOutput("mapa_local")),
            tabPanel("Estimador Global", plotOutput("mapa_global"))
          )
        )
      ),
      tabItem(
        tabName = "guia",
        fluidRow(
          tabBox(width = 12,
            tabPanel("Guia de utilização",tags$iframe(style="height:550px; width:100%; scrolling=yes", src="www/manual.pdf"))
          )
        )
      )
      
    )
  )
  
)




