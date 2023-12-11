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


#df_filtrado = read.csv2("df_homicidios.csv", encoding = "UTF-8", sep = ";")
#df_taxa = read.csv2("taxahom.csv", encoding = "UTF-8")
#df_censo = read.csv2("df_censo.csv", encoding = "UTF-8", sep = ";")
#load("municipios_hom.RData")
#load("teste_tcc.RData")
load("BASE_FINAL.RData")
addResourcePath(prefix = "www",getwd())
sf_use_s2(FALSE)

server <- function(input, output) {
  
  inputPorte = reactive(input$v1) %>%  debounce(2000)
  inputCapital = reactive(input$v2) %>%  debounce(2000)
  inputSexo = reactive(input$v3) %>%  debounce(2000)
  inputIdade = reactive(input$v4) %>%  debounce(2000)
  inputRacaCor = reactive(input$v5) %>%  debounce(2000)
  inputGrandeRegiao = reactive(input$v6) %>%  debounce(2000)
  inputRacaCor2 = reactive(input$v7) %>%  debounce(2000)
  inputAno = reactive(input$ANO) %>%  debounce(2000)
  inputEstado = reactive(input$v8) %>%  debounce(2000)
  
  
  novo_df = reactive({df_filtrado})
  df_taxa2 = reactive({df_taxa})
  censo = reactive({df_censo})
  
  municipios_hom_filtrado = reactive({
    mun = municipios_hom %>% 
      filter(ANO %in% as.factor(input$ANO)) %>% 
      filter(GrandeRegiao %in% as.factor(inputGrandeRegiao())) %>%
      filter(abbrev_state %in% as.factor(inputEstado()) )%>%
      na.omit() 
    return(mun)
    })
  
  novo_df_filtrado = reactive({
    novo_df = df_filtrado %>%
      filter(Porte %in% as.factor(inputPorte())) %>%
      filter(Capital %in% as.factor(inputCapital())) %>%
      filter(SEXO %in% as.factor(inputSexo())) %>%
      filter(FaixaEtaria %in% as.factor(inputIdade())) %>%
      filter(RACACOR %in% as.factor(inputRacaCor())) %>%
      filter(GrandeRegiao %in% as.factor(inputGrandeRegiao())) %>%
      filter(RACACOR2 %in% as.factor(inputRacaCor2())) %>%
      filter(sigla_UF %in% as.factor(inputEstado()))
    return(novo_df)
  })
  
  novo_df_censo = reactive({
    censo_df = df_censo %>%
      filter(Porte %in% as.factor(inputPorte())) %>%
      filter(Capital %in% as.factor(inputCapital())) %>%
      filter(SEXO %in% as.factor(inputSexo())) %>%
      filter(FaixaEtaria %in% as.factor(inputIdade())) %>%
      filter(RACACOR %in% as.factor(inputRacaCor())) %>%
      filter(GrandeRegiao%in% as.factor(inputGrandeRegiao())) %>%
      filter(RACACOR2 %in% as.factor(inputRacaCor2())) %>%
      filter(sigla_UF %in% as.factor(inputEstado()))
      return(censo_df)
  })
  
  
  absoluto <- reactive({
    req(input$xcol)
    novo_df_filtrado() %>%
      group_by(ANO, !!!rlang::syms(input$xcol)) %>%
      summarise(Quantidade = n()) %>%
      group_by(ANO) %>%
      na.omit()
  })
  
  porcentagens <- reactive({
    req(input$xcol)
    novo_df_filtrado() %>%
      group_by(ANO, !!!rlang::syms(input$xcol)) %>%
      summarise(Quantidade = n()) %>%
      group_by(ANO) %>%
      mutate(Porcentagem = (Quantidade / sum(Quantidade)) * 100) %>%
      na.omit()
  })
  
  taxa_hom = reactive({
    req(input$xcol)
    df_taxa2() %>%
      filter(!is.na(taxa)) %>% 
      group_by(ANO, !!!rlang::syms(input$xcol)) %>%
      summarise(MediaTaxa = (sum(Quantidade)/sum(POPULAÇÃO)) * 100000)
  })
  
  populacao = reactive({
    req(input$xcol)
    novo_df_censo() %>% 
      filter(RACACOR != "Ignorado") %>% 
      group_by(!!!rlang::syms(input$xcol)) %>%
      summarise(Quant = sum(populacao))%>%
      na.omit()
  })
  
  taxa_hom2 = reactive({ 
    req(input$xcol)
    novo_df_filtrado() %>%
      group_by(ANO, !!!rlang::syms(input$xcol)) %>%
      summarise(Quantidade = n()) %>%
      na.omit() %>%
      group_by(ANO) %>%
      mutate(MediaTaxa = ((Quantidade/populacao()$Quant)*mean(novo_df_filtrado()$TaxaCresc)) * 100000)
  })
  
 homicidios.nb = reactive ({
   nb = municipios_hom_filtrado() %>% 
        poly2nb(municipios_hom_filtrado(), queen = F)
  return(nb)
   
 })
 

 res = reactive ({ 
   res = EBlocal(municipios_hom_filtrado()$Quantidade,  
                 municipios_hom_filtrado()$POPULAÇÃO, homicidios.nb(), zero.policy = T) 
  return(res)
  })
 
resglobal = reactive ({ 
   res = EBest(municipios_hom_filtrado()$Quantidade,  
                 municipios_hom_filtrado()$POPULAÇÃO) 
   return(res)
})
 
 
 homicidios_final = reactive({
   # Filtrando os municípios
   mun <- municipios_hom_filtrado()
   mun$taxahom <- res()$est * 100000
   mun$taxahom_brks <- cut(as.numeric(mun$taxahom),
     breaks = c(-1, 5, 10, 15, 20, 25, 30, 35, 40, 1000),
     labels = c("0 - 5", "5 - 10", "10 - 15", "15 - 20", "20 - 25", "25 - 30", "30 - 35", "35 - 40", "40+")
   )
   mun$taxahomglobal =  resglobal()$est* 100000
   return(mun)
 })
  
  output$plot_absoluto <- renderPlotly({
    validate(
      need(inputPorte, "please select a porte")
    )
    req(input$xcol)
    g1 = ggplot(absoluto(), 
                aes_string(x = absoluto()$ANO, y = absoluto()$Quantidade, color = input$xcol, group = input$xcol)) +
      geom_line() +
      geom_text(aes(label = paste0(format(Quantidade, digits = 2)))) +
      geom_point(size = 2, stroke = 0, shape = 16) +
      labs(x = "Ano", y = "Total (N)") +
      scale_x_continuous(breaks = unique(absoluto()$ANO))
    ggplotly(g1, tooltip = c("x", "y"),
             width = (as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2])) %>% 
      style(textposition = "top center")
    
  })
  
  output$plot_relativo <- renderPlotly({
    req(input$xcol)
    g1 = ggplot(porcentagens(), 
                aes_string(x = porcentagens()$ANO,
                           y = round(porcentagens()$Porcentagem,2), color = input$xcol, group = input$xcol)) +
      geom_line() +
      geom_text(aes(label = paste0(format(Porcentagem, digits = 2), "%")),
                position = position_dodge(width = 1), vjust = -3.5, size = 4, check_overlap = TRUE) +
      geom_point(size = 2, stroke = 0, shape = 16) +
      labs(x = "Ano", y = "Porcentagem (%)") +
      scale_x_continuous(breaks = unique(porcentagens()$ANO))
    ggplotly(g1, tooltip = c("x", "y"),width = (as.numeric(input$dimension[1])), 
             height = as.numeric(input$dimension[2]))%>% 
      style(textposition = "top center")
    
  })
  
  output$plot_taxahom <-  renderPlotly({
    validate(
      need(inputPorte, "please select a porte")
    )
    req(input$xcol)
    g1 = ggplot(taxa_hom2(), 
                aes_string(x = taxa_hom2()$ANO, y = taxa_hom2()$MediaTaxa, color = input$xcol, group = input$xcol)) +
      geom_line() +
      geom_text(aes(label = round(MediaTaxa, 2)), hjust = -0.2, vjust = 0) +
      geom_point(size = 2, stroke = 0, shape = 16) +
      labs(x = "Ano", y = "Taxa a cada 100 mil hab.") +
      scale_x_continuous(breaks = unique(taxa_hom2()$ANO))
    ggplotly(g1, tooltip = c("Ano:", "y"),width = (as.numeric(input$dimension[1])), 
             height = as.numeric(input$dimension[2]))%>% 
      style(textposition = "top center")
    
    
  })
  
  output$mapa_int <- renderPlot({
    ggplot() +
      geom_sf(data = homicidios_final(), aes(fill = homicidios_final()$taxahom_brks), color=NA) +
      labs(title = "Homicidios por municipio a cada 100 mil habitantes",fill='Faixa de Taxas de Homicidios') +
      theme(plot.title = element_text(hjust = 0.5, size = 20,face="bold")) +
      scale_fill_brewer(palette = "YlOrRd",name="Taxa de Homicidios",
                        na.value = "grey100")}, height = 460)
  
  output$mapa_basico <- renderPlot({
    ggplot() +
      geom_sf(data = homicidios_final(), aes(fill = homicidios_final()$taxa), color=NA) +
      labs(title = "Homicidios por municipio a cada 100 mil habitantes") +
      theme(plot.title = element_text(hjust = 0.5, size = 20,face="bold"))+
      scale_fill_distiller(palette = "YlOrRd",limits = c(0, 80),
                           breaks = seq(0, 80, by = 10), na.value = "#bd0026",labels = c("0","10","20","30","40","50","60","70","80+"),name="Taxa de Homicidios", direction = 1)},height = 460)
  
  output$mapa_local <- renderPlot({
    ggplot() +
      geom_sf(data = homicidios_final(), aes(fill = homicidios_final()$taxahom), color=NA) +
      labs(title = "Homicidios por municipio a cada 100 mil habitantes") +
      theme(plot.title = element_text(hjust = 0.5, size = 20,face="bold"))+
      scale_fill_distiller(palette = "YlOrRd",limits = c(0, 80),
                           breaks = seq(0, 80, by = 10), na.value = "#bd0026",labels = c("0","10","20","30","40","50","60","70","80+"),name="Taxa de Homicidios", direction = 1)},height = 460)
  
  output$mapa_global <- renderPlot({
    ggplot() +
      geom_sf(data = homicidios_final(), aes(fill = homicidios_final()$taxahomglobal), color=NA) +
      labs(title = "Homicidios por municipio a cada 100 mil habitantes") +
      theme(plot.title = element_text(hjust = 0.5, size = 20,face="bold"))+
      scale_fill_distiller(palette = "YlOrRd", limits = c(0, 80),
                           breaks = seq(0, 80, by = 10), na.value = "#bd0026",labels = c("0","10","20","30","40","50","60","70","80+"),name="Taxa de Homicidios", direction = 1)}, height = 460)
  
}
