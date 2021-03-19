library(shiny)
library(shinyjs)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(mapview)
library(leaflet)

source("./pie_chart.R")
source("./pr_calculations.R")
source("./fptp_election_map.R")
source("./uk_fptp_dhondt_comparison.R")
# Define UI for the app
ui <- shinyUI(navbarPage("Verkiezingen Nederland: D'Hondt v FPTP", theme=shinytheme("united"), position = "fixed-top",
                         tabPanel("Verkiezingsuitlag Tweede Kamer 2017",useShinyjs(),
                                  absolutePanel(top = 50, left = 10, right = 0, bottom = 0,width = "auto", height = "auto",
                                                #br(),
                                                h2("Verkiezingsuitlag Tweede Kamer 2017", align = "center"),
                                                sidebarPanel(
                                                  h2("Een ode aan het D'Hondt systeem"),
                                                  tags$p(HTML("<b>Verkiezingsuitlag 2017</b> aan de hand van het D'Hondt systeem, het huidige systeem.")),
                                                ),
                                                mainPanel(plotOutput("parlement"),),
                                                #you then construct the page chronologically, so under the navigation bar you'll have the Validate Data tab.
                                                
                                  )# end of absolutepanel
                         ),# end of tabpanel
                         tabPanel("Alternatieve FPTP Verkiezingsuitlag Tweede Kamer 2017",useShinyjs(),
                                  absolutePanel(top = 50, left = 10, right = 0, bottom = 0,width = "auto", height = "auto",
                                                #br(),
                                                h2("Verkiezingsuitlag Tweede Kamer 2017 met FPTP systeem", align = "center"),
                                                sidebarPanel(
                                                  h2("De potentiele hel van het FPTP systeem"),
                                                  tags$p(HTML("<b>Verkiezingsuitlag 2017</b> aan de hand van het <b>first-past-the-post (FPTP)</b> systeem, gebruikelijk in het VK.")),
                                                  tags$p(HTML("Hier is <b>een zetel per gemeente</b> verdeelt.")),
                                                  tags$p(HTML("Dit is natuurlijk niet hoe het in de werkelijkheid er aan toe zou gaan.")),
                                                  tags$p(HTML("Immers werken ze in het VK met <b>constituencies</b> die enigzins op bevolking zijn aangepast.")),
                                                  
                                                ),#end of sidebarpanel
                                                mainPanel(
                                                  plotOutput("fptp_parlement"),
                                                  girafeOutput("plot"),
                                                  plotOutput("pie"),
                                                ),#end of mainpanel
                                                #you then construct the page chronologically, so under the navigation bar you'll have the Validate Data tab.
                                                
                                  )# end of absolutepanel
                                  
                         ),# end of tabpanel
                         tabPanel("Vergelijken UK General Election 2019: D'Hondt vs. FPTP",useShinyjs(),
                                  absolutePanel(top = 50, left = 10, right = 0, bottom = 0,width = "auto", height = "auto",
                                                #br(),
                                                h2("Verkiezingsuitlag Tweede Kamer 2017 met FPTP systeem", align = "center"),
                                                sidebarPanel(
                                                  h2("D'Hondt vs FPTP"),
                                                ),#end of sidebarpanel
                                                mainPanel(
                                                  plotOutput("fptp_uk"),
                                                  plotOutput("pr_uk")
                                                ),#end of mainpanel
                                                #you then construct the page chronologically, so under the navigation bar you'll have the Validate Data tab.
                                                
                                  )# end of absolutepanel
                                  
                         ),
                         tabPanel("Test with mapviewplot",useShinyjs(),
                                  absolutePanel(top = 50, left = 10, right = 0, bottom = 0,width = "auto", height = "auto",
                                                mainPanel(
                                                  leafletOutput("mapviewplot",width="80%",height="400px"),
                                                  
                                                ),
                                  )
                         )
)# end of navbarpage
)# end of UI


server <- shinyServer(function(input, output, session) {
  
  #render gemeente map
  output$plot <- renderGirafe({
    girafe(ggobj = g,
           options = list(opts_selection(type = "single")))
  })
  
  output$parlement <- renderPlot({
    representatives_pr
  })
  
  output$fptp_parlement <- renderPlot({
    representatives_fptp
  })
  
  output$fptp_uk <- renderPlot({
    hoc_fptp
  })
  
  output$pr_uk <- renderPlot({
    hoc_pr
  })
  
  output$mapviewplot <- renderLeaflet({
    
    #shpfile order will need to be the same as the pie plot order (gemeentes have changes, so not currently the case)
    # shp_fptp <- shp_fptp[order(shp_fptp$regio),]
    # x <- 1:length(shp_fptp$regio)
    # rownames(shp_fptp) <- x
    
    mapview(shp_fptp,
            zcol = "party",
            col.regions = shp_fptp$colour,
            popup = popupGraph(pie_plot_list))
  })
  
  #render piechart based on selection
  observeEvent(input$plot_selected, {
    #prepare data for plotting based on gemeente selection
    xx <- df_pie %>% 
      filter(regio == input$plot_selected) %>%
      dplyr::ungroup() %>% 
      select(party,percentage) %>% 
      as.data.frame()
    xx <- xx[order(xx$percentage,decreasing = T),]
    xx <- left_join(xx,colourscheme)
    xx <- xx %>% 
      filter(percentage >0.1)
    
    #render plot based on gemeente selection
    output$pie <-  renderPlot({
      piechart <- ggplot(xx) +
        geom_bar(aes(x = "", y = percentage, fill = party),
                 stat = "identity", colour = "black", width =1) +
        #geom_text(aes(x = "", y = pct, label = percent(pct)), position = position_stack(vjust = 0.5))+
        coord_polar("y", start = 0) +
        labs(x = NULL, y = NULL, fill = NULL, 
             title = paste("Verkiezingsuitslag",input$plot_selected)) +
        theme_classic()+
        scale_fill_manual(values = xx$colour, 
                          limits = xx$party)+ 
        theme(axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              plot.title = element_text(hjust = 0.5, color = "black"))
      piechart
    }) # end of render plot 
    

  }) # end of observeevent
})# end of server

# Run the application 
shinyApp(ui = ui, server = server)