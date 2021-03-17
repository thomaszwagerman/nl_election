library(shiny)
library(shinyjs)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(ggiraph)

source("./pie_chart.R")
source("./fptp_election_map.R")
# Define UI for the app
ui <- shinyUI(navbarPage("FPTP ", theme=shinytheme("united"), position = "fixed-top",
                         tabPanel("Verkiezingsuitlag Tweede Kamer 2017",useShinyjs(),
                                  absolutePanel(top = 50, left = 10, right = 0, bottom = 0,width = "auto", height = "auto",
                                                #br(),
                                                h2("Verkiezingsuitlag Tweede Kamer 2017", align = "center"),
                                                girafeOutput("plot"),
                                                plotOutput("pie")
                                                #you then construct the page chronologically, so under the navigation bar you'll have the Validate Data tab.
                                                
                                  )# end of absolutepanel
                         )# end of tabpanel
)# end of navbarpage
)# end of UI


server <- shinyServer(function(input, output, session) {
  
  #render gemeente map
  output$plot <- renderGirafe({
    girafe(ggobj = g,
           options = list(opts_selection(type = "single")))
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