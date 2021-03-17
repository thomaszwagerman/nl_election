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
                                                plotOutput("pie"),
                                                #you then construct the page chronologically, so under the navigation bar you'll have the Validate Data tab.
                                                sidebarPanel(
                                                  #within the Validate data tab there is a mainPanel (this just means in the centre of the page).
                                                  # you can also have a leftPanel if  you want everything on the left hand side.
                                                  fluidRow(
                                                    #fluidRow is just ui generic text in this case.
                                                    #as the first row, it will be at the top of the main panel
                                                    helpText("F")
                                                  ),
                                                  
                                                )),
                         ))) 


server <- shinyServer(function(input, output, session) {
  
  output$plot <- renderGirafe({
    girafe(ggobj = g,
           options = list(opts_selection(type = "single")))
  })
  
  observeEvent(input$plot_selected, {
    xx <- df_pie %>% 
      filter(regio == input$plot_selected) %>%
      dplyr::ungroup() %>% 
      select(party,percentage) %>% 
      as.data.frame()
    
    xx <- xx[order(xx$percentage,decreasing = T),]
    xx <- left_join(xx,colourscheme)
    xx <- xx %>% 
      filter(percentage >0.1)
    
    output$pie <-  renderPlot({
      piechart <- ggplot(xx) +
        geom_bar(aes(x = "", y = percentage, fill = party),
                 stat = "identity", colour = "black", width =1) +
        #geom_text(aes(x = "", y = pct, label = percent(pct)), position = position_stack(vjust = 0.5))+
        coord_polar("y", start = 0) +
        labs(x = NULL, y = NULL, fill = NULL, 
             title = "Verkiezingsuitslag") +
        theme_classic()+
        scale_fill_manual(values = xx$colour, 
                          limits = xx$party)+ 
        theme(axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              plot.title = element_text(hjust = 0.5, color = "black"))
      piechart
    }) 
    
    
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)