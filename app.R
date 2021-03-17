library(shiny)
library(shinyjs)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(ggiraph)

source("./fptp_election_map.R")
# Define UI for the app
ui <- shinyUI(navbarPage("FPTP ", theme=shinytheme("united"), position = "fixed-top",
                         tabPanel("Verkiezingsuitlag Europa 2017",useShinyjs(),
                                  absolutePanel(top = 50, left = 10, right = 0, bottom = 0,width = "auto", height = "auto",
                                                #br(),
                                                h2("Europese Verkiezingsuitslag 2017", align = "center"),
                                                girafeOutput("plot"),
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
    browser()
  })
})

# Run the application 
shinyApp(ui = ui, server = server)