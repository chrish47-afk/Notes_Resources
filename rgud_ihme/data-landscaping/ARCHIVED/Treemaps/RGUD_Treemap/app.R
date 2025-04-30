#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#install.packages("shiny")
#install.packages("treemapify")
# Load required packages
library(shiny)
library(treemap, lib.loc = "/homes/chrish47/R_packages_cluster/")
library(dplyr)
library(treemapify)
library(shiny)
library(plotly)
library(shinydashboard)


# Load the GNI2014 dataset
data(GNI2014)

# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "GNI2014 Treemap"),
  dashboardSidebar(
    selectInput("continent", "Continent", choices = c("All", levels(GNI2014$continent)))
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "Treemap",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("treemap")
      )
    )
  )
)

# Define the server
server <- function(input, output) {
  
  # Create the treemap plot
  output$treemap <- renderPlot({
    if(input$continent == "All") {
      # Plot the entire dataset
      treemap(GNI2014, index = c("continent", "country"), vSize = "GNI", type = "value", title = "GNI2014")
    } else {
      # Filter the dataset by the selected continent
      subset_data <- subset(GNI2014, continent == input$continent)
      treemap(subset_data, index = "country", vSize = "GNI", type = "value", title = paste0(input$continent, " GNI2014"))
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)

