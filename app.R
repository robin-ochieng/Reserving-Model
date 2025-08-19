# SACOS Group Limited - Reserving Model Application
# Using Shiny Fluent for modern UI components

# app.R
# Load required libraries
library(shiny)
library(shiny.fluent)
library(htmltools)
library(DT)
library(shinycssloaders)
library(readxl)
library(shinyjs)
library(dplyr)
library(tidyverse) 
library(lubridate)
library(plotly)

# Load custom modules
source("Modules/landingPageModule.R")
source("Modules/dataModule.R")
source("Modules/claimPlotsModule.R")
source("Modules/reportedDataModule.R")


# Main UI
ui <- fluidPage(
  tags$head(
    # Include Font Awesome for icons
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon/saccos_logo.ico"),
    tags$link(rel = "shortcut icon", type = "image/x-icon", href = "favicon/saccos_logo.ico"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/claimPlots.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom_styles.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/landingPage.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/dataModule.css"),
    tags$title("SACOS Reserving Model"),
    # Add custom JavaScript for download functionality
    tags$script(HTML("
      Shiny.addCustomMessageHandler('downloadFile', function(message) {
        const link = document.createElement('a');
        link.href = message.dataUri;
        link.download = message.filename;
        document.body.appendChild(link);
        link.click();
        document.body.removeChild(link);
      });
    ")),
  
  # Include external custom JavaScript file
  tags$script(src = "js/customjs.js")
  ),
  
  div(class = "main-container",
    # Navigation using Fluent UI Pivot
    Pivot(
      inputId = "mainNavigation",
      style = list(marginBottom = "20px"),
      PivotItem(
        key = "home",
        headerText = "Home",
        itemIcon = "Home",
        landingPageUI("landing")
      ),
      PivotItem(
        key = "data",
        headerText = "Data",
        itemIcon = "Database",
        dataModuleUI("data")
      ),
      PivotItem(
        key = "plots",
        headerText = "Claim Plots",
        itemIcon = "LineChart",
        claimPlotsModuleUI("plots")
      ),
      PivotItem(
        key = "reported",
        headerText = "Reported Data",
        itemIcon = "Table",
        reportedDataModuleUI("reported")
      )
    )
  )
)

# Main Server
server <- function(input, output, session) {
  # Initialize modules
  landingPageServer("landing")
  data_module <- dataModuleServer("data")
  claimPlotsModuleServer("plots", data_module = data_module)
  reportedDataModuleServer("reported", data_module = data_module)
}

# Run the application
shinyApp(ui = ui, server = server)