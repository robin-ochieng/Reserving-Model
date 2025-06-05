# SACOS Group Limited - Reserving Model Application
# Using Shiny Fluent for modern UI components

# Load required libraries
library(shiny)
library(shiny.fluent)
library(htmltools)
library(DT)

# Load custom modules
source("Modules/landingPageModule.R")
source("Modules/dataModule.R")
source("Modules/dashboardModule.R")
source("Modules/claimPlotsModule.R")
source("Modules/reportedDataModule.R")


# Main UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom_styles.css"),
    tags$title("SACOS Reserving Model")
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
        key = "dashboard",
        headerText = "Dashboard", 
        itemIcon = "ViewDashboard",
        dashboardModuleUI("dashboard")
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
  dataModuleServer("data")
  dashboardModuleServer("dashboard")
  claimPlotsModuleServer("plots")
  reportedDataModuleServer("reported")
}

# Run the application
shinyApp(ui = ui, server = server)