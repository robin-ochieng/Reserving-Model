# Landing Page Module
landingPageUI <- function(id) {
  ns <- NS(id)
  
  div(class = "main-container",
    div(class = "header-section",
      div(class = "sacos-logo",
        tags$img(src = "images/saccos_logo.png", alt = "SACOS Logo", 
                style = "max-height: 80px; max-width: 200px; object-fit: contain;")
      ),
      div(class = "company-name", "SACOS Group Limited"),
      div(class = "tagline", "Here for you"),
      div(class = "valuation-title", "Valuation of Claims Liabilities"),
      
      tags$table(class = "info-table",
        tags$tr(
          tags$td("Start Date"),
          tags$td("01/01/2013")
        ),
        tags$tr(
          tags$td("Valuation Date"),
          tags$td("31/12/2024")
        )
      )
    )
  )
}

landingPageServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Landing page logic here
  })
}
