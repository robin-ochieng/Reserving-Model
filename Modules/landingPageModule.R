# Landing Page Module
landingPageUI <- function(id) {
  ns <- NS(id)
  
  div(class = "main-container",
    div(class = "header-section",
      div(class = "sacos-logo",
        tags$img(src = "images/saccos_logo.png", alt = "SACOS Logo", 
                style = "max-height: 80px; max-width: 200px; object-fit: contain; transform: none; display: block; margin: 0 auto;")
      ),
      div(class = "company-name", "SACOS Group Limited"),
      div(class = "valuation-title", "Valuation of Claims Liabilities"),
      
      div(class = "date-inputs-container",
        div(class = "date-input-row",
          div(class = "date-label", "Start Date:"),
          div(class = "date-picker-wrapper",
            DatePicker.shinyInput(
              inputId = "startDate",
              value = as.Date("2013-01-01"),
              style = list(width = "200px")
            )
          )
        ),
        div(class = "date-input-row",
          div(class = "date-label", "Valuation Date:"),
          div(class = "date-picker-wrapper",
            DatePicker.shinyInput(
              inputId = "valuationDate", 
              value = as.Date("2024-12-31"),
              style = list(width = "200px")
           )
          )
        ),
        # Professional Footer
        div(
          class = "app-footer",
          div(class = "footer-content",
            div(class = "footer-left",
              tags$span("© 2024 SACOS Group Limited. All rights reserved.")
            ),
            div(class = "footer-right",
              tags$span("Powered by "),
              tags$img(
                src = "images/kenbright.png", 
                alt = "Kenbright AI",
                class = "footer-logo"
              )
            )
          )
        )
      )
    )
  )
}

landingPageServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Reactive values for dates that can be used by other modules
    dates <- reactiveValues(
      start = as.Date("2013-01-01"),
      valuation = as.Date("2024-12-31")
    )
    
    # Update reactive values when user changes dates
    observeEvent(input$startDate, {
      dates$start <- input$startDate
    })
    
    observeEvent(input$valuationDate, {
      dates$valuation <- input$valuationDate
    })
    
    # Return dates for use in other modules
    return(dates)
  })
}