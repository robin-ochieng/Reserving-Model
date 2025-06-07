# Data Module UI
dataModuleUI <- function(id) {
  ns <- NS(id)
  
  div(class = "content-section",
    # Simple Header
    div(class = "data-header",
      Text("Claims Data Management", variant = "xLarge", 
           style = list(fontWeight = "600", color = "#1e6bb8", marginBottom = "10px")),
           variant = "medium", style = list(color = "#666", marginBottom = "30px")),
    
    
    # Simple Status Row
    div(class = "simple-status-row",
      div(class = "status-item",
        Icon(iconName = "Money", style = list(fontSize = "20px", color = "#107c10")),
        div(class = "status-info",
          Text("Paid Claims", style = list(fontWeight = "500")),
          uiOutput(ns("paidStatus"))
        )
      ),
      div(class = "status-item",
        Icon(iconName = "Warning", style = list(fontSize = "20px", color = "#d13438")),
        div(class = "status-info",
          Text("Outstanding Claims", style = list(fontWeight = "500")),
          uiOutput(ns("outstandingStatus"))
        )
      ),
      div(class = "status-item",
        Icon(iconName = "ReportDocument", style = list(fontSize = "20px", color = "#8764b8")),
        div(class = "status-info",
          Text("Reported Claims", style = list(fontWeight = "500")),
          uiOutput(ns("reportedStatus"))
        )
      ),
      div(class = "status-item",
        DefaultButton(
          inputId = ns("refreshData"),
          text = "Refresh All",
          iconProps = list(iconName = "Refresh")
        )
      )
    ),
    
    # Simple Tabs for Each Dataset
    Pivot(
      inputId = ns("claimsTabs"),
      style = list(marginTop = "30px"),
      
      # Paid Claims Tab
      PivotItem(
        key = "paid",
        headerText = "Paid Claims",
        itemIcon = "Money",
        div(
          uiOutput(ns("paidCards")),
          br(),
          div(class = "simple-table-container",
            div(class = "table-title",
              Text("Paid Claims Data", variant = "large", style = list(fontWeight = "500")),
              downloadButton(ns("downloadPaid"), "Download Excel", 
                           class = "btn btn-primary", 
                           style = "background-color: #0078d4; border: none;")
            ),
            DT::dataTableOutput(ns("paidTable"))
          )
        )
      ),
      
      # Outstanding Claims Tab
      PivotItem(
        key = "outstanding",
        headerText = "Outstanding Claims", 
        itemIcon = "Warning",
        div(
          uiOutput(ns("outstandingCards")),
          br(),
          div(class = "simple-table-container",
            div(class = "table-title",
              Text("Outstanding Claims Data", variant = "large", style = list(fontWeight = "500")),
              downloadButton(ns("downloadOutstanding"), "Download Excel", 
                           class = "btn btn-primary",
                           style = "background-color: #0078d4; border: none;")
            ),
            DT::dataTableOutput(ns("outstandingTable"))
          )
        )
      ),
      
      # Reported Claims Tab
      PivotItem(
        key = "reported",
        headerText = "Reported Claims",
        itemIcon = "ReportDocument", 
        div(
          uiOutput(ns("reportedCards")),
          br(),
          div(class = "simple-table-container",
            div(class = "table-title",
              Text("Reported Claims Data", variant = "large", style = list(fontWeight = "500")),
               downloadButton(ns("downloadReported"), "Download Excel", 
                           class = "btn btn-primary",
                           style = "background-color: #0078d4; border: none;")
            ),
            DT::dataTableOutput(ns("reportedTable"))
          )
        )
      )
    ),
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
              ),
              tags$span(" AI")
            )
          )
        )     
  )
 
}

# Data Module Server
dataModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Simple reactive values
    paid_data <- reactiveVal(NULL)
    outstanding_data <- reactiveVal(NULL)
    reported_data <- reactiveVal(NULL)
    
    # Simple function to create summary cards
    make_summary_cards <- function(data, type) {
      if(is.null(data) || nrow(data) == 0) return(NULL)
      
      total_claims <- nrow(data)
      
      # Calculate amounts based on data type
      if(type == "paid") {
        main_amount <- sum(data$`Amount Paid`, na.rm = TRUE)
        net_amount <- sum(data$`Net Amount`, na.rm = TRUE) 
        avg_amount <- mean(data$`Net Amount`, na.rm = TRUE)
        main_label <- "Total Paid"
        icon_name <- "Money"
        icon_color <- "#107c10"
      } else if(type == "outstanding") {
        main_amount <- sum(data$`Amount O/S`, na.rm = TRUE)
        net_amount <- sum(data$`Net Amount`, na.rm = TRUE)
        avg_amount <- mean(data$`Net Amount`, na.rm = TRUE)
        main_label <- "Total Outstanding"
        icon_name <- "Warning"
        icon_color <- "#d13438"
      } else { # reported
        main_amount <- sum(data$`Gross Amount`, na.rm = TRUE)
        net_amount <- sum(data$`Net Claims`, na.rm = TRUE)
        avg_amount <- mean(data$`Net Claims`, na.rm = TRUE)
        main_label <- "Gross Amount"
        icon_name <- "ReportDocument"
        icon_color <- "#8764b8"
      }
      
      # Create the cards
      div(class = "simple-cards-row",
        div(class = "summary-card",
          Icon(iconName = "NumberField", style = list(fontSize = "24px", color = "#0078d4")),
          div(
            div(class = "card-number", format(total_claims, big.mark = ",")),
            div(class = "card-label", "Total Claims")
          )
        ),
        div(class = "summary-card",
          Icon(iconName = icon_name, style = list(fontSize = "24px", color = icon_color)),
          div(
            div(class = "card-number", paste0("$", format(round(main_amount/1000000, 1), big.mark = ","), "M")),
            div(class = "card-label", main_label)
          )
        ),
        div(class = "summary-card",
          Icon(iconName = "CalculatorAddition", style = list(fontSize = "24px", color = "#8764b8")),
          div(
            div(class = "card-number", paste0("$", format(round(net_amount/1000000, 1), big.mark = ","), "M")),
            div(class = "card-label", if(type == "reported") "Net Claims" else "Net Amount")
          )
        ),
        div(class = "summary-card",
          Icon(iconName = "BarChart4", style = list(fontSize = "24px", color = "#6264a7")),
          div(
            div(class = "card-number", paste0("$", format(round(avg_amount/1000, 0), big.mark = ","), "K")),
            div(class = "card-label", "Average")
          )
        )
      )
    }
    
    # Simple function to create data tables
    make_data_table <- function(data, type) {
      if(is.null(data)) {
        return(DT::datatable(
          data.frame(Message = paste("No", type, "data available")), 
          options = list(dom = 't'), rownames = FALSE
        ))
      }
      
      # Currency column positions based on type
      if(type == "paid") {
        currency_cols <- c(9, 10, 11)  # Amount Paid, RI Amount, Net Amount
      } else if(type == "outstanding") {
        currency_cols <- c(9, 10, 11, 12)  # Amount O/S, Gross OS Claims Adjusted, Reinsurance, Net Amount
      } else { # reported
        currency_cols <- c(9, 10, 11, 12)  # Gross Amount, Gross Claims Adjusted, RI Amount, Net Claims
      }
      
      DT::datatable(
        data,
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          scrollY = "350px",
          columnDefs = list(
            # Format currency columns
            list(targets = currency_cols, render = JS(
              "function(data, type, full, meta) {",
              "  if(type === 'display' && data != null) {",
              "    return '$' + parseFloat(data).toLocaleString('en-US', {minimumFractionDigits: 2, maximumFractionDigits: 2});",
              "  }",
              "  return data;",
              "}"
            )),
            # Format date columns  
            list(targets = c(6, 7, 8), render = JS(
              "function(data, type, full, meta) {",
              "  if(type === 'display' && data != null) {",
              "    return new Date(data).toLocaleDateString('en-US');",
              "  }",
              "  return data;",
              "}"
            ))
          )
        ),
        class = 'cell-border stripe hover',
        rownames = FALSE,
        filter = 'top'
      )
    }
    
    # Load all data when app starts
    observe({
      # Load Paid Claims
      if(file.exists("Data/Paid Claims.xlsx")) {
        tryCatch({
          paid <- read_excel("Data/Paid Claims.xlsx")
          paid_data(paid)
        }, error = function(e) {
          showNotification(paste("Error loading paid claims:", e$message), type = "error")
        })
      }
      
      # Load Outstanding Claims
      if(file.exists("Data/Outstanding Claims.xlsx")) {
        tryCatch({
          outstanding <- read_excel("Data/Outstanding Claims.xlsx")
          outstanding_data(outstanding)
        }, error = function(e) {
          showNotification(paste("Error loading outstanding claims:", e$message), type = "error")
        })
      }
      
      # Load Reported Claims
      if(file.exists("Data/Reported Claims.xlsx")) {
        tryCatch({
          reported <- read_excel("Data/Reported Claims.xlsx")
          reported_data(reported)
        }, error = function(e) {
          showNotification(paste("Error loading reported claims:", e$message), type = "error")
        })
      }
    })
    
    # Refresh all data
    observeEvent(input$refreshData, {
      showNotification("Refreshing all data...", type = "message")
      paid_data(NULL)
      outstanding_data(NULL) 
      reported_data(NULL)
      invalidateLater(1000)
    })
    
    # Status outputs
    output$paidStatus <- renderUI({
      if(!is.null(paid_data())) {
        Text(paste("✓", format(nrow(paid_data()), big.mark = ","), "records"), 
             style = list(color = "#107c10", fontSize = "12px"))
      } else {
        Text("⚠ No data", style = list(color = "#d13438", fontSize = "12px"))
      }
    })
    
    output$outstandingStatus <- renderUI({
      if(!is.null(outstanding_data())) {
        Text(paste("✓", format(nrow(outstanding_data()), big.mark = ","), "records"), 
             style = list(color = "#107c10", fontSize = "12px"))
      } else {
        Text("⚠ No data", style = list(color = "#d13438", fontSize = "12px"))
      }
    })
    
    output$reportedStatus <- renderUI({
      if(!is.null(reported_data())) {
        Text(paste("✓", format(nrow(reported_data()), big.mark = ","), "records"), 
             style = list(color = "#107c10", fontSize = "12px"))
      } else {
        Text("⚠ No data", style = list(color = "#d13438", fontSize = "12px"))
      }
    })
    
    # Summary cards outputs
    output$paidCards <- renderUI({
      make_summary_cards(paid_data(), "paid")
    })
    
    output$outstandingCards <- renderUI({
      make_summary_cards(outstanding_data(), "outstanding")
    })
    
    output$reportedCards <- renderUI({
      make_summary_cards(reported_data(), "reported")
    })
    
    # Data table outputs
    output$paidTable <- DT::renderDataTable({
      make_data_table(paid_data(), "paid")
    })
    
    output$outstandingTable <- DT::renderDataTable({
      make_data_table(outstanding_data(), "outstanding")
    })
    
    output$reportedTable <- DT::renderDataTable({
      make_data_table(reported_data(), "reported")
    })
    
    # Download handlers for actual file downloads
    output$downloadPaid <- downloadHandler(
      filename = function() {
        paste("SACOS_Paid_Claims_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        if(!is.null(paid_data())) {
          tryCatch({
            # Create a temporary workbook
            wb <- openxlsx::createWorkbook()
            openxlsx::addWorksheet(wb, "Paid Claims")
            openxlsx::writeData(wb, "Paid Claims", paid_data())
            
            # Add some basic formatting
            openxlsx::addStyle(wb, "Paid Claims", 
                             style = openxlsx::createStyle(textDecoration = "bold"),
                             rows = 1, cols = 1:ncol(paid_data()))
            
            # Save the file
            openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
            
            showNotification("Paid claims data exported successfully!", type = "message")
          }, error = function(e) {
            showNotification(paste("Export error:", e$message), type = "error")
          })
        } else {
          showNotification("No paid claims data to export", type = "warning")
        }
      }
    )
    
    output$downloadOutstanding <- downloadHandler(
      filename = function() {
        paste("SACOS_Outstanding_Claims_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        if(!is.null(outstanding_data())) {
          tryCatch({
            wb <- openxlsx::createWorkbook()
            openxlsx::addWorksheet(wb, "Outstanding Claims")
            openxlsx::writeData(wb, "Outstanding Claims", outstanding_data())
            
            openxlsx::addStyle(wb, "Outstanding Claims", 
                             style = openxlsx::createStyle(textDecoration = "bold"),
                             rows = 1, cols = 1:ncol(outstanding_data()))
            
            openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
            
            showNotification("Outstanding claims data exported successfully!", type = "message")
          }, error = function(e) {
            showNotification(paste("Export error:", e$message), type = "error")
          })
        } else {
          showNotification("No outstanding claims data to export", type = "warning")
        }
      }
    )
    
    output$downloadReported <- downloadHandler(
      filename = function() {
        paste("SACOS_Reported_Claims_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        if(!is.null(reported_data())) {
          tryCatch({
            wb <- openxlsx::createWorkbook()
            openxlsx::addWorksheet(wb, "Reported Claims")
            openxlsx::writeData(wb, "Reported Claims", reported_data())
            
            openxlsx::addStyle(wb, "Reported Claims", 
                             style = openxlsx::createStyle(textDecoration = "bold"),
                             rows = 1, cols = 1:ncol(reported_data()))
            
            openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
            
            showNotification("Reported claims data exported successfully!", type = "message")
          }, error = function(e) {
            showNotification(paste("Export error:", e$message), type = "error")
          })
        } else {
          showNotification("No reported claims data to export", type = "warning")
        }
      }
    )    
    # Return all data for other modules
    return(list(
      paid = paid_data,
      outstanding = outstanding_data,
      reported = reported_data
    ))
  })
}