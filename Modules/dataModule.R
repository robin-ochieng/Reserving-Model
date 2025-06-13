# Data Module UI with File Upload
# Required libraries
library(shiny)
library(shiny.fluent)
library(DT)
library(readxl)
library(dplyr)
library(lubridate) # For year() function

dataModuleUI <- function(id) {
  ns <- NS(id)
  
  div(class = "content-section",
    # Simple Header
    div(class = "data-header",
      Text("Claims Data Management", variant = "xLarge",
           style = list(fontWeight = "600", color = "#1e6bb8", marginBottom = "10px"))
    ),
    
    # File Upload Section
    div(class = "upload-section",
      style = list(
        background = "#f3f2f1",
        padding = "20px",
        borderRadius = "8px",
        marginBottom = "20px"
      ),
      Stack(
        horizontal = FALSE,
        tokens = list(childrenGap = 15),
        Text("Upload Claims Data", variant = "large", style = list(fontWeight = "500", marginBottom = "10px")),
        
        # Upload buttons row
        div(
          style = list(display = "flex", gap = "20px", flexWrap = "wrap"),
          
        # Paid Claims Upload
        div(
          style = list(flex = "1", minWidth = "300px"),
          Stack(
            horizontal = FALSE,
            tokens = list(childrenGap = 12),
            Stack(
              horizontal = TRUE,
              tokens = list(childrenGap = 10),
              Icon(iconName = "ExcelDocument", style = list(fontSize = "24px", color = "#107c10")),
              Text("Paid Claims Data", variant = "medium", style = list(fontWeight = "600"))
            ),
            
            # Styled file input wrapper
            div(
              class = "custom-file-wrapper",
              fileInput(
                ns("uploadPaid"),
                label = NULL,
                accept = c(".xlsx", ".xls"),
                buttonLabel = HTML(paste0(
                  '<i class="ms-Icon ms-Icon--CloudUpload" style="margin-right: 8px;"></i>',
                  'Choose or Drop File'
                )),
                placeholder = "No file selected"
              )
            ),
            
            uiOutput(ns("paidUploadStatus"))
          )
        ),

        # Outstanding Claims Upload
        div(
          style = list(flex = "1", minWidth = "300px"),
          Stack(
            horizontal = FALSE,
            tokens = list(childrenGap = 12),
            Stack(
              horizontal = TRUE,
              tokens = list(childrenGap = 10),
              Icon(iconName = "ExcelDocument", style = list(fontSize = "24px", color = "#d13438")),
              Text("Outstanding Claims Data", variant = "medium", style = list(fontWeight = "600"))
            ),
            
            # Styled file input wrapper
            div(
              class = "custom-file-wrapper",
              fileInput(
                ns("uploadOutstanding"),
                label = NULL,
                accept = c(".xlsx", ".xls"),
                buttonLabel = HTML(paste0(
                  '<i class="ms-Icon ms-Icon--CloudUpload" style="margin-right: 8px;"></i>',
                  'Choose or Drop File'
                )),
                placeholder = "No file selected"
              )
            ),
            
            uiOutput(ns("outstandingUploadStatus"))
          )
        )
        ),
        
        # Clear data button
        div(
          style = list(marginTop = "10px"),
          DefaultButton(
            inputId = ns("clearData"),
            text = "Clear All Data",
            iconProps = list(iconName = "Delete")
          )
        )
      )
    ),
    
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
              Text("Paid Claims Data", variant = "large", style = list(fontWeight = "500"))
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
              Text("Outstanding Claims Data", variant = "large", style = list(fontWeight = "500"))
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
              Stack(
                horizontal = FALSE,
                Text("Reported Claims Data (Combined)", variant = "large", style = list(fontWeight = "500")),
                Text("Includes both Paid and Outstanding claims", variant = "small", style = list(color = "#605e5c"))
              )
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
    
    # Upload status reactive values
    paid_upload_status <- reactiveVal(NULL)
    outstanding_upload_status <- reactiveVal(NULL)
    
    # Simple function to create summary cards
    make_summary_cards <- function(data, type) {
      if(is.null(data) || nrow(data) == 0) return(NULL)
      
      total_claims <- nrow(data)
      
      # Calculate amounts based on data type
      if(type == "paid") {
        main_amount <- sum(data$`Gross Amount`, na.rm = TRUE)
        net_amount <- sum(data$`Net Amount`, na.rm = TRUE) 
        avg_amount <- mean(data$`Net Amount`, na.rm = TRUE)
        main_label <- "Total Paid"
        icon_name <- "Money"
        icon_color <- "#107c10"
      } else if(type == "outstanding") {
        main_amount <- sum(data$`Gross Amount`, na.rm = TRUE)
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
            div(class = "card-number", paste0("SCR ", format(round(main_amount/1000000, 1), big.mark = ","), "M")),
            div(class = "card-label", main_label)
          )
        ),
        div(class = "summary-card",
          Icon(iconName = "CalculatorAddition", style = list(fontSize = "24px", color = "#8764b8")),
          div(
            div(class = "card-number", paste0("SCR ", format(round(net_amount/1000000, 1), big.mark = ","), "M")),
            div(class = "card-label", if(type == "reported") "Net Claims" else "Net Amount")
          )
        ),
        div(class = "summary-card",
          Icon(iconName = "BarChart4", style = list(fontSize = "24px", color = "#6264a7")),
          div(
            div(class = "card-number", paste0("SCR ", format(round(avg_amount/1000, 0), big.mark = ","), "K")),
            div(class = "card-label", "Average")
          )
        )
      )
    }
    
    # Simple function to create data tables
    make_data_table <- function(data, type) {
      if(is.null(data)) {
        return(DT::datatable(
          data.frame(Message = paste("No", type, "data available. Please upload data files above.")), 
          options = list(dom = 't'), rownames = FALSE
        ))
      }
      
      # Currency column positions based on type
      if(type == "paid") {
        currency_cols <- c(9, 10, 11)  # Amount Paid, RI Amount, Net Amount
      } else if(type == "outstanding") {
        currency_cols <- c(9, 10, 11)  # Amount O/S, Gross OS Claims Adjusted, Reinsurance, Net Amount
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
              "    return 'SCR' + parseFloat(data).toLocaleString('en-US', {minimumFractionDigits: 2, maximumFractionDigits: 2});",
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
    
    # Function to combine paid and outstanding data to create reported data
    combine_to_reported <- function(paid, outstanding) {
      if(is.null(paid) && is.null(outstanding)) return(NULL)
      
      # Prepare paid data for combination
      paid_for_reported <- NULL
      if(!is.null(paid) && nrow(paid) > 0) {
        # Check which date columns exist and use appropriate ones
        paid_for_reported <- paid %>%
          mutate(
            # Handle different possible column names
            `Transaction Date` = if("Paid Date" %in% names(.)) `Paid Date` 
                                else if("Transaction Date" %in% names(.)) `Transaction Date`
                                else if("Paid Year" %in% names(.)) as.Date(paste0(`Paid Year`, "-01-01"))
                                else as.Date(NA),
            `Notification Date` = if("Reported Date" %in% names(.)) `Reported Date`
                                 else if("Notification Date" %in% names(.)) `Notification Date`
                                 else `Loss Date`,
            `Gross Amount` = `Gross Amount`,
            `RI Amount` = `RI Amount`,
            `Net Claims` = `Net Amount`,
            Claim_Type = "Paid",
            # Ensure all required columns exist
            `Business Class` = if("Business Class" %in% names(.)) `Business Class` else CLASS
          )
      }
      
      # Prepare outstanding data for combination
      outstanding_for_reported <- NULL
      if(!is.null(outstanding) && nrow(outstanding) > 0) {
        # Check for Gross OS Claims Adjusted column
        
        outstanding_for_reported <- outstanding %>%
          mutate(
            # Create Transaction Date as NA for outstanding claims
            `Transaction Date` = as.Date(NA),
            `Notification Date` = if("Reported Date" %in% names(.)) `Reported Date`
                                 else if("Notification Date" %in% names(.)) `Notification Date`
                                 else `Loss Date`,
            `Gross Amount` = `Gross Amount`,
            `RI Amount` = if("Reinsurance" %in% names(.)) `Reinsurance` else 0,
            `Net Claims` = `Net Amount`,
            Claim_Type = "Outstanding",
            # Ensure all required columns exist
            `Business Class` = if("Business Class" %in% names(.)) `Business Class` else CLASS
          )
      }
      
      # Get common columns
      if(!is.null(paid_for_reported) && !is.null(outstanding_for_reported)) {
        common_cols <- intersect(names(paid_for_reported), names(outstanding_for_reported))
        paid_for_reported <- paid_for_reported[, common_cols]
        outstanding_for_reported <- outstanding_for_reported[, common_cols]
      }
      
      # Combine the datasets
      combined <- bind_rows(paid_for_reported, outstanding_for_reported)
      
      # Apply the reported data transformations
      if(!is.null(combined) && nrow(combined) > 0) {
        combined <- combined %>%
          mutate(
            Paid_Year = year(`Transaction Date`),
            Loss_Year = year(`Loss Date`),
            Reported_Year = year(`Notification Date`),
            Reporting_Delay = pmax(0, floor(as.numeric(difftime(`Notification Date`, `Loss Date`, units = "days")) / 90)),
            Reporting_Delay_Years = pmax(0, floor(as.numeric(difftime(`Notification Date`, `Loss Date`, units = "days")) / 365)),
            Runique = ifelse(!duplicated(`Claim No`), 1, 0)
          ) %>%
          arrange(desc(`Transaction Date`), `Claim No`)
      }
      
      return(combined)
    }
    
    # Process paid claims upload
    observeEvent(input$uploadPaid, {
      req(input$uploadPaid)
      
      tryCatch({
        # Read the file
        paid <- read_excel(input$uploadPaid$datapath)
        colnames(paid) <- trimws(colnames(paid))
        colnames(paid) <- recode(colnames(paid),
          "Gross Amount" = "Gross Amount",
          "RI Amount"    = "RI Amount",
          "Net Amount"   = "Net Amount",
          "Loss Date"    = "Loss Date",
          "Reported Date"= "Reported Date",
          "Paid Date"    = "Paid Date",
          "Claim No"     = "Claim No",
          "Business Class" = "Business Class"
        )        
        # Detect and rename date columns if needed
        date_columns <- c("Loss Date", "Notification Date", "Transaction Date", "Paid Date", "Reported Date")
        for(col in date_columns) {
          if(col %in% names(paid)) {
            paid[[col]] <- as.Date(paid[[col]], origin = "1899-12-30")
          }
        }
        
        # Apply transformations based on available columns
        paid <- paid %>%
          mutate(
            Paid_Year = if("Paid Date" %in% names(.)) year(`Paid Date`) 
                       else if("Transaction Date" %in% names(.)) year(`Transaction Date`)
                       else if("Paid Year" %in% names(.)) `Paid Year`
                       else NA,
            Loss_Year = if("Loss Date" %in% names(.)) year(`Loss Date`) else NA,
            Notification_Year = if("Reported Date" %in% names(.)) year(`Reported Date`)
                               else if("Notification Date" %in% names(.)) year(`Notification Date`)
                               else NA,
            Loss_Year_Loss_Quarter = if("Loss Date" %in% names(.)) 
                                    paste0(year(`Loss Date`), "Q", ceiling(month(`Loss Date`) / 3))
                                    else NA,
            Unique = ifelse(!duplicated(`Claim No`), 1, 0)
          )
        
        paid_data(paid)
        paid_upload_status(list(
          status = "success",
          message = paste("Successfully loaded", nrow(paid), "paid claims"),
          filename = input$uploadPaid$name
        ))
        showNotification(paste("Loaded", nrow(paid), "paid claims from", input$uploadPaid$name), type = "message", duration = 3)
        
      }, error = function(e) {
        paid_upload_status(list(
          status = "error",
          message = paste("Error:", e$message),
          filename = input$uploadPaid$name
        ))
        showNotification(paste("Error loading paid claims:", e$message), type = "error")
      })
    })
    
    # Process outstanding claims upload
    observeEvent(input$uploadOutstanding, {
      req(input$uploadOutstanding)
      
      tryCatch({
        # Read the file
        outstanding <- read_excel(input$uploadOutstanding$datapath)
        colnames(outstanding) <- trimws(colnames(outstanding))
        colnames(outstanding) <- recode(colnames(outstanding),
          "Gross Amount" = "Gross Amount",
          "RI Amount"    = "RI Amount",
          "Net Amount"   = "Net Amount",
          "Loss Date"    = "Loss Date",
          "Reported Date"= "Reported Date",
          "Paid Date"    = "Paid Date",
          "Claim No"     = "Claim No",
          "Business Class" = "Business Class"
        )
        # Detect and rename date columns if needed
        date_columns <- c("Loss Date", "Notification Date", "Reported Date")
        for(col in date_columns) {
          if(col %in% names(outstanding)) {
            outstanding[[col]] <- as.Date(outstanding[[col]], origin = "1899-12-30")
          }
        }
        
        # Apply transformations
        outstanding <- outstanding %>%
          mutate(
            Loss_Year = if("Loss Date" %in% names(.)) year(`Loss Date`) else NA,
            Notification_Year = if("Reported Date" %in% names(.)) year(`Reported Date`)
                               else if("Notification Date" %in% names(.)) year(`Notification Date`)
                               else NA,
            Loss_Year_Loss_Quarter = if("Loss Date" %in% names(.)) 
                                    paste0(year(`Loss Date`), "Q", ceiling(month(`Loss Date`) / 3))
                                    else NA,
            Unique = ifelse(!duplicated(`Claim No`), 1, 0)
          )
        
        outstanding_data(outstanding)
        outstanding_upload_status(list(
          status = "success",
          message = paste("Successfully loaded", nrow(outstanding), "outstanding claims"),
          filename = input$uploadOutstanding$name
        ))
        showNotification(paste("Loaded", nrow(outstanding), "outstanding claims from", input$uploadOutstanding$name), type = "message", duration = 3)
        
      }, error = function(e) {
        outstanding_upload_status(list(
          status = "error",
          message = paste("Error:", e$message),
          filename = input$uploadOutstanding$name
        ))
        showNotification(paste("Error loading outstanding claims:", e$message), type = "error")
      })
    })
    
    # Update reported data when paid or outstanding data changes
    observe({
      paid <- paid_data()
      outstanding <- outstanding_data()
      
      if(!is.null(paid) || !is.null(outstanding)) {
        tryCatch({
          reported <- combine_to_reported(paid, outstanding)
          reported_data(reported)
          
          if(!is.null(reported)) {
            showNotification(
              paste("Reported data created:", nrow(reported), "total claims"), 
              type = "message", 
              duration = 3
            )
          }
        }, error = function(e) {
          showNotification(paste("Error creating reported data:", e$message), type = "error")
        })
      }
    })
    
    # Clear all data
    observeEvent(input$clearData, {
      paid_data(NULL)
      outstanding_data(NULL) 
      reported_data(NULL)
      paid_upload_status(NULL)
      outstanding_upload_status(NULL)
      showNotification("All data cleared", type = "message", duration = 2)
    })
    
    # Upload status outputs
    output$paidUploadStatus <- renderUI({
      status <- paid_upload_status()
      if(!is.null(status)) {
        if(status$status == "success") {
          div(
            style = list(fontSize = "12px", color = "#107c10"),
            Icon(iconName = "CheckMark", style = list(fontSize = "12px")),
            tags$span(style = list(marginLeft = "5px"), status$filename)
          )
        } else {
          div(
            style = list(fontSize = "12px", color = "#d13438"),
            Icon(iconName = "ErrorBadge", style = list(fontSize = "12px")),
            tags$span(style = list(marginLeft = "5px"), status$message)
          )
        }
      }
    })
    
    output$outstandingUploadStatus <- renderUI({
      status <- outstanding_upload_status()
      if(!is.null(status)) {
        if(status$status == "success") {
          div(
            style = list(fontSize = "12px", color = "#107c10"),
            Icon(iconName = "CheckMark", style = list(fontSize = "12px")),
            tags$span(style = list(marginLeft = "5px"), status$filename)
          )
        } else {
          div(
            style = list(fontSize = "12px", color = "#d13438"),
            Icon(iconName = "ErrorBadge", style = list(fontSize = "12px")),
            tags$span(style = list(marginLeft = "5px"), status$message)
          )
        }
      }
    })
    
    # Status outputs
    output$paidStatus <- renderUI({
      if(!is.null(paid_data())) {
        Text(paste("✓", format(nrow(paid_data()), big.mark = ","), "records"), 
             style = list(color = "#107c10", fontSize = "12px"))
      } else {
        Text("⚠ No data uploaded", style = list(color = "#d13438", fontSize = "12px"))
      }
    })
    
    output$outstandingStatus <- renderUI({
      if(!is.null(outstanding_data())) {
        Text(paste("✓", format(nrow(outstanding_data()), big.mark = ","), "records"), 
             style = list(color = "#107c10", fontSize = "12px"))
      } else {
        Text("⚠ No data uploaded", style = list(color = "#d13438", fontSize = "12px"))
      }
    })
    
    output$reportedStatus <- renderUI({
      if(!is.null(reported_data())) {
        paid_count <- sum(reported_data()$Claim_Type == "Paid", na.rm = TRUE)
        outstanding_count <- sum(reported_data()$Claim_Type == "Outstanding", na.rm = TRUE)
        Text(paste("✓", format(nrow(reported_data()), big.mark = ","), "records",
                   paste0("(", paid_count, " paid, ", outstanding_count, " outstanding)")), 
             style = list(color = "#107c10", fontSize = "12px"))
      } else {
        Text("⚠ No data generated", style = list(color = "#d13438", fontSize = "12px"))
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
    

    # Return all data for other modules
    return(list(
      paid = paid_data,
      outstanding = outstanding_data,
      reported = reported_data
    ))
  })
}