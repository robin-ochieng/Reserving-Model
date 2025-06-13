# Claim Plots Module - Enhanced Version
# Purpose: Extreme Value Analysis and Visualization for Insurance Claims
# Dependencies: shiny, shiny.fluent, DT, plotly, dplyr

# ==============================================================================
# UI FUNCTION
# ==============================================================================

claimPlotsModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    div(
      class = "main-container",
      div(
        class = "content-section",
        
        # Module Header
        div(
          class = "module-header",
          h1(class = "module-title", "Claim Development Analysis"),
          p(style = "color: #6c757d; font-size: 1.1rem;", 
            "Extreme Value Analysis and Visualization for Insurance Claims")
        ),
        
        # Control Panel Section
        div(
          class = "control-panel-card",
          createControlPanel(ns)
        ),
        
        # Extreme Value Analysis Table Section
        div(
          class = "table-card",
          createTableSection(ns)
        ),
        
        # Scatter Plot Section
        div(
          class = "plot-card",
          createPlotSection(ns)
        ),
        
        # Footer
        createFooter()
      )
    )
  )
}

# ==============================================================================
# SERVER FUNCTION
# ==============================================================================

claimPlotsModuleServer <- function(id, data_module) {
  moduleServer(id, function(input, output, session) {
    
    # --- REACTIVE VALUES ---
    threshold_values <- reactiveValues()
    
    # --- DATA REACTIVES ---
    
    # Get reported data from data module
    reported_data <- reactive({
      req(data_module)
      data_module$reported()
    })
    
    # Process data for extreme value analysis
    table_data <- reactive({
      req(reported_data())
      calculateExtremeValues(
        data = reported_data(),
        class_filter = input$classFilter,
        default_threshold = input$defaultThreshold %||% 100000,
        threshold_values = threshold_values
      )
    })
    
    # Calculate plot statistics
    plot_stats <- reactive({
      req(reported_data())
      calculatePlotStats(
        data = reported_data(),
        class_filter = input$plotClassFilter
      )
    })
    
    # --- OBSERVERS ---
    
    # Update dropdown options when data changes
    observe({
      updateDropdowns(session, reported_data())
    })
    
    # Initialize thresholds for each business class
    observe({
      initializeThresholds(
        table_data(), 
        threshold_values, 
        input$defaultThreshold %||% 100000
      )
    })
    
    # Handle threshold edits in table
    observeEvent(input$extremeValueTable_cell_edit, {
      handleThresholdEdit(
        edit_info = input$extremeValueTable_cell_edit,
        table_data = table_data(),
        threshold_values = threshold_values
      )
    })
    
    # Reset all thresholds
    observeEvent(input$resetThresholds, {
      resetAllThresholds(
        threshold_values, 
        input$defaultThreshold %||% 100000
      )
    })
    
    # Update notification (FIXED: using valid notification type)
    observeEvent(input$updateTable, {
      showNotification("Analysis updated successfully!", type = "message", duration = 2)
    })
    
    # --- OUTPUTS ---
    
    # Extreme value analysis table
    output$extremeValueTable <- DT::renderDataTable({
      createExtremeValueTable(table_data())
    })
    
    # Summary statistics
    output$plotClaimCount <- renderText({
      format(plot_stats()$count, big.mark = ",")
    })
    
    output$plotMeanValue <- renderText({
      paste0("$", format(round(plot_stats()$mean), big.mark = ","))
    })
    
    output$plotMaxValue <- renderText({
      paste0("$", format(round(plot_stats()$max), big.mark = ","))
    })
    
    output$plotStdDev <- renderText({
      paste0("$", format(round(plot_stats()$sd), big.mark = ","))
    })
    
    # Scatter plot
    output$claimScatterPlot <- renderPlotly({
      createScatterPlot(
        data = reported_data(),
        class_filter = input$plotClassFilter
      )
    })
    
  })
}

# ==============================================================================
# UI HELPER FUNCTIONS
# ==============================================================================

createControlPanel <- function(ns) {
  Stack(
    horizontal = TRUE,
    tokens = list(childrenGap = 20),
    
    # Business Class Filter
    Stack(
      Text("Business Class Filter", 
           variant = "medium", 
           style = list(fontWeight = "600", color = "white")),
      Dropdown.shinyInput(
        ns("classFilter"),
        placeholder = "Select Business Class",
        options = list(list(key = "all", text = "All Classes")),
        styles = list(
          root = list(width = "250px"),
          dropdown = list(borderRadius = "8px")
        )
      )
    ),
    
    # Default Threshold
    Stack(
      Text("Default Upper Threshold", 
           variant = "medium", 
           style = list(fontWeight = "600", color = "white")),
      SpinButton.shinyInput(
        ns("defaultThreshold"),
        defaultValue = 100000,
        min = 0,
        max = 10000000,
        step = 10000,
        label = "Set default threshold",
        styles = list(
          root = list(width = "200px"),
          spinButton = list(borderRadius = "8px")
        )
      )
    ),
    
    # Action Buttons
    Stack(
      horizontal = TRUE,
      tokens = list(childrenGap = 10),
      PrimaryButton.shinyInput(
        ns("updateTable"),
        text = "Update Analysis",
        iconProps = list(iconName = "Refresh"),
        styles = list(
          root = list(borderRadius = "8px", fontWeight = "600")
        )
      ),
      DefaultButton.shinyInput(
        ns("resetThresholds"),
        text = "Reset All",
        iconProps = list(iconName = "RevToggleKey"),
        styles = list(
          root = list(borderRadius = "8px", fontWeight = "600")
        )
      )
    )
  )
}

createTableSection <- function(ns) {
  Stack(
    tokens = list(childrenGap = 15),
    Stack(
      horizontal = TRUE,
      tokens = list(childrenGap = 10),
      verticalAlign = "center",
      Text("Extreme Value Analysis", 
           variant = "xLarge", 
           style = list(fontWeight = "700", color = "#2c3e50")),
      Icon(iconName = "Info", 
           styles = list(root = list(fontSize = "16px", color = "#667eea")))
    ),
    Text("Click on Upper Threshold values to edit them directly in the table", 
         variant = "medium", 
         style = list(color = "#6c757d", fontStyle = "italic")),
    div(
      id = ns("tableContainer"),
      DT::dataTableOutput(ns("extremeValueTable"))
    )
  )
}

createPlotSection <- function(ns) {
  Stack(
    tokens = list(childrenGap = 15),
    
    # Header with dropdown
    Stack(
      horizontal = TRUE,
      tokens = list(childrenGap = 20),
      verticalAlign = "center",
      Text("Claim Distribution Analysis", 
           variant = "xLarge", 
           style = list(fontWeight = "700", color = "#2c3e50")),
      div(style = "flex-grow: 1;"),
      Dropdown.shinyInput(
        ns("plotClassFilter"),
        placeholder = "Select Business Class for Plot",
        options = list(list(key = "all", text = "All Classes")),
        styles = list(
          root = list(width = "280px"),
          dropdown = list(borderRadius = "8px")
        )
      )
    ),
    
    # Summary statistics
    createSummaryStats(ns),
    
    # Plot
    div(
      style = "border-radius: 8px; overflow: hidden; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
      plotlyOutput(ns("claimScatterPlot"), height = "500px")
    )
  )
}

createSummaryStats <- function(ns) {
  div(
    class = "summary-stats",
    
    createStatItem(ns("plotClaimCount"), "Total Claims", "📊"),
    createStatItem(ns("plotMeanValue"), "Mean Value", "📈"),
    createStatItem(ns("plotMaxValue"), "Maximum", "🔝"),
    createStatItem(ns("plotStdDev"), "Std. Deviation", "📉")
  )
}

createStatItem <- function(outputId, label, icon = NULL) {
  div(
    class = "stat-item",
    if(!is.null(icon)) span(icon, style = "font-size: 2rem; margin-bottom: 10px; display: block;"),
    div(class = "stat-value", textOutput(outputId)),
    div(class = "stat-label", label)
  )
}

createFooter <- function() {
  div(
    class = "app-footer",
    div(
      class = "footer-content",
      div(
        class = "footer-left",
        tags$span("© 2024 SACOS Group Limited. All rights reserved."),
        br(),
        tags$small("Extreme Value Analysis for Risk Management")
      ),
      div(
        class = "footer-right",
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
}

# ==============================================================================
# SERVER HELPER FUNCTIONS
# ==============================================================================

# Calculate extreme values for each business class
calculateExtremeValues <- function(data, class_filter, default_threshold, threshold_values) {
  
  # Filter by selected class if not "all"
  if(!is.null(class_filter) && class_filter != "all") {
    data <- data %>% filter(`Business Class` == class_filter)
  }
  
  # Group by Business Class and calculate metrics
  summary_data <- data %>%
    group_by(`Business Class`) %>%
    summarise(
      Claim_Count = n(),
      Net_Claims = list(`Net Claims`),
      .groups = 'drop'
    ) %>%
    rowwise() %>%
    mutate(
      # Use stored threshold or default
      Upper_Threshold = ifelse(
        `Business Class` %in% names(threshold_values),
        threshold_values[[`Business Class`]],
        default_threshold
      ),
      Lower_Threshold = -Upper_Threshold,
      
      # Count extreme losses
      Extreme_Plus_Losses = sum(unlist(Net_Claims) > Upper_Threshold, na.rm = TRUE),
      Extreme_Minus_Losses = sum(unlist(Net_Claims) < Lower_Threshold, na.rm = TRUE),
      
      # Sum extreme values
      Extreme_Plus_Values = sum(unlist(Net_Claims)[unlist(Net_Claims) > Upper_Threshold], na.rm = TRUE),
      Extreme_Minus_Values = sum(unlist(Net_Claims)[unlist(Net_Claims) < Lower_Threshold], na.rm = TRUE),
      
      # Calculate excluded values
      Excluded_Plus_Values = ifelse(
        Extreme_Plus_Values > 0,
        Extreme_Plus_Values - (Upper_Threshold * Extreme_Plus_Losses),
        0
      ),
      Excluded_Minus_Values = ifelse(
        Extreme_Minus_Values < 0,
        Extreme_Minus_Values - (Lower_Threshold * Extreme_Minus_Losses),
        0
      ),
      
      # Calculate proportion of excluded claims
      Proptn_of_Excld_Claims = round((Extreme_Plus_Losses + Extreme_Minus_Losses) / Claim_Count * 100, 2)
    ) %>%
    select(-Net_Claims)
  
  return(summary_data)
}

# Calculate statistics for the plot
calculatePlotStats <- function(data, class_filter) {
  
  # Filter data
  if(!is.null(class_filter) && class_filter != "all") {
    data <- data %>% filter(`Business Class` == class_filter)
  }
  
  net_claims <- data$`Net Claims`
  net_claims <- net_claims[!is.na(net_claims)]
  
  if(length(net_claims) == 0) {
    return(list(count = 0, mean = 0, max = 0, sd = 0))
  }
  
  list(
    count = length(net_claims),
    mean = mean(net_claims, na.rm = TRUE),
    max = max(net_claims, na.rm = TRUE),
    sd = sd(net_claims, na.rm = TRUE)
  )
}

# Update dropdown options
updateDropdowns <- function(session, data) {
  req(data)
  
  # Get unique business classes
  classes <- unique(data$`Business Class`)
  classes <- classes[!is.na(classes)]
  classes <- sort(classes)
  
  # Create options for dropdowns
  class_options <- c(
    list(list(key = "all", text = "All Classes")),
    lapply(classes, function(x) list(key = x, text = x))
  )
  
  # Update both dropdowns
  updateDropdown.shinyInput(session, "classFilter", options = class_options)
  updateDropdown.shinyInput(session, "plotClassFilter", options = class_options)
}

# Initialize threshold values
initializeThresholds <- function(data, threshold_values, default_threshold) {
  req(data)
  
  classes <- data$`Business Class`
  for(class in classes) {
    if(is.null(threshold_values[[class]])) {
      threshold_values[[class]] <- default_threshold
    }
  }
}

# Handle threshold editing (FIXED: using valid notification type)
handleThresholdEdit <- function(edit_info, table_data, threshold_values) {
  
  # Check if Upper Threshold column was edited
  if(edit_info$col == 2) {
    new_threshold <- as.numeric(edit_info$value)
    
    # Validate input
    if(is.na(new_threshold) || new_threshold <= 0) {
      showNotification(
        "Please enter a valid positive number for the threshold.", 
        type = "warning",
        duration = 3
      )
      return()
    }
    
    # Get the business class for this row
    business_class <- table_data$`Business Class`[edit_info$row]
    
    # Update the threshold for this class
    threshold_values[[business_class]] <- new_threshold
    
    showNotification(
      paste("Threshold updated for", business_class, "to $", 
            format(new_threshold, big.mark = ",")), 
      type = "message",
      duration = 2
    )
  }
}

# Reset all thresholds to default (FIXED: using valid notification type)
resetAllThresholds <- function(threshold_values, default_threshold) {
  classes <- names(threshold_values)
  
  for(class in classes) {
    threshold_values[[class]] <- default_threshold
  }
  
  showNotification(
    paste("All thresholds reset to $", format(default_threshold, big.mark = ",")), 
    type = "default",
    duration = 2
  )
}

# Create the extreme value analysis table
createExtremeValueTable <- function(data) {
  req(data)
  
  # Prepare data for display
  display_data <- data %>%
    mutate(
      Upper_Threshold = as.numeric(Upper_Threshold),
      Lower_Threshold = as.numeric(Lower_Threshold),
      Proptn_of_Excld_Claims = paste0(Proptn_of_Excld_Claims, "%")
    )
  
  # Create datatable with enhanced styling
  DT::datatable(
    display_data,
    editable = list(
      target = 'cell',
      disable = list(columns = c(0, 1, 3:10)) # Only Upper_Threshold is editable
    ),
    options = list(
      pageLength = 15,
      scrollX = TRUE,
      dom = 'Bfrtip',
      buttons = list(
        list(extend = 'copy', className = 'btn-primary'),
        list(extend = 'csv', className = 'btn-primary'),
        list(extend = 'excel', className = 'btn-primary')
      ),
      columnDefs = list(
        # Format currency columns
        list(
          targets = c(2, 3, 6, 7, 8, 9),
          render = JS(
            "function(data, type, full, meta) {
              if(type === 'display' && data != null) {
                return '$' + parseFloat(data).toLocaleString('en-US', {
                  minimumFractionDigits: 0,
                  maximumFractionDigits: 0
                });
              }
              return data;
            }"
          )
        ),
        # Format count columns
        list(
          targets = c(1, 4, 5),
          render = JS(
            "function(data, type, full, meta) {
              if(type === 'display' && data != null) {
                return parseFloat(data).toLocaleString('en-US');
              }
              return data;
            }"
          )
        ),
        # Center align specific columns
        list(
          targets = c(1, 4, 5, 10),
          className = 'dt-center'
        ),
        # Add editable class to Upper Threshold column
        list(
          targets = 2,
          className = 'editable'
        )
      ),
      initComplete = JS(
        "function(settings, json) {
          $(this.api().table().header()).css({
            'background': 'linear-gradient(135deg, #667eea 0%, #764ba2 100%)',
            'color': 'white'
          });
        }"
      )
    ),
    class = 'cell-border stripe hover compact',
    rownames = FALSE,
    colnames = c(
      'Business Class',
      'Claim Count',
      'Upper Threshold',
      'Lower Threshold',
      'Extreme (+) Losses',
      'Extreme (-) Losses',
      'Extreme (+) Values',
      'Extreme (-) Values',
      'Excluded (+) Values',
      'Excluded (-) Values',
      'Proportion of Excluded Claims'
    )
  ) %>%
    formatCurrency(
      columns = c('Upper_Threshold', 'Lower_Threshold', 'Extreme_Plus_Values', 
                 'Extreme_Minus_Values', 'Excluded_Plus_Values', 'Excluded_Minus_Values'),
      currency = "$",
      digits = 0
    )
}

# Create the scatter plot with enhanced styling
createScatterPlot <- function(data, class_filter) {
  req(data)
  
  # Filter data
  plot_data <- data
  if(!is.null(class_filter) && class_filter != "all") {
    plot_data <- plot_data %>% filter(`Business Class` == class_filter)
    title_text <- paste("Claim Distribution -", class_filter)
  } else {
    title_text <- "Claim Distribution - All Classes"
  }
  
  # Get net claims
  net_claims <- plot_data$`Net Claims`
  net_claims <- net_claims[!is.na(net_claims)]
  
  # Handle empty data
  if(length(net_claims) == 0) {
    return(
      plot_ly() %>%
        layout(
          title = list(
            text = "No data available for selected class",
            font = list(size = 18, family = "Arial, sans-serif", color = "#6c757d")
          ),
          xaxis = list(title = ""),
          yaxis = list(title = ""),
          plot_bgcolor = "#f8f9fa",
          paper_bgcolor = "white"
        )
    )
  }
  
  # Calculate statistics
  mean_val <- mean(net_claims, na.rm = TRUE)
  median_val <- median(net_claims, na.rm = TRUE)
  p75 <- quantile(net_claims, 0.75, na.rm = TRUE)
  p90 <- quantile(net_claims, 0.90, na.rm = TRUE)
  p95 <- quantile(net_claims, 0.95, na.rm = TRUE)
  
  # Create color scale based on claim values
  colors <- colorRampPalette(c("#667eea", "#764ba2", "#ff6b6b"))(length(net_claims))
  
  # Create plot with enhanced styling
  plot_ly() %>%
    # Scatter points with gradient colors
    add_trace(
      x = seq_along(net_claims),
      y = net_claims,
      type = 'scatter',
      mode = 'markers',
      name = 'Claims',
      marker = list(
        size = 10,
        color = net_claims,
        colorscale = list(
          list(0, "#667eea"),
          list(0.5, "#764ba2"),
          list(1, "#ff6b6b")
        ),
        showscale = TRUE,
        colorbar = list(
          title = "Claim Amount",
          tickformat = "$,.0f",
          thickness = 15,
          len = 0.8
        ),
        line = list(color = 'white', width = 1),
        opacity = 0.8
      ),
      text = paste0(
        "<b>Claim #", seq_along(net_claims), "</b><br>",
        "Amount: <b>$", format(round(net_claims), big.mark = ","), "</b><br>",
        "Percentile: <b>", round(ecdf(net_claims)(net_claims) * 100, 1), "%</b>"
      ),
      hoverinfo = 'text',
      hoverlabel = list(
        bgcolor = 'white',
        bordercolor = '#667eea',
        font = list(family = "Arial, sans-serif", size = 12)
      )
    ) %>%
    # Mean line
    add_trace(
      x = c(1, length(net_claims)),
      y = c(mean_val, mean_val),
      type = 'scatter',
      mode = 'lines',
      name = paste('Mean ($', format(round(mean_val), big.mark = ","), ')', sep = ""),
      line = list(color = '#2ecc71', width = 3, dash = 'solid'),
      showlegend = TRUE
    ) %>%
    # Median line
    add_trace(
      x = c(1, length(net_claims)),
      y = c(median_val, median_val),
      type = 'scatter',
      mode = 'lines',
      name = paste('Median ($', format(round(median_val), big.mark = ","), ')', sep = ""),
      line = list(color = '#3498db', width = 2.5, dash = 'dash'),
      showlegend = TRUE
    ) %>%
    # 75th percentile line
    add_trace(
      x = c(1, length(net_claims)),
      y = c(p75, p75),
      type = 'scatter',
      mode = 'lines',
      name = paste('75th %ile ($', format(round(p75), big.mark = ","), ')', sep = ""),
      line = list(color = '#f39c12', width = 2, dash = 'dashdot'),
      showlegend = TRUE
    ) %>%
    # 90th percentile line
    add_trace(
      x = c(1, length(net_claims)),
      y = c(p90, p90),
      type = 'scatter',
      mode = 'lines',
      name = paste('90th %ile ($', format(round(p90), big.mark = ","), ')', sep = ""),
      line = list(color = '#e74c3c', width = 2, dash = 'dot'),
      showlegend = TRUE
    ) %>%
    # 95th percentile line
    add_trace(
      x = c(1, length(net_claims)),
      y = c(p95, p95),
      type = 'scatter',
      mode = 'lines',
      name = paste('95th %ile ($', format(round(p95), big.mark = ","), ')', sep = ""),
      line = list(color = '#9b59b6', width = 2, dash = 'longdash'),
      showlegend = TRUE
    ) %>%
    layout(
      title = list(
        text = title_text,
        font = list(size = 22, family = "Arial, sans-serif", color = "#2c3e50"),
        x = 0.5,
        xanchor = 'center'
      ),
      xaxis = list(
        title = list(
          text = "Claim Index",
          font = list(size = 14, family = "Arial, sans-serif")
        ),
        gridcolor = '#e9ecef',
        showgrid = TRUE,
        zeroline = FALSE,
        tickfont = list(size = 12)
      ),
      yaxis = list(
        title = list(
          text = "Net Claim Amount ($)",
          font = list(size = 14, family = "Arial, sans-serif")
        ),
        gridcolor = '#e9ecef',
        showgrid = TRUE,
        zeroline = TRUE,
        rangemode = 'tozero',
        tickformat = "$,.0f",
        tickfont = list(size = 12)
      ),
      legend = list(
        orientation = "h",
        yanchor = "bottom",
        y = -0.2,
        xanchor = "center",
        x = 0.5,
        bgcolor = 'rgba(255, 255, 255, 0.8)',
        bordercolor = '#e9ecef',
        borderwidth = 1,
        font = list(size = 11)
      ),
      hovermode = 'closest',
      plot_bgcolor = '#f8f9fa',
      paper_bgcolor = 'white',
      margin = list(t = 80, b = 100),
      hoverlabel = list(
        bgcolor = 'white',
        bordercolor = '#667eea',
        font = list(family = "Arial, sans-serif", size = 12)
      )
    ) %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToAdd = list('hoverclosest', 'hovercompare'),
      toImageButtonOptions = list(
        format = 'png',
        filename = paste0('claim_distribution_', Sys.Date()),
        height = 600,
        width = 1200,
        scale = 2
      )
    )
}