# Claim Plots Module - Updated and Fixed Version
# Purpose: Claim Distribution Visualization with Threshold Management

# =============================
# UI FUNCTION
# =============================
claimPlotsModuleUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$head(tags$style(HTML(".ms-Dropdown-container { margin-bottom: 20px; }"))),
    div(
      class = "main-container",
      div(
        class = "content-section",
        div(
          class = "module-header",
          h1(class = "module-title", "Claim Distribution Analysis"),
          p("Visualize claim distributions and manage thresholds by business class")
        ),
        div(
          class = "plot-card",
          Stack(
            tokens = list(childrenGap = 15),
            Stack(
              Text("Select Business Class", variant = "large"),
              uiOutput(ns("businessClassDropdown"))
            ),
            div(
              withSpinner(
                plotlyOutput(ns("claimScatterPlot"), height = "500px"),
                type = 6,  # Choose spinner type
                color = "#0078D4",  # Microsoft Fluent Blue
                color.background = "#F3F2F1"
              )
            )
          )
        ),
        div(
          class = "table-card",
          Stack(
            tokens = list(childrenGap = 15),
            Text("Claims Threshold", variant = "xLarge"),
            Text("Click on threshold values to edit them directly in the table", variant = "medium"),
            DT::dataTableOutput(ns("thresholdTable")),
          )
        )
      )
    )
  )
}


# =============================
# SERVER FUNCTION
# =============================
claimPlotsModuleServer <- function(id, data_module) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    threshold_values <- reactiveValues(upper = list(), lower = list())

    reported_data <- reactive({
      if (is.null(data_module) || !is.reactive(data_module$reported)) return(NULL)
      data <- tryCatch(data_module$reported(), error = function(e) NULL)
      if (is.null(data) || nrow(data) == 0) return(NULL)
      if (!all(c("Business Class", "Net Claims") %in% names(data))) return(NULL)
      return(data)
    })

    threshold_table_data <- reactive({
      data <- reported_data()
      if (is.null(data)) return(NULL)
      classes <- sort(unique(data$`Business Class`[!is.na(data$`Business Class`)]))
      tibble(
        `Class of Business` = classes,
        `Upper Threshold` = sapply(classes, function(x) threshold_values$upper[[x]] %||% 100000),
        `Lower Threshold` = sapply(classes, function(x) threshold_values$lower[[x]] %||% -100000)
      )
    })

    output$businessClassDropdown <- renderUI({
      req(reported_data())
      data <- reported_data()
      names(data) <- trimws(names(data))

      if (!"Business Class" %in% names(data)) return(NULL)
      classes <- sort(unique(trimws(as.character(data$`Business Class`[!is.na(data$`Business Class`)]))))
      options <- c("All Classes", classes)

      Dropdown.shinyInput(
        inputId = ns("businessClassFilter"),
        label = "",
        value = "All Classes",
        options = lapply(options, function(x) list(key = x, text = x)),
        searchable = TRUE,
        styles = list(root = list(width = "300px"))
      )
    })

    observeEvent(reported_data(), {
      data <- reported_data()
      if (!"Business Class" %in% names(data)) return()
      classes <- sort(unique(trimws(as.character(data$`Business Class`[!is.na(data$`Business Class`)]))))
      for (class in classes) {
        if (is.null(threshold_values$upper[[class]])) threshold_values$upper[[class]] <- 100000
        if (is.null(threshold_values$lower[[class]])) threshold_values$lower[[class]] <- -100000
      }
    })

    observeEvent(input$thresholdTable_cell_edit, {
      info <- input$thresholdTable_cell_edit
      data <- threshold_table_data()
      if (!is.null(data) && info$row <= nrow(data)) {
        business_class <- data$`Class of Business`[info$row]
        new_value <- as.numeric(info$value)
        if (!is.na(new_value) && info$col == 1) {
          threshold_values$upper[[business_class]] <- new_value
          threshold_values$lower[[business_class]] <- -abs(new_value)
        }
      }
    })

    output$claimScatterPlot <- renderPlotly({
      data <- reported_data()
      if (is.null(data)) return(NULL)

      selected_class <- input$businessClassFilter %||% "All Classes"
      plot_data <- if (!is.null(selected_class) && selected_class != "All Classes") {
        data %>% filter(`Business Class` == selected_class)
      } else {
        data
      }

      if (nrow(plot_data) == 0) return(NULL)

      plot_data <- plot_data %>%
        mutate(
          index = row_number(),
          hover_text = paste0(
            "<b>Claim #", index, "</b><br>",
            "Amount: $", format(round(`Net Claims`), big.mark = ",")
          )
        )

      color_limits <- range(reported_data()$`Net Claims`, na.rm = TRUE)
      upper_threshold <- if (selected_class != "All Classes") threshold_values$upper[[selected_class]] else NULL
      lower_threshold <- if (selected_class != "All Classes") threshold_values$lower[[selected_class]] else NULL

      plot_ly(
        data = plot_data,
        x = ~index,
        y = ~`Net Claims`,
        type = 'scatter',
        mode = 'lines',
        marker = list(
          size = 10,
          color = ~`Net Claims`,
          colorscale = 'RdBu',
          cmin = color_limits[1],
          cmax = color_limits[2],
          showscale = TRUE
        ),
        hoverinfo = 'text',
        text = ~hover_text
      ) %>%
          layout(
            title = list(text = paste("Claim Distribution -", selected_class)),
            yaxis = list(title = "Net Claim Amount ($)", tickformat = "$,.0f"),
            legend = list(
              orientation = "v",        # vertical legend (or "h" for horizontal)
              x = 0.09,                 # left padding (0 = far left, 1 = far right)
              y = 0.99,                 # top (1 = top, 0 = bottom)
              bgcolor = 'rgba(255,255,255,0.8)',  # light background behind legend
              bordercolor = "#ccc",
              borderwidth = 1
            )            
          ) %>%
          {
            if (!is.null(upper_threshold)) add_trace(
              .,
              x = c(min(plot_data$index), max(plot_data$index)),
              y = rep(upper_threshold, 2),
              mode = 'lines',
              name = 'Upper Threshold',
              line = list(color = '#9b59b6', dash = 'dot'),
              text = NULL,
              hoverinfo = 'none'
            ) else .
          } %>%
          {
            if (!is.null(lower_threshold)) add_trace(
              .,
              x = c(min(plot_data$index), max(plot_data$index)),
              y = rep(lower_threshold, 2),
              mode = 'lines',
              name = 'Lower Threshold',
              line = list(color = '#e67e22', dash = 'dot'),
              text = NULL,
              hoverinfo = 'none'
            ) else .
          }

    })

    output$thresholdTable <- DT::renderDataTable({
      data <- threshold_table_data()
      if (is.null(data)) return(DT::datatable(data.frame(Message = "No data available")))

      DT::datatable(
        data,
        editable = list(target = 'cell', disable = list(columns = c(0, 2))),
        options = list(
          dom = 't',
          columnDefs = list(
            list(targets = c(1, 2), className = 'dt-center'),
            list(targets = 1:2, render = JS(
              "function(data, type, row, meta) {",
              "if(type === 'display') return '$' + parseFloat(data).toLocaleString();",
              "return data; }"
            ))
          )
        ),
        rownames = FALSE
      )
    })
  })
}
