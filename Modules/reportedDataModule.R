# Reported Data Module
reportedDataModuleUI <- function(id) {
  ns <- NS(id)
  
  div(class = "main-container",
        div(class = "content-section",
          Stack(
            tokens = list(childrenGap = 20),
            Text("Reported Claims Data", variant = "xLarge", style = list(fontWeight = "600")),
            Separator(),
            Text("This table reflects the combined reported dataset (from Paid + Outstanding). Use the filters above each column to refine.", 
                style = list(color = "#6c757d")),
            div(
              style = list(marginTop = "10px"),
              Text(outputId = ns("rowCount"), variant = "medium")
            ),
            div(
              style = list(display = "flex", gap = "10px", marginTop = "4px"),
              downloadButton(ns("download_csv"), "Download CSV"),
              downloadButton(ns("download_xlsx"), "Download Excel")
            ),
            uiOutput(ns("reportedSummaryCards")),
            div(class = "simple-table-container",
              DT::dataTableOutput(ns("reportedTableFull"))
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
              ),
              tags$span(" AI")
            )
          )
        )
  )

}

reportedDataModuleServer <- function(id, data_module) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    reported_data <- reactive({
      if (is.null(data_module) || is.null(data_module$reported)) return(NULL)
      data <- tryCatch(data_module$reported(), error = function(e) NULL)
      if (is.null(data) || nrow(data) == 0) return(NULL)
      data
    })

    # Adjust Gross Amounts based on thresholds per Business Class
    adjusted_reported_data <- reactive({
      dat <- reported_data()
      if (is.null(dat)) return(NULL)
      if (!"Gross Amount" %in% names(dat)) {
        dat$`Adjusted Gross Amount` <- NA_real_
        return(dat)
      }

      th <- session$userData$thresholds_rv
      th_vals <- tryCatch(if (!is.null(th)) th() else NULL, error = function(e) NULL)
      if (is.null(th_vals) || length(th_vals) == 0) {
        dat$`Adjusted Gross Amount` <- dat$`Gross Amount`
        return(dat)
      }

      upper_map <- if (!is.null(th_vals$upper)) th_vals$upper else list()
      lower_map <- if (!is.null(th_vals$lower)) th_vals$lower else list()

      # Vectorized per-row adjustment using class-specific thresholds
      classes <- as.character(dat$`Business Class`)
      u <- vapply(classes, function(x) if (!is.null(upper_map[[x]])) as.numeric(upper_map[[x]]) else Inf, numeric(1))
      l <- vapply(classes, function(x) if (!is.null(lower_map[[x]])) as.numeric(lower_map[[x]]) else -Inf, numeric(1))
      ga <- as.numeric(dat$`Gross Amount`)
      adj <- pmin(pmax(ga, l), u)
      dat$`Adjusted Gross Amount` <- adj
      dat
    })

    output$rowCount <- renderText({
      dat <- reported_data()
      if (is.null(dat)) return("No reported data available.")
      paste0(format(nrow(dat), big.mark = ","), " rows")
    })

    # Build the table data (ordered/coerced) for both UI and exports
    table_data <- reactive({
      dat <- adjusted_reported_data()
      if (is.null(dat)) return(NULL)
      # Reorder columns to place Adjusted Gross Amount right after Gross Amount (if present)
      if ("Adjusted Gross Amount" %in% names(dat)) {
        cols <- names(dat)
        idx_adj <- match("Adjusted Gross Amount", cols)
        idx_gross <- match("Gross Amount", cols)
        if (!is.na(idx_gross) && !is.na(idx_adj) && idx_adj != (idx_gross + 1)) {
          keep <- setdiff(seq_along(cols), idx_adj)
          before <- keep[keep <= idx_gross]
          after <- keep[keep > idx_gross]
          new_order <- c(before, idx_adj, after)
          dat <- dat[, new_order, drop = FALSE]
        }
      }
      # Ensure numeric and year types
      if ("Gross Amount" %in% names(dat)) dat$`Gross Amount` <- suppressWarnings(as.numeric(dat$`Gross Amount`))
      if ("Adjusted Gross Amount" %in% names(dat)) dat$`Adjusted Gross Amount` <- suppressWarnings(as.numeric(dat$`Adjusted Gross Amount`))
      for (yr in c("Loss_Year", "Notification_Year", "Paid_Year", "Reported_Year")) {
        if (yr %in% names(dat)) dat[[yr]] <- suppressWarnings(as.integer(dat[[yr]]))
      }
      dat
    })

    output$reportedTableFull <- DT::renderDataTable({
      dat <- table_data()
      if (is.null(dat)) {
        return(DT::datatable(data.frame(Message = "No reported data available. Please upload Paid and/or Outstanding data."), options = list(dom = 't'), rownames = FALSE))
      }

      # Determine currency and date columns by name
  currency_cols <- which(names(dat) %in% c("Gross Amount", "Adjusted Gross Amount", "RI Amount", "Net Amount", "Net Claims"))
  date_cols <- which(names(dat) %in% c("Loss Date", "Notification Date", "Transaction Date", "Paid Date", "Reported Date"))
  # DataTables uses 0-based column indices
  currency_cols0 <- if (length(currency_cols)) currency_cols - 1 else currency_cols
  date_cols0 <- if (length(date_cols)) date_cols - 1 else date_cols

      DT::datatable(
        dat,
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          scrollY = "420px",
          columnDefs = list(
            list(targets = currency_cols0, render = DT::JS(
              "function(data, type, full, meta) {",
              "  if(type === 'display' && data != null) {",
              "    var num = parseFloat(data);",
              "    if (!isNaN(num)) return 'SCR ' + num.toLocaleString('en-US', {minimumFractionDigits: 2, maximumFractionDigits: 2});",
              "  }",
              "  return data;",
              "}"
            )),
            list(targets = date_cols0, render = DT::JS(
              "function(data, type, full, meta) {",
              "  if(type === 'display' && data != null) {",
              "    var d = new Date(data);",
              "    if (!isNaN(d)) return d.toLocaleDateString('en-US');",
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
    })

    # Current filtered view (respects column filters/search)
    current_view <- reactive({
      dat <- table_data()
      if (is.null(dat)) return(NULL)
      idx <- input$reportedTableFull_rows_all
      if (!is.null(idx) && length(idx) > 0) dat[idx, , drop = FALSE] else dat
    })

    # Summary cards (sums for filtered rows)
    output$reportedSummaryCards <- renderUI({
      dat <- current_view()
      if (is.null(dat) || nrow(dat) == 0) return(NULL)
      sum_gross <- if ("Gross Amount" %in% names(dat)) sum(dat$`Gross Amount`, na.rm = TRUE) else 0
      sum_adj   <- if ("Adjusted Gross Amount" %in% names(dat)) sum(dat$`Adjusted Gross Amount`, na.rm = TRUE) else sum_gross
      sum_ri    <- if ("RI Amount" %in% names(dat)) sum(dat$`RI Amount`, na.rm = TRUE) else 0
      sum_net   <- if ("Net Amount" %in% names(dat)) sum(dat$`Net Amount`, na.rm = TRUE) else 0
      fmt <- function(x) paste0("SCR ", format(round(x, 2), big.mark = ",", nsmall = 2))
      div(class = "simple-cards-row",
        div(class = "summary-card",
          Icon(iconName = "Money", style = list(fontSize = "24px", color = "#107c10")),
          div(
            div(class = "card-number", fmt(sum_gross)),
            div(class = "card-label", "Gross Amount")
          )
        ),
        div(class = "summary-card",
          Icon(iconName = "CalculatorEqualTo", style = list(fontSize = "24px", color = "#0078d4")),
          div(
            div(class = "card-number", fmt(sum_adj)),
            div(class = "card-label", "Adjusted Gross Amount")
          )
        ),
        div(class = "summary-card",
          Icon(iconName = "Shield", style = list(fontSize = "24px", color = "#d13438")),
          div(
            div(class = "card-number", fmt(sum_ri)),
            div(class = "card-label", "Reinsurance Amount")
          )
        ),
        div(class = "summary-card",
          Icon(iconName = "CalculatorAddition", style = list(fontSize = "24px", color = "#8764b8")),
          div(
            div(class = "card-number", fmt(sum_net)),
            div(class = "card-label", "Net Amount")
          )
        )
      )
    })

    # Download filtered rows (CSV / Excel)
  get_filtered <- function() current_view()

    output$download_csv <- downloadHandler(
      filename = function() paste0("reported_filtered_", Sys.Date(), ".csv"),
      content = function(file) {
        dat <- get_filtered()
        write.csv(dat, file, row.names = FALSE, na = "")
      },
      contentType = "text/csv"
    )

    output$download_xlsx <- downloadHandler(
      filename = function() paste0("reported_filtered_", Sys.Date(), ".xlsx"),
      content = function(file) {
        dat <- get_filtered()
        if (requireNamespace("writexl", quietly = TRUE)) {
          writexl::write_xlsx(dat, path = file)
        } else if (requireNamespace("openxlsx", quietly = TRUE)) {
          openxlsx::write.xlsx(dat, file)
        } else {
          stop("Please install 'writexl' or 'openxlsx' to export Excel.")
        }
      }
    )
  })
}