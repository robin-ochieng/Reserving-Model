# ACC (Accident Class) Module
accModuleUI <- function(id) {
  ns <- NS(id)

  div(class = "main-container",
    div(class = "content-section",
      Stack(
        tokens = list(childrenGap = 20),
        Text("ACC — Accident Class", variant = "xLarge", style = list(fontWeight = "600")),
        Separator(),
        Text("This view is scoped to Accident Class (ACC). Filters and exports apply to the visible rows.", 
             style = list(color = "#6c757d")),
        div(style = list(marginTop = "10px"),
          Text(outputId = ns("rowCount"), variant = "medium")
        ),
        div(style = list(display = "flex", gap = "10px", marginTop = "4px"),
          downloadButton(ns("download_csv"), "Download CSV"),
          downloadButton(ns("download_xlsx"), "Download Excel")
        ),
  uiOutput(ns("summaryCards")),
        div(class = "simple-table-container",
          DT::dataTableOutput(ns("accTable"))
        ),
        Separator(),
        Text("Incremental Triangle (ACC)", variant = "large", style = list(fontWeight = "600")),
        div(class = "simple-table-container",
          verbatimTextOutput(ns("accTriangleText"))
        )
      )
    ),
    # Footer
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

accModuleServer <- function(id, data_module) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    reported_data <- reactive({
      if (is.null(data_module) || is.null(data_module$reported)) return(NULL)
      data <- tryCatch(data_module$reported(), error = function(e) NULL)
      if (is.null(data) || nrow(data) == 0) return(NULL)
      data
    })

    # Filter to Accident Class (accepts 'ACC' or 'Accident', case-insensitive)
    acc_data <- reactive({
      dat <- reported_data()
      if (is.null(dat)) return(NULL)
      if (!"Business Class" %in% names(dat)) return(dat[0, , drop = FALSE])
      cls <- tolower(as.character(dat[["Business Class"]]))
      keep <- cls %in% c("acc", "accident")
      dat[keep, , drop = FALSE]
    })

    # Adjust Gross Amounts based on thresholds per Business Class
    adjusted_data <- reactive({
      dat <- acc_data()
      if (is.null(dat)) return(NULL)
      if (!"Gross Amount" %in% names(dat)) {
        dat$`Adjusted Gross Amount` <- NA_real_
        return(dat)
      }

      th <- session$userData$thresholds_rv
      th_vals <- tryCatch(if (!is.null(th)) th() else NULL, error = function(e) NULL)
      if (is.null(th_vals) || length(th_vals) == 0) {
        dat$`Adjusted Gross Amount` <- suppressWarnings(as.numeric(dat$`Gross Amount`))
        return(dat)
      }

      upper_map <- if (!is.null(th_vals$upper)) th_vals$upper else list()
      lower_map <- if (!is.null(th_vals$lower)) th_vals$lower else list()

      classes <- as.character(dat$`Business Class`)
      u <- vapply(classes, function(x) if (!is.null(upper_map[[x]])) as.numeric(upper_map[[x]]) else Inf, numeric(1))
      l <- vapply(classes, function(x) if (!is.null(lower_map[[x]])) as.numeric(lower_map[[x]]) else -Inf, numeric(1))
      ga <- suppressWarnings(as.numeric(dat$`Gross Amount`))
      dat$`Adjusted Gross Amount` <- pmin(pmax(ga, l), u)
      dat
    })

    output$rowCount <- renderText({
      dat <- acc_data()
      if (is.null(dat)) return("No Accident (ACC) data available.")
      paste0(format(nrow(dat), big.mark = ","), " rows")
    })

    table_data <- reactive({
      dat <- adjusted_data()
      if (is.null(dat)) return(NULL)
      # Place Adjusted next to Gross
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
      if ("Gross Amount" %in% names(dat)) dat$`Gross Amount` <- suppressWarnings(as.numeric(dat$`Gross Amount`))
      if ("Adjusted Gross Amount" %in% names(dat)) dat$`Adjusted Gross Amount` <- suppressWarnings(as.numeric(dat$`Adjusted Gross Amount`))
      for (yr in c("Loss_Year", "Notification_Year", "Paid_Year", "Reported_Year")) {
        if (yr %in% names(dat)) dat[[yr]] <- suppressWarnings(as.integer(dat[[yr]]))
      }
      dat
    })

    output$accTable <- DT::renderDataTable({
      dat <- table_data()
      if (is.null(dat) || nrow(dat) == 0) {
        return(DT::datatable(data.frame(Message = "No Accident (ACC) rows in reported data."), options = list(dom = 't'), rownames = FALSE))
      }
      currency_cols <- which(names(dat) %in% c("Gross Amount", "Adjusted Gross Amount", "RI Amount", "Net Amount", "Net Claims"))
      date_cols <- which(names(dat) %in% c("Loss Date", "Notification Date", "Transaction Date", "Paid Date", "Reported Date"))
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

    # Incremental Triangle (ACC): origin = Loss_Year_Loss_Quarter, dev = Reported_Delay, value = sum(Adjusted Gross Amount)
    triangle_data <- reactive({
      dat <- adjusted_data()
      if (is.null(dat) || nrow(dat) == 0) return(NULL)

      # Ensure numeric Adjusted Gross Amount
      if ("Adjusted Gross Amount" %in% names(dat)) dat$`Adjusted Gross Amount` <- suppressWarnings(as.numeric(dat$`Adjusted Gross Amount`))

      # Derive Loss Year / Quarter
      loss_year <- if ("Loss_Year" %in% names(dat)) suppressWarnings(as.integer(dat$Loss_Year)) else NA_integer_
      loss_quarter <- if ("Loss_Quarter" %in% names(dat)) suppressWarnings(as.integer(dat$Loss_Quarter)) else NA_integer_

      # If not available, try from dates
      if ((all(is.na(loss_year)) || all(is.na(loss_quarter))) && "Loss Date" %in% names(dat)) {
        ld <- suppressWarnings(as.Date(dat$`Loss Date`))
        if (all(is.na(loss_year))) loss_year <- suppressWarnings(lubridate::year(ld))
        if (all(is.na(loss_quarter))) loss_quarter <- suppressWarnings(lubridate::quarter(ld))
      }

      # Fallbacks
      loss_year[is.na(loss_year)] <- NA_integer_
      loss_quarter[is.na(loss_quarter)] <- 1L

      origin <- paste0(loss_year, "-Q", loss_quarter)

      # Derive Reported Delay (in quarters)
      if ("Reported_Delay" %in% names(dat)) {
        dev <- suppressWarnings(as.integer(dat$Reported_Delay))
      } else {
        dev <- rep(NA_integer_, nrow(dat))
        has_dates <- ("Reported Date" %in% names(dat)) && ("Loss Date" %in% names(dat))
        if (has_dates) {
          rd <- suppressWarnings(as.Date(dat$`Reported Date`))
          ld <- suppressWarnings(as.Date(dat$`Loss Date`))
          # months difference approx; divide by 3 for quarters
          months_diff <- suppressWarnings((lubridate::year(rd) - lubridate::year(ld)) * 12 + (lubridate::month(rd) - lubridate::month(ld)))
          dev <- suppressWarnings(pmax(0L, as.integer(floor(months_diff / 3))))
        } else if (("Reported_Year" %in% names(dat)) && ("Loss_Year" %in% names(dat))) {
          ry <- suppressWarnings(as.integer(dat$Reported_Year))
          ly <- suppressWarnings(as.integer(dat$Loss_Year))
          rq <- if ("Reported_Quarter" %in% names(dat)) suppressWarnings(as.integer(dat$Reported_Quarter)) else 1L
          lq <- if ("Loss_Quarter" %in% names(dat)) suppressWarnings(as.integer(dat$Loss_Quarter)) else 1L
          dev <- suppressWarnings(pmax(0L, (ry - ly) * 4L + (rq - lq)))
        } else {
          dev <- 0L
        }
      }

      work <- data.frame(
        origin = origin,
        dev = dev,
        value = dat$`Adjusted Gross Amount`,
        stringsAsFactors = FALSE
      )
      # Clean invalid rows
      work <- work[!is.na(work$origin) & !is.na(work$dev) & !is.na(work$value), , drop = FALSE]
      if (nrow(work) == 0) return(NULL)

      # Aggregate and complete grid
      agg <- work %>%
        dplyr::group_by(origin, dev) %>%
        dplyr::summarise(Gross_Amount = sum(value, na.rm = TRUE), .groups = "drop")

      if (nrow(agg) == 0) return(NULL)

      dev_levels <- seq.int(0L, max(agg$dev, na.rm = TRUE))
      full <- tidyr::complete(agg, origin, dev = dev_levels, fill = list(Gross_Amount = 0))

      tri <- tidyr::pivot_wider(full, names_from = dev, values_from = Gross_Amount, values_fill = 0, names_prefix = "Dev_")

      # Order origins chronologically if possible
      o_yr <- suppressWarnings(as.integer(sub("-Q.*$", "", tri$origin)))
      o_q  <- suppressWarnings(as.integer(sub("^.*-Q", "", tri$origin)))
      ord <- order(o_yr, o_q, na.last = TRUE)
      tri <- tri[ord, , drop = FALSE]
      tri
    })

    output$accTriangleText <- renderText({
      tri <- triangle_data()
      if (is.null(tri) || nrow(tri) == 0) {
        return("Triangle not available for ACC.")
      }
      # Pretty print with fixed-width columns
      # Format numbers with comma separators and no scientific notation
      fmt_num <- function(x) {
        ifelse(is.na(x), "", format(x, big.mark = ",", scientific = FALSE, trim = TRUE))
      }
      tri_fmt <- tri
      num_cols <- which(sapply(tri_fmt, is.numeric))
      tri_fmt[num_cols] <- lapply(tri_fmt[num_cols], fmt_num)

      # Build header
      cols <- names(tri_fmt)
      # Compute widths: max of header and column values
      widths <- vapply(seq_along(cols), function(i) {
        max(nchar(cols[i]), max(nchar(as.character(tri_fmt[[i]])), na.rm = TRUE))
      }, integer(1))

      pad <- function(x, w) sprintf(paste0("%-", w, "s"), x)
      header <- paste(mapply(pad, cols, widths), collapse = "  ")
      sep <- paste(mapply(function(w) paste(rep("-", w), collapse = ""), widths), collapse = "  ")
      rows <- apply(tri_fmt, 1, function(r) paste(mapply(pad, r, widths), collapse = "  "))
      paste(c(header, sep, rows), collapse = "\n")
    })

    current_view <- reactive({
      dat <- table_data()
      if (is.null(dat)) return(NULL)
      idx <- input$accTable_rows_all
      if (!is.null(idx) && length(idx) > 0) dat[idx, , drop = FALSE] else dat
    })

    output$summaryCards <- renderUI({
      dat <- current_view()
      if (is.null(dat) || nrow(dat) == 0) return(NULL)
      # Counts and sums for Accident (ACC) filtered view
      cnt_acc  <- nrow(dat)
      sum_gross <- if ("Gross Amount" %in% names(dat)) sum(suppressWarnings(as.numeric(dat$`Gross Amount`)), na.rm = TRUE) else 0
      sum_adj   <- if ("Adjusted Gross Amount" %in% names(dat)) sum(suppressWarnings(as.numeric(dat$`Adjusted Gross Amount`)), na.rm = TRUE) else sum_gross
      fmt_curr <- function(x) paste0("SCR ", format(round(x, 2), big.mark = ",", nsmall = 2))
      div(class = "simple-cards-row",
        div(class = "summary-card",
          Icon(iconName = "NumberSymbol", style = list(fontSize = "24px", color = "#605e5c")),
          div(
            div(class = "card-number", format(cnt_acc, big.mark = ",")),
            div(class = "card-label", "Accident Count")
          )
        ),
        div(class = "summary-card",
          Icon(iconName = "Money", style = list(fontSize = "24px", color = "#107c10")),
          div(
            div(class = "card-number", fmt_curr(sum_gross)),
            div(class = "card-label", "Gross Amount (Sum)")
          )
        ),
        div(class = "summary-card",
          Icon(iconName = "CalculatorEqualTo", style = list(fontSize = "24px", color = "#0078d4")),
          div(
            div(class = "card-number", fmt_curr(sum_adj)),
            div(class = "card-label", "Adjusted Gross Amount (Sum)")
          )
        )
      )
    })

    get_filtered <- function() current_view()

    output$download_csv <- downloadHandler(
      filename = function() paste0("acc_reported_filtered_", Sys.Date(), ".csv"),
      content = function(file) {
        dat <- get_filtered()
        write.csv(dat, file, row.names = FALSE, na = "")
      },
      contentType = "text/csv"
    )

    output$download_xlsx <- downloadHandler(
      filename = function() paste0("acc_reported_filtered_", Sys.Date(), ".xlsx"),
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
