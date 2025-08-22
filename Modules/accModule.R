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
        # Triangle styling (scoped by class selector)
        tags$style(HTML('
          .triangle-box {
            border: 1px solid #0137A6; border-radius: 8px; background: #f7faff; padding: 12px; overflow-x: auto;
            box-shadow: 0 1px 3px rgba(0,0,0,0.05);
          }
          .triangle-box pre {
            margin: 0; color: #0137A6; font-family: Consolas, "Courier New", monospace; font-size: 12px; line-height: 1.15;
          }
        ')),
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
        div(style = list(display = "flex", gap = "10px", marginTop = "4px"),
          downloadButton(ns("download_triangle_csv"), "Download Triangle CSV"),
          downloadButton(ns("download_triangle_xlsx"), "Download Triangle Excel")
        ),
        div(class = "simple-table-container",
          div(class = "triangle-box",
            verbatimTextOutput(ns("accTriangleText"))
          )
        )
        ,
        Separator(),
        Text("Cumulative Triangle (ACC)", variant = "large", style = list(fontWeight = "600")),
        div(style = list(display = "flex", gap = "10px", marginTop = "4px"),
          downloadButton(ns("download_cum_triangle_csv"), "Download Cumulative CSV"),
          downloadButton(ns("download_cum_triangle_xlsx"), "Download Cumulative Excel")
        ),
        div(class = "simple-table-container",
          div(class = "triangle-box",
            verbatimTextOutput(ns("accCumTriangleText"))
          )
        ),
        Separator(),
        Text("Cumulative Triangle Column Sums (ACC)", variant = "large", style = list(fontWeight = "600")),
        div(style = list(display = "flex", gap = "10px", marginTop = "4px"),
          downloadButton(ns("download_cum_summary_csv"), "Download Summary CSV"),
          downloadButton(ns("download_cum_summary_xlsx"), "Download Summary Excel")
        ),
        div(class = "simple-table-container",
          div(class = "triangle-box",
            verbatimTextOutput(ns("accCumTriangleSumsText"))
          )
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

      # Cap origins to start from 2013-Q1
      min_idx <- 2013L * 4L + 1L
      idx <- loss_year * 4L + as.integer(loss_quarter)
      keep_origin <- !is.na(idx) & idx >= min_idx
      if (!any(keep_origin)) return(NULL)
      # Filter dataset and aligned vectors before proceeding
      dat <- dat[keep_origin, , drop = FALSE]
      loss_year <- loss_year[keep_origin]
      loss_quarter <- loss_quarter[keep_origin]

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
          # months difference via interval; round down months, then divide by 3 for quarters
          months_diff <- suppressWarnings(floor(lubridate::time_length(lubridate::interval(ld, rd), "months")))
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

      # Build a square dev axis with NA beyond allowed development
      # 1) Order & index origins
      orig_levels <- sort(unique(agg$origin))
      N <- length(orig_levels)
      o_yr_idx <- suppressWarnings(as.integer(sub("-Q.*$", "", orig_levels)))
      o_q_idx  <- suppressWarnings(as.integer(sub("^.*-Q",  "", orig_levels)))
      origin_index_vec <- o_yr_idx * 4L + o_q_idx
      names(origin_index_vec) <- orig_levels

      # 2) Latest observed calendar index
      agg$o_idx <- origin_index_vec[agg$origin]
      agg$cal_i <- agg$o_idx + agg$dev
      latest_cal <- max(agg$cal_i, na.rm = TRUE)

      # 3) Full square grid dev = 0..N-1 for all origins
      full_dev <- 0L:(N - 1L)
      full_sq <- tidyr::complete(
        agg,
        origin = orig_levels,
        dev    = full_dev
      )

      # 4) Within allowed dev: keep values; if missing -> 0. Beyond allowed -> NA
      full_sq$o_idx <- origin_index_vec[full_sq$origin]
      full_sq$allowed_dev <- pmax(0L, latest_cal - full_sq$o_idx)
      full_sq$Gross_Amount <- dplyr::case_when(
        !is.na(full_sq$Gross_Amount)               ~ full_sq$Gross_Amount,
        full_sq$dev <= full_sq$allowed_dev         ~ 0,
        TRUE                                       ~ NA_real_
      )

      # 5) Pivot to wide without forcing fill so NAs are preserved
      tri <- tidyr::pivot_wider(
        full_sq[, c("origin", "dev", "Gross_Amount")],
        names_from  = dev,
        values_from = Gross_Amount
      )

      # 6) Order origins and round numerics, preserving NAs
      o_yr <- suppressWarnings(as.integer(sub("-Q.*$", "", tri$origin)))
      o_q  <- suppressWarnings(as.integer(sub("^.*-Q",  "", tri$origin)))
      ord <- order(o_yr, o_q, na.last = TRUE)
      tri <- tri[ord, , drop = FALSE]
      num_cols_tri <- which(sapply(tri, is.numeric))
      if (length(num_cols_tri)) {
        tri[num_cols_tri] <- lapply(tri[num_cols_tri], function(x) ifelse(is.na(x), NA, round(x, 0)))
      }

      tri
    })

    output$accTriangleText <- renderText({
      tri <- triangle_data()
      if (is.null(tri) || nrow(tri) == 0) {
        return("Triangle not available for ACC.")
      }
      # Pretty print with fixed-width columns
      # Format numbers with comma separators and no scientific notation
  fmt_num <- function(x) ifelse(is.na(x), "", format(round(x, 0), big.mark = ",", scientific = FALSE, trim = TRUE))
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
      join <- function(parts) paste(parts, collapse = " | ")
      header <- join(mapply(pad, cols, widths))
      sep <- join(mapply(function(w) paste(rep("-", w), collapse = ""), widths))
      rows <- apply(tri_fmt, 1, function(r) join(mapply(pad, r, widths)))
      # Insert horizontal borders between all data rows
      interleaved <- as.vector(rbind(rows, rep(sep, length(rows))))
      paste(c(header, sep, interleaved), collapse = "\n")
        })

    # Cumulative triangle with future devs masked to NA (proper run-off)
    cum_triangle_data <- reactive({
      tri_inc <- triangle_data()
      if (is.null(tri_inc) || nrow(tri_inc) == 0) return(NULL)

      tri <- as.data.frame(tri_inc, stringsAsFactors = FALSE)

      # Identify dev columns and dev integers
      dev_names <- setdiff(names(tri), "origin")
      if (!length(dev_names)) return(NULL)
      dev_int <- suppressWarnings(as.integer(dev_names))

      # Origin index (year*4 + quarter)
      o_yr <- suppressWarnings(as.integer(sub("-Q.*$", "", tri$origin)))
      o_q  <- suppressWarnings(as.integer(sub("^.*-Q",  "", tri$origin)))
      origin_index <- o_yr * 4L + o_q

      # Build long view to compute latest observed calendar index using only non-NA observations
      vals_mat <- as.matrix(tri[, dev_names])
      long <- data.frame(
        origin = rep(tri$origin, each = length(dev_names)),
        dev    = rep(dev_names, times = nrow(tri)),
        val    = as.vector(t(vals_mat)),
        stringsAsFactors = FALSE
      )
      long$dev_i <- suppressWarnings(as.integer(as.character(long$dev)))
      long$o_idx <- rep(origin_index, each = length(dev_names))
      long$cal_i <- long$o_idx + long$dev_i
      latest_cal <- max(long$cal_i[!is.na(long$val)], na.rm = TRUE)

      # Allowed dev for each origin
      allowed_dev <- pmax(0L, latest_cal - origin_index)

      # Cumulative by row (treat NA as 0), then re-mask future cells to NA
      storage.mode(vals_mat) <- "double"
      mat_cum <- t(apply(replace(vals_mat, is.na(vals_mat), 0), 1, cumsum))
      for (i in seq_len(nrow(mat_cum))) {
        future_mask <- dev_int > allowed_dev[i]
        mat_cum[i, future_mask] <- NA_real_
      }

      tri_cum <- tri
      tri_cum[, dev_names] <- lapply(seq_along(dev_names), function(j) ifelse(is.na(mat_cum[, j]), NA, round(mat_cum[, j], 0)))
      class(tri_cum) <- "data.frame"
      tri_cum
    })




    output$accCumTriangleText <- renderText({
      tri <- cum_triangle_data()
      if (is.null(tri) || nrow(tri) == 0) {
        return("Cumulative triangle not available for ACC.")
      }
  fmt_num <- function(x) ifelse(is.na(x), "", format(round(x, 0), big.mark = ",", scientific = FALSE, trim = TRUE))
  tri_fmt <- tri
  num_cols <- which(sapply(tri_fmt, is.numeric))
  tri_fmt[num_cols] <- lapply(tri_fmt[num_cols], fmt_num)

      cols <- names(tri_fmt)
      widths <- vapply(seq_along(cols), function(i) {
        max(nchar(cols[i]), max(nchar(as.character(tri_fmt[[i]])), na.rm = TRUE))
      }, integer(1))

  pad <- function(x, w) sprintf(paste0("%-", w, "s"), x)
  join <- function(parts) paste(parts, collapse = " | ")
  header <- join(mapply(pad, cols, widths))
  sep <- join(mapply(function(w) paste(rep("-", w), collapse = ""), widths))
  rows <- apply(tri_fmt, 1, function(r) join(mapply(pad, r, widths)))
      interleaved <- as.vector(rbind(rows, rep(sep, length(rows))))
      paste(c(header, sep, interleaved), collapse = "\n")
    })

    # Standalone Column Sums under cumulative triangle
    output$accCumTriangleSumsText <- renderText({
      tri <- cum_triangle_data()
      if (is.null(tri) || nrow(tri) == 0) {
        return("Cumulative triangle sums not available for ACC.")
      }
      fmt_num <- function(x) ifelse(is.na(x), "", format(round(x, 0), big.mark = ",", scientific = FALSE, trim = TRUE))
      dev_cols <- setdiff(names(tri), "origin")
      if (!length(dev_cols)) return("")
      col_sums <- vapply(dev_cols, function(nm) sum(suppressWarnings(as.numeric(tri[[nm]])), na.rm = TRUE), numeric(1))
      sums_fmt <- sapply(col_sums, fmt_num)

      # Compute last non-NA observed value per development column
      last_vals <- vapply(dev_cols, function(nm) {
        col <- suppressWarnings(as.numeric(tri[[nm]]))
        idx <- which(!is.na(col))
        if (length(idx)) col[max(idx)] else NA_real_
      }, numeric(1))
      last_fmt <- sapply(last_vals, fmt_num)

      # Compute Development Factors per formula
      # dev_factor[j] = col_sums[j+1] / (col_sums[j] - last_vals[j]); last column = 1
      n_dev <- length(dev_cols)
      dev_factors <- rep(NA_real_, n_dev)
      if (n_dev >= 1) {
        for (j in seq_len(n_dev)) {
          if (j == n_dev) {
            dev_factors[j] <- 1
          } else {
            num <- col_sums[j + 1]
            den <- col_sums[j] - last_vals[j]
            if (is.na(num) || is.na(den) || den <= 0) {
              dev_factors[j] <- NA_real_
            } else {
              dev_factors[j] <- num / den
            }
          }
        }
      }
  fmt_factor <- function(x) ifelse(is.na(x), "", formatC(x, format = "f", digits = 4))
      factors_fmt <- sapply(dev_factors, fmt_factor)

      # Compute Cumulative Development Factors (reverse cumulative product with NA/non-finite guard)
      cdf <- rep(NA_real_, n_dev)
      if (n_dev >= 1) {
        cdf[n_dev] <- 1
        if (n_dev >= 2) {
          for (j in (n_dev - 1):1) {
            slice <- dev_factors[j:n_dev]
            if (any(is.na(slice)) || any(!is.finite(slice))) {
              cdf[j] <- NA_real_
            } else {
              cdf[j] <- prod(slice)
            }
          }
        }
      }
  cdf_fmt <- sapply(cdf, fmt_factor)

      # Build a 2-row summary table (Column Sum; Last Column Value) with aligned headers and widths
    header_cols <- c("origin", dev_cols)
    header_labels <- c("Development Periods", dev_cols)
    widths <- vapply(seq_along(header_cols), function(i) {
        if (i == 1) {
          max(
            nchar("Development Periods"),
            nchar("Column Sum"),
            nchar("Last Column Value"),
            nchar("Development Factors"),
            nchar("Cumulative Development Factors")
          )
        } else {
          j <- i - 1
          max(
            nchar(dev_cols[j]),
            nchar(sums_fmt[j]),
            nchar(last_fmt[j]),
            nchar(factors_fmt[j]),
            nchar(cdf_fmt[j])
          )
        }
      }, integer(1))

      pad <- function(x, w) sprintf(paste0("%-", w, "s"), x)
      join <- function(parts) paste(parts, collapse = " | ")
  header <- join(mapply(pad, header_labels, widths))
  sep <- join(mapply(function(w) paste(rep("-", w), collapse = ""), widths))
  row_sum <- join(mapply(pad, c("Column Sum", as.character(sums_fmt)), widths))
  row_last <- join(mapply(pad, c("Last Column Value", as.character(last_fmt)), widths))
  row_factor <- join(mapply(pad, c("Development Factors", as.character(factors_fmt)), widths))
  row_cdf <- join(mapply(pad, c("Cumulative Development Factors", as.character(cdf_fmt)), widths))
  paste(c(header, sep, row_sum, sep, row_last, sep, row_factor, sep, row_cdf, sep), collapse = "\n")
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

    # Triangle downloads (CSV / Excel)
    output$download_triangle_csv <- downloadHandler(
      filename = function() paste0("acc_triangle_", Sys.Date(), ".csv"),
      content = function(file) {
        tri <- triangle_data()
        if (is.null(tri) || nrow(tri) == 0) stop("Triangle not available for ACC.")
        utils::write.csv(tri, file, row.names = FALSE, na = "")
      },
      contentType = "text/csv"
    )

    output$download_triangle_xlsx <- downloadHandler(
      filename = function() paste0("acc_triangle_", Sys.Date(), ".xlsx"),
      content = function(file) {
        tri <- triangle_data()
        if (is.null(tri) || nrow(tri) == 0) stop("Triangle not available for ACC.")
        if (requireNamespace("writexl", quietly = TRUE)) {
          writexl::write_xlsx(tri, path = file)
        } else if (requireNamespace("openxlsx", quietly = TRUE)) {
          openxlsx::write.xlsx(tri, file)
        } else {
          stop("Please install 'writexl' or 'openxlsx' to export Excel.")
        }
      }
    )

    # Cumulative Triangle downloads (CSV / Excel)
    output$download_cum_triangle_csv <- downloadHandler(
      filename = function() paste0("acc_cumulative_triangle_", Sys.Date(), ".csv"),
      content = function(file) {
        tri <- cum_triangle_data()
        if (is.null(tri) || nrow(tri) == 0) stop("Cumulative triangle not available for ACC.")
        utils::write.csv(tri, file, row.names = FALSE, na = "")
      },
      contentType = "text/csv"
    )

    output$download_cum_triangle_xlsx <- downloadHandler(
      filename = function() paste0("acc_cumulative_triangle_", Sys.Date(), ".xlsx"),
      content = function(file) {
        tri <- cum_triangle_data()
        if (is.null(tri) || nrow(tri) == 0) stop("Cumulative triangle not available for ACC.")
        if (requireNamespace("writexl", quietly = TRUE)) {
          writexl::write_xlsx(tri, path = file)
        } else if (requireNamespace("openxlsx", quietly = TRUE)) {
          openxlsx::write.xlsx(tri, file)
        } else {
          stop("Please install 'writexl' or 'openxlsx' to export Excel.")
        }
      }
    )

    # Cumulative Summary downloads (CSV / Excel)
    build_cum_summary_df <- function() {
      tri <- cum_triangle_data()
      if (is.null(tri) || nrow(tri) == 0) return(NULL)
      dev_cols <- setdiff(names(tri), "origin")
      if (!length(dev_cols)) return(NULL)

      col_sums <- vapply(dev_cols, function(nm) sum(suppressWarnings(as.numeric(tri[[nm]])), na.rm = TRUE), numeric(1))
      last_vals <- vapply(dev_cols, function(nm) {
        col <- suppressWarnings(as.numeric(tri[[nm]]))
        idx <- which(!is.na(col))
        if (length(idx)) col[max(idx)] else NA_real_
      }, numeric(1))

      n_dev <- length(dev_cols)
      dev_factors <- rep(NA_real_, n_dev)
      if (n_dev >= 1) {
        for (j in seq_len(n_dev)) {
          if (j == n_dev) {
            dev_factors[j] <- 1
          } else {
            num <- col_sums[j + 1]
            den <- col_sums[j] - last_vals[j]
            if (is.na(num) || is.na(den) || den <= 0) {
              dev_factors[j] <- NA_real_
            } else {
              dev_factors[j] <- num / den
            }
          }
        }
      }

      cdf <- rep(NA_real_, n_dev)
      if (n_dev >= 1) {
        cdf[n_dev] <- 1
        if (n_dev >= 2) {
          for (j in (n_dev - 1):1) {
            slice <- dev_factors[j:n_dev]
            if (any(is.na(slice)) || any(!is.finite(slice))) {
              cdf[j] <- NA_real_
            } else {
              cdf[j] <- prod(slice)
            }
          }
        }
      }

      # Assemble a tidy summary table with one row per metric
      summary_mat <- rbind(
        setNames(as.list(c("Column Sum", as.numeric(col_sums))), c("Metric", dev_cols)),
        setNames(as.list(c("Last Column Value", as.numeric(last_vals))), c("Metric", dev_cols)),
        setNames(as.list(c("Development Factors", as.numeric(dev_factors))), c("Metric", dev_cols)),
        setNames(as.list(c("Cumulative Development Factors", as.numeric(cdf))), c("Metric", dev_cols))
      )
      # Convert to data.frame with stringsAsFactors=FALSE
      as.data.frame(summary_mat, stringsAsFactors = FALSE, check.names = FALSE)
    }

    output$download_cum_summary_csv <- downloadHandler(
      filename = function() paste0("acc_cumulative_summary_", Sys.Date(), ".csv"),
      content = function(file) {
        df <- build_cum_summary_df()
        if (is.null(df)) stop("Cumulative summary not available for ACC.")
        # Ensure numeric columns stay numeric; NA exported as blank
        utils::write.csv(df, file, row.names = FALSE, na = "")
      },
      contentType = "text/csv"
    )

    output$download_cum_summary_xlsx <- downloadHandler(
      filename = function() paste0("acc_cumulative_summary_", Sys.Date(), ".xlsx"),
      content = function(file) {
        df <- build_cum_summary_df()
        if (is.null(df)) stop("Cumulative summary not available for ACC.")
        if (requireNamespace("writexl", quietly = TRUE)) {
          writexl::write_xlsx(df, path = file)
        } else if (requireNamespace("openxlsx", quietly = TRUE)) {
          openxlsx::write.xlsx(df, file)
        } else {
          stop("Please install 'writexl' or 'openxlsx' to export Excel.")
        }
      }
    )
  })
}
