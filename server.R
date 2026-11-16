
# ============================================================
# Team Starflock - Atmospheric Explorer
# server.R
#
# Geographic map:
# - Original 5-degree geographic cells
# - Actual trend values (no interpolation)
# - Shared color scale across all 12 months
# - Missing cells remain blank
# ============================================================

server <- function(input, output, session) {

  # ==========================================================
  # 1. COLORS AND THEMES
  # ==========================================================

  observation_mode <- reactive({
    req(input$observation)
    tolower(trimws(as.character(input$observation)))
  })

  theme_colors <- reactive({

    mode <- observation_mode()

    if (mode == "night") {

      return(list(
        paper = "#101827",
        plot = "#17243B",
        grid = "#35445F",
        text = "#E8F0FF",
        secondary = "#B9C8E0",
        border = "#DCE6FF",
        palette = c("#2166AC", "#F7F7F7", "#B2182B"),
        lines = c(
          "#FF8FA3", "#FFB86B", "#FDE047",
          "#A3E635", "#34D399", "#2DD4BF",
          "#22D3EE", "#60A5FA", "#818CF8",
          "#C084FC", "#F472B6", "#FB7185"
        )
      ))

    }

    if (mode == "combined") {

      return(list(
        paper = "#485A73",
        plot = "#65768F",
        grid = "#8899AF",
        text = "#FFFFFF",
        secondary = "#E1EAF5",
        border = "#F5F8FF",
        palette = c("#2166AC", "#F7F7F7", "#B2182B"),
        lines = c(
          "#FFB0BA", "#FFD18A", "#FFF18A",
          "#C8F18C", "#83E9BC", "#79E8DC",
          "#89DDF7", "#A0C5FF", "#B3B0FF",
          "#D1ADFF", "#FFACDF", "#FF9FAD"
        )
      ))

    }

    list(
      paper = "#FFFFFF",
      plot = "#F8FAFC",
      grid = "#E2E8F0",
      text = "#334155",
      secondary = "#64748B",
      border = "#303030",
      palette = c("#2166AC", "#F7F7F7", "#B2182B"),
      lines = grDevices::hcl.colors(12, "Dark 3")
    )
  })

  # ==========================================================
  # 2. FILTER MONTHLY DATA
  # ==========================================================

  filtered <- reactive({

    req(
      input$variable,
      input$observation,
      input$surface
    )

    trends[
      variable == input$variable &
      observation_type == input$observation &
      surface_type == input$surface
    ]
  })

  # ==========================================================
  # 3. FIXED ALTITUDE AXES
  # ==========================================================

  altitude_limits <- reactive({

    d <- filtered()[
      is.finite(pressure_hPa) &
      pressure_hPa > 0 &
      is.finite(trend_per_decade)
    ]

    validate(need(
      nrow(d) > 0,
      "No altitude trend data available."
    ))

    xr <- range(d$trend_per_decade)

    xpad <- max(
      diff(xr) * 0.05,
      abs(xr[1]) * 0.01,
      abs(xr[2]) * 0.01,
      1e-8
    )

    if (diff(xr) == 0) {
      xpad <- max(abs(xr[1]) * 0.05, 1e-6)
    }

    lo <- min(d$pressure_hPa)
    hi <- max(d$pressure_hPa)

    yp <- max(
      0.03,
      abs(log10(hi / lo)) * 0.04
    )

    list(
      x = c(xr[1] - xpad, xr[2] + xpad),
      y = c(log10(hi) + yp, log10(lo) - yp),
      pressure_min = lo,
      pressure_max = hi
    )
  })

  # ==========================================================
  # 4. ALTITUDE TREND PLOT
  # ==========================================================

  output$altitude_plot <- renderPlotly({

    req(input$month, input$plot_style)

    theme <- theme_colors()
    lim <- altitude_limits()

    d <- data.table::copy(
      filtered()[
        is.finite(pressure_hPa) &
        pressure_hPa > 0 &
        is.finite(trend_per_decade)
      ]
    )

    if (input$month != "all") {
      d <- d[
        sampling_month == as.integer(input$month)
      ]
    }

    validate(need(
      nrow(d) > 0,
      "No trend data available for this month."
    ))

    data.table::setorder(
      d,
      sampling_month,
      -pressure_hPa
    )

    show_month_legend <- identical(
      input$month,
      "all"
    )

    plot_mode <- switch(
      as.character(input$plot_style),
      "dots" = "markers",
      "dots_only" = "markers",
      "markers" = "markers",
      "lines" = "lines",
      "lines_only" = "lines",
      "both" = "lines+markers",
      "lines+markers" = "lines+markers",
      "lines+markers"
    )

    p <- plotly::plot_ly()

    for (m in sort(unique(d$sampling_month))) {

      sub <- d[sampling_month == m]

      color <- theme$lines[as.integer(m)]

      trace_args <- list(
        p = p,
        data = sub,
        x = ~trend_per_decade,
        y = ~pressure_hPa,
        type = "scatter",
        mode = plot_mode,
        name = month.name[as.integer(m)],
        showlegend = show_month_legend,
        text = ~paste0(
          "Month: ",
          month.name[sampling_month],
          "<br>Pressure: ",
          signif(pressure_hPa, 5),
          " hPa",
          "<br>Trend/decade: ",
          signif(trend_per_decade, 5),
          "<br>95% CI: ",
          signif(ci_low, 3),
          " to ",
          signif(ci_high, 3)
        ),
        hoverinfo = "text"
      )

      if (grepl("markers", plot_mode)) {

        trace_args$marker <- list(
          size = 7,
          color = color
        )
      }

      if (grepl("lines", plot_mode)) {

        trace_args$line <- list(
          width = 1.5,
          color = color
        )
      }

      p <- do.call(
        plotly::add_trace,
        trace_args
      )
    }

    # Atmospheric layers

    position <- function(pressure) {

      (lim$y[1] - log10(pressure)) /
        (lim$y[1] - lim$y[2])
    }

    bands <- list(
      list(
        "Troposphere",
        1013.25,
        226.32,
        "rgba(255,190,100,0.10)"
      ),
      list(
        "Stratosphere",
        226.32,
        0.798,
        "rgba(90,200,140,0.10)"
      ),
      list(
        "Mesosphere",
        0.798,
        0.008,
        "rgba(90,150,230,0.10)"
      ),
      list(
        "Thermosphere",
        0.008,
        0.00001,
        "rgba(160,120,220,0.10)"
      )
    )

    shapes <- list()
    annotations <- list()

    for (b in bands) {

      upper <- max(
        b[[3]],
        lim$pressure_min
      )

      lower <- min(
        b[[2]],
        lim$pressure_max
      )

      if (upper >= lower) next

      shapes[[length(shapes) + 1L]] <- list(
        type = "rect",
        xref = "paper",
        x0 = 0,
        x1 = 1,
        yref = "paper",
        y0 = position(lower),
        y1 = position(upper),
        fillcolor = b[[4]],
        line = list(width = 0),
        layer = "below"
      )

      annotations[[length(annotations) + 1L]] <- list(
        xref = "paper",
        x = 0.97,
        xanchor = "right",
        yref = "paper",
        y = position(sqrt(upper * lower)),
        text = b[[1]],
        showarrow = FALSE,
        bgcolor = "rgba(255,255,255,0.08)",
        font = list(
          size = 11,
          color = theme$secondary
        )
      )
    }

    p <- plotly::layout(
      p,

      title = if (input$month == "all") {
        "Monthly Trends by Pressure - All 12 Months"
      } else {
        paste(
          "Trends by Pressure -",
          month.name[as.integer(input$month)]
        )
      },

      paper_bgcolor = theme$paper,
      plot_bgcolor = theme$plot,
      font = list(color = theme$text),

      autosize = TRUE,
      showlegend = show_month_legend,

      legend = list(
        x = 1.02,
        y = 1,
        xanchor = "left",
        yanchor = "top",
        orientation = "v",
        font = list(
          size = 11,
          color = theme$text
        )
      ),

      margin = list(
        l = 105,
        r = 135,
        t = 65,
        b = 75
      ),

      xaxis = list(
        title = "Trend per decade (variable units)",
        domain = c(0, 1),
        range = as.numeric(lim$x),
        autorange = FALSE,
        fixedrange = TRUE,
        gridcolor = theme$grid,
        zerolinecolor = theme$secondary
      ),

      yaxis = list(
        title = paste(
          "Atmospheric Pressure (hPa)",
          "- Proxy for Altitude"
        ),
        type = "log",
        range = as.numeric(lim$y),
        autorange = FALSE,
        fixedrange = TRUE,
        gridcolor = theme$grid,
        exponentformat = "none"
      ),

      shapes = shapes,
      annotations = annotations
    )

    plotly::config(
      p,
      scrollZoom = FALSE,
      doubleClick = FALSE,
      displayModeBar = TRUE
    )
  })

    # ==========================================================
    # 5. SEASONAL HEATMAP - CORRECTED
    # ==========================================================

    output$heatmap_plot <- renderPlotly({

    theme <- theme_colors()

    d <- data.table::copy(
        filtered()[
        is.finite(pressure_hPa) &
        pressure_hPa > 0 &
        is.finite(trend_per_decade)
        ]
    )

    validate(need(
        nrow(d) > 0,
        "No seasonal heatmap data available."
    ))

    # Aggregate duplicate month/pressure combinations
    d <- d[
        ,
        .(
        trend_per_decade = mean(
            trend_per_decade,
            na.rm = TRUE
        )
        ),
        by = .(sampling_month, pressure_hPa)
    ]

    # Build a complete month-by-pressure matrix
    months <- 1:12

    pressures <- sort(
        unique(d$pressure_hPa),
        decreasing = TRUE
    )

    z <- matrix(
        NA_real_,
        nrow = length(pressures),
        ncol = length(months)
    )

    row_index <- match(d$pressure_hPa, pressures)
    col_index <- match(d$sampling_month, months)

    z[cbind(row_index, col_index)] <- d$trend_per_decade

    # Symmetric color range centered on zero
    max_abs <- max(
        abs(z),
        na.rm = TRUE
    )

    if (!is.finite(max_abs) || max_abs <= 0) {
        max_abs <- 1
    }

    p <- plotly::plot_ly(
        x = months,
        y = seq_along(pressures),
        z = z,

        type = "heatmap",

        zmin = -max_abs,
        zmax = max_abs,
        zmid = 0,

        colorscale = list(
        c(0, theme$palette[1]),
        c(0.5, theme$palette[2]),
        c(1, theme$palette[3])
        ),

        hoverongaps = FALSE,

        hovertemplate = paste0(
        "Month: %{x}",
        "<br>Pressure: %{customdata} hPa",
        "<br>Trend/decade: %{z:.5g}",
        "<extra></extra>"
        ),

        customdata = matrix(
        rep(pressures, times = length(months)),
        nrow = length(pressures),
        ncol = length(months)
        )
    )
    # Show approximately 12 evenly spaced pressure labels
    tick_indices <- unique(
    round(
        seq(
        1,
        length(pressures),
        length.out = min(12, length(pressures))
        )
    )
    )
    plotly::layout(
        p,

        title = "Monthly Trend Heatmap",

        paper_bgcolor = theme$paper,
        plot_bgcolor = theme$plot,
        font = list(color = theme$text),

        xaxis = list(
        title = "Month",
        tickmode = "array",
        tickvals = months,
        ticktext = month.abb
        ),
        yaxis = list(
        title = "Atmospheric Pressure (hPa)",
        tickmode = "array",
        tickvals = tick_indices,
        ticktext = sprintf("%g", pressures[tick_indices]),
        autorange = "reversed",
        showgrid = FALSE
        ),

        margin = list(
        l = 100,
        r = 90,
        t = 60,
        b = 70
        )
    )
    })
  # ==========================================================
  # 6. LOAD GEOGRAPHIC DATA
  # ==========================================================

  map_data <- reactive({

    req(input$map_month)

    path <- sprintf(
      "data/shiny_spatial_trends_month_%02d.csv",
      as.integer(input$map_month)
    )

    validate(need(
      file.exists(path),
      paste("Missing geographic CSV:", path)
    ))

    data.table::fread(path)

  }) |>
    shiny::bindCache(input$map_month)

  # ==========================================================
  # 7. GEOGRAPHIC PRESSURE LEVELS
  # ==========================================================

  map_levels <- reactive({

    req(
      input$variable,
      input$observation,
      input$surface
    )

    d <- map_data()[
      variable == input$variable &
      observation_type == input$observation &
      surface_type == input$surface &
      is.finite(pressure_hPa) &
      pressure_hPa > 0 &
      is.finite(trend_per_decade)
    ]

    validate(need(
      nrow(d) > 0,
      "No geographic pressure levels available."
    ))

    d[
      ,
      .(
        pressure_hPa = median(pressure_hPa)
      ),
      by = pressure_level_index
    ][order(-pressure_hPa)]
  })

  observeEvent(map_levels(), {

    levels <- map_levels()

    shiny::updateSliderInput(
      session,
      "map_pressure_index",
      min = 1,
      max = nrow(levels),
      value = 1,
      step = 1
    )
  })

  selected_map_level <- reactive({

    levels <- map_levels()

    req(input$map_pressure_index)

    i <- max(
      1L,
      min(
        nrow(levels),
        as.integer(input$map_pressure_index)
      )
    )

    levels[i]
  })

  output$map_pressure_label <- renderText({

    level <- selected_map_level()

    paste0(
      "Selected: ",
      signif(level$pressure_hPa, 6),
      " hPa (level ",
      level$pressure_level_index,
      ")"
    )
  })

  # ==========================================================
  # 8. CONSISTENT MAP COLOR SCALE
  #
  # Based on all 12 months for the selected combination.
  # Uses the 95th percentile of absolute trends.
  # ==========================================================

  map_color_limit <- reactive({

    req(
      input$variable,
      input$observation,
      input$surface
    )

    level_index <- selected_map_level()$pressure_level_index

    files <- sprintf(
      "data/shiny_spatial_trends_month_%02d.csv",
      1:12
    )

    validate(need(
      all(file.exists(files)),
      "All 12 geographic CSV files are required."
    ))

    values <- unlist(
      lapply(files, function(path) {

        x <- data.table::fread(
          path,
          select = c(
            "variable",
            "observation_type",
            "surface_type",
            "pressure_level_index",
            "trend_per_decade"
          )
        )

        x[
          variable == input$variable &
          observation_type == input$observation &
          surface_type == input$surface &
          pressure_level_index == level_index &
          is.finite(trend_per_decade),
          trend_per_decade
        ]
      }),
      use.names = FALSE
    )

    validate(need(
      length(values) > 0,
      "No geographic trend values available."
    ))

    limit <- as.numeric(
      stats::quantile(
        abs(values),
        probs = 0.95,
        na.rm = TRUE,
        names = FALSE
      )
    )

    if (!is.finite(limit) || limit <= 0) {
      limit <- max(abs(values), na.rm = TRUE)
    }

    if (!is.finite(limit) || limit <= 0) {
      limit <- 1
    }

    limit

  }) |>
    shiny::bindCache(
      input$variable,
      input$observation,
      input$surface,
      selected_map_level()$pressure_level_index
    )

  # ==========================================================
  # 9. ORIGINAL GEOGRAPHIC GRID
  #
  # No interpolation.
  # No artificial smoothing.
  #
  # Data centers:
  # Latitude: 22.5 to 47.5
  # Longitude: -122.5 to -67.5
  # Resolution: 5 degrees
  #
  # Full cell boundaries:
  # Latitude: 20 to 50
  # Longitude: -125 to -65
  # ==========================================================

  geographic_grid <- reactive({

    req(
      input$variable,
      input$observation,
      input$surface
    )

    level_index <- selected_map_level()$pressure_level_index

    d <- data.table::copy(
      map_data()[
        variable == input$variable &
        observation_type == input$observation &
        surface_type == input$surface &
        pressure_level_index == level_index &
        is.finite(latitude_center) &
        is.finite(longitude_center) &
        is.finite(trend_per_decade)
      ]
    )

    validate(need(
      nrow(d) > 0,
      "No valid geographic trends for this selection."
    ))

    if ("n_years" %in% names(d)) {
      data.table::setorder(d, -n_years)
    }

    d <- unique(
      d,
      by = c("lat_bin", "lon_bin")
    )

    # The original data use 5-degree cells.
    resolution <- 5

    # Use cell boundaries, not centers, so the heatmap
    # fills the entire original 5-degree geographic cell.
    lon_edges <- seq(-125, -65, by = resolution)
    lat_edges <- seq(20, 50, by = resolution)

    lon_centers <- head(lon_edges, -1) + resolution / 2
    lat_centers <- head(lat_edges, -1) + resolution / 2

    z <- matrix(
      NA_real_,
      nrow = length(lat_centers),
      ncol = length(lon_centers)
    )

    # Match original cell centers to matrix positions.
    lon_index <- match(
      round(d$longitude_center, 6),
      round(lon_centers, 6)
    )

    lat_index <- match(
      round(d$latitude_center, 6),
      round(lat_centers, 6)
    )

    valid <- which(
      !is.na(lon_index) &
      !is.na(lat_index)
    )

    for (k in valid) {

      z[
        lat_index[k],
        lon_index[k]
      ] <- d$trend_per_decade[k]
    }

    list(
      lon_edges = lon_edges,
      lat_edges = lat_edges,
      lon_centers = lon_centers,
      lat_centers = lat_centers,
      z = z,
      original_cells = nrow(d),
      plotted_cells = sum(is.finite(z)),
      value_range = range(d$trend_per_decade)
    )

  }) |>
    shiny::bindCache(
      input$map_month,
      input$variable,
      input$observation,
      input$surface,
      selected_map_level()$pressure_level_index
    )

  # ==========================================================
  # 10. GEOGRAPHIC MAP
  #
  # Original 5-degree cells with real trend values.
  # The heatmap receives a full 2D matrix.
  # NA values remain blank.
  # ==========================================================

  output$map_plot <- renderPlotly({

    theme <- theme_colors()

    grid <- geographic_grid()
    max_abs <- map_color_limit()

    validate(need(
      grid$plotted_cells > 0,
      "No geographic cells could be plotted."
    ))

    # Explicit diverging scale.
    # Negative: blue
    # Zero: light
    # Positive: red

    colorscale <- list(
      c(0.00, "#2166AC"),
      c(0.25, "#92C5DE"),
      c(0.50, "#F7F7F7"),
      c(0.75, "#F4A582"),
      c(1.00, "#B2182B")
    )

    # Actual values are passed to Plotly.
    # Values outside the color limits are saturated
    # visually, but the hover shows the original value.

    p <- plotly::plot_ly(
    x = grid$lon_centers,
    y = grid$lat_centers,
    z = grid$z,
      type = "heatmap",

      zmin = -max_abs,
      zmax = max_abs,
      zmid = 0,

      zsmooth = FALSE,

      colorscale = colorscale,

      showscale = TRUE,

      colorbar = list(
        title = list(text = "Trend/decade"),
        thickness = 18,
        len = 0.80,
        x = 1.02,
        tickformat = ".3g"
      ),

      hoverongaps = FALSE,

      hovertemplate = paste0(
        "Latitude: %{y:.1f}",
        "<br>Longitude: %{x:.1f}",
        "<br>Trend/decade: %{z:.5g}",
        "<extra></extra>"
      )
    )

    # --------------------------------------------------------
    # State boundaries
    # --------------------------------------------------------

    states <- maps::map(
      "state",
      plot = FALSE,
      fill = FALSE
    )

    p <- plotly::add_trace(
      p,
      x = states$x,
      y = states$y,
      type = "scatter",
      mode = "lines",
      line = list(
        color = theme$border,
        width = 0.8
      ),
      hoverinfo = "skip",
      showlegend = FALSE,
      inherit = FALSE
    )

    # --------------------------------------------------------
    # Country boundaries
    # --------------------------------------------------------

    countries <- maps::map(
      "world",
      plot = FALSE,
      fill = FALSE
    )

    p <- plotly::add_trace(
      p,
      x = countries$x,
      y = countries$y,
      type = "scatter",
      mode = "lines",
      line = list(
        color = theme$border,
        width = 1.2
      ),
      hoverinfo = "skip",
      showlegend = FALSE,
      inherit = FALSE
    )

    level <- selected_map_level()

    title_text <- paste0(
      "Geographic Trends - ",
      month.name[as.integer(input$map_month)],
      " - ",
      signif(level$pressure_hPa, 5),
      " hPa (",
      tools::toTitleCase(observation_mode()),
      ")"
    )

    p <- plotly::layout(
      p,

      title = title_text,

      paper_bgcolor = theme$paper,
      plot_bgcolor = theme$plot,
      font = list(color = theme$text),

      showlegend = FALSE,

      xaxis = list(
        title = "Longitude",
        range = c(-125, -65),
        autorange = FALSE,
        fixedrange = TRUE,
        gridcolor = theme$grid,
        zeroline = FALSE,
        tickmode = "linear",
        dtick = 10
      ),

      yaxis = list(
        title = "Latitude",
        range = c(20, 50),
        autorange = FALSE,
        fixedrange = TRUE,
        gridcolor = theme$grid,
        zeroline = FALSE,
        tickmode = "linear",
        dtick = 5
      ),

      margin = list(
        l = 65,
        r = 115,
        t = 75,
        b = 75
      ),

      annotations = list(
        list(
          xref = "paper",
          yref = "paper",
          x = 0,
          y = -0.18,
          xanchor = "left",
          showarrow = FALSE,

          text = paste0(
            "Original cells: ",
            grid$original_cells,
            " | Plotted cells: ",
            grid$plotted_cells,
            " | Trend range: ",
            signif(grid$value_range[1], 4),
            " to ",
            signif(grid$value_range[2], 4),
            " | Shared color limit: +/-",
            signif(max_abs, 4)
          ),

          font = list(
            size = 10,
            color = theme$secondary
          )
        )
      )
    )

    plotly::config(
      p,
      scrollZoom = FALSE,
      displayModeBar = TRUE
    )
  })

}

