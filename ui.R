
# ============================================================
# Team Starflock - Atmospheric Explorer
# ui.R
# ============================================================

ui <- fluidPage(

  titlePanel("Team Starflock - Atmospheric Explorer"),

  sidebarLayout(

    sidebarPanel(

      # ------------------------------------------------------
      # Main controls
      # ------------------------------------------------------

      selectInput(
        "variable",
        "Variable:",
        choices = profile_variables,
        selected = if ("temperature_K" %in% profile_variables) {
          "temperature_K"
        } else {
          profile_variables[1]
        }
      ),

      selectInput(
        "observation",
        "Observation:",
        choices = c(
          "Combined" = "combined",
          "Day" = "day",
          "Night" = "night"
        ),
        selected = "day"
      ),

      selectInput(
        "surface",
        "Surface:",
        choices = c(
          "All" = "all",
          "Land" = "land",
          "Water" = "water"
        ),
        selected = "land"
      ),

      # ------------------------------------------------------
      # Altitude Trends controls
      # ------------------------------------------------------

      conditionalPanel(
        "input.view_tab == 'Altitude Trends'",

        selectInput(
          "month",
          "Month:",
          choices = month_choices,
          selected = "0"
        ),

        selectInput(
          "plot_style",
          "Plot Style:",
          choices = c(
            "Dots Only" = "markers",
            "Lines Only" = "lines",
            "Lines + Dots" = "lines+markers"
          ),
          selected = "lines+markers"
        )
      ),

      # ------------------------------------------------------
      # Geographic Map controls
      # ------------------------------------------------------

      conditionalPanel(
        "input.view_tab == 'Geographic Map'",

        selectInput(
          "map_month",
          "Map Month:",
          choices = setNames(
            as.character(1:12),
            month.name
          ),
          selected = "1"
        ),

        sliderInput(
          "map_pressure_index",
          "Map Pressure Level - Proxy for Altitude:",
          min = 1,
          max = 2,
          value = 1,
          step = 1,
          ticks = FALSE,
          animate = animationOptions(
            interval = 900,
            loop = FALSE
          )
        ),

        textOutput("map_pressure_label")
      ),

      # ------------------------------------------------------
      # Description
      # ------------------------------------------------------

      helpText(
        paste(
          "Monthly trend estimates use the sampled AIRS",
          "observations; pressure is a proxy for altitude."
        )
      ),

      # ------------------------------------------------------
      # Project Details link
      # Opens Google Slides in a NEW browser tab
      # ------------------------------------------------------

      tags$div(
        style = paste(
          "margin-top: 20px;",
          "padding-top: 15px;",
          "border-top: 1px solid #dddddd;"
        ),

        tags$a(
          href = paste0(
            "https://docs.google.com/presentation/d/",
            "1PDlWuj_W0Z8IL2xRHmcPQSQ-zILl6o9jHYNG5nlIVGw/",
            "edit?slide=id.h39f49cc29aa86a15_0_0",
            "#slide=id.h39f49cc29aa86a15_0_0"
          ),
          target = "_blank",
          rel = "noopener noreferrer",
          style = paste(
            "font-size: 15px;",
            "font-weight: 500;",
            "color: #337ab7;",
            "text-decoration: none;"
          ),
          "Project Details"
        )
      )

    ),

    # ========================================================
    # Main content
    # ========================================================

    mainPanel(

      tabsetPanel(

        id = "view_tab",

        # ----------------------------------------------------
        # TAB 1: Altitude Trends
        # ----------------------------------------------------

        tabPanel(
          "Altitude Trends",

          plotlyOutput(
            "altitude_plot",
            height = "650px"
          )
        ),

        # ----------------------------------------------------
        # TAB 2: Seasonal Heatmap
        # ----------------------------------------------------

        tabPanel(
          "Seasonal Heatmap",

          plotlyOutput(
            "heatmap_plot",
            height = "650px"
          )
        ),

        # ----------------------------------------------------
        # TAB 3: Geographic Map
        # ----------------------------------------------------

        tabPanel(
          "Geographic Map",

          shinycssloaders::withSpinner(

            plotlyOutput(
              "map_plot",
              height = "650px"
            ),

            type = 4,
            color = "#2166AC",
            size = 1.5
          ),

          helpText(
            paste(
              "Colored squares represent 5° × 5° geographic",
              "grid cells. Values are not interpolated and",
              "may extend beyond coastlines."
            )
          )
        )

      )

    )

  )

)

