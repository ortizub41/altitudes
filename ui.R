# ============================================================
# Team Starflock - ALTITUDES Dashboard
# ui.R - Google Analytics 4 enabled
# ============================================================

format_variable_label <- function(x) {
  custom_labels <- c(
    temperature_K = "Air Temperature (K)",
    surface_temperature_K = "Surface Temperature (K)",
    relative_humidity = "Relative Humidity (%)",
    specific_humidity = "Specific Humidity (kg/kg)",
    water_vapor = "Water Vapor (kg/m2)",
    cloud_fraction = "Cloud Coverage (fraction)",
    ozone = "Ozone (ppbv)",
    carbon_monoxide = "Carbon Monoxide (ppbv)",
    geopotential_height = "Geopotential Height (m)"
  )
  if (x %in% names(custom_labels)) return(unname(custom_labels[x]))
  tools::toTitleCase(gsub("_", " ", x, fixed = TRUE))
}

variable_choices <- setNames(
  profile_variables,
  vapply(profile_variables, format_variable_label, character(1))
)

ui <- fluidPage(
  tags$head(
    tags$script(async = NA, src = "https://www.googletagmanager.com/gtag/js?id=G-47CHPS5PF4"),
    tags$script(HTML("
      window.dataLayer = window.dataLayer || [];
      function gtag(){dataLayer.push(arguments);}
      gtag('js', new Date());
      gtag('config', 'G-47CHPS5PF4');

      // Track interactions only, without collecting visitor-entered text.
      $(document).on('shiny:connected', function() {
        var previous = {};
        var tracked = {
          view_tab: 'dashboard_tab',
          variable: 'atmospheric_variable',
          observation: 'observation_type',
          surface: 'surface_type',
          month: 'trend_month',
          plot_style: 'plot_style',
          map_month: 'map_month',
          map_basemap: 'map_style',
          map_pressure_index: 'altitude_level'
        };
        Object.keys(tracked).forEach(function(id) {
          var el = document.getElementById(id);
          if (!el) return;
          var initial = (id === 'view_tab')
            ? $('.nav-tabs li.active a').text().trim()
            : $(el).val();
          previous[id] = initial;
        });
        $(document).on('shiny:inputchanged', function(event) {
          var id = event.name;
          if (!Object.prototype.hasOwnProperty.call(tracked, id)) return;
          var value = String(event.value);
          if (previous[id] === value) return;
          previous[id] = value;
          if (typeof window.gtag === 'function') {
            window.gtag('event', tracked[id], {
              selection: value,
              interaction_type: id
            });
          }
        });
      });
    ")),
    tags$style(HTML("
      @media (min-width: 992px) {
        .container-fluid > .row > .col-sm-4 { width: 20%; }
        .container-fluid > .row > .col-sm-8 { width: 80%; }
      }
      .container-fluid { padding-top: 10vh; padding-bottom: 10vh; }
      .nav-tabs { display: flex; gap: 10px; border-bottom: 2px solid #2166AC;
        padding-bottom: 8px; margin-bottom: 15px; }
      .nav-tabs > li > a { background-color: #E8EEF5; color: #24476B;
        font-size: 16px; font-weight: 600; padding: 14px 22px;
        border: 1px solid #CBD8E6; border-radius: 8px; }
      .nav-tabs > li > a:hover { background-color: #D0E2F3; color: #174A78; }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus { background-color: #2166AC;
        color: white; border: 1px solid #2166AC; font-weight: 700; }
      @media (max-width: 767px) {
        .nav-tabs { flex-wrap: wrap; }
        .nav-tabs > li > a { font-size: 14px; padding: 10px 14px; }
      }
    "))
  ),
  sidebarLayout(
    sidebarPanel(
      tags$h3("ALTITUDES Dashboard"),
      selectInput("variable", "Atmospheric Variable:", choices = variable_choices,
        selected = if ("temperature_K" %in% profile_variables) "temperature_K" else profile_variables[1]),
      selectInput("observation", "Time of Observation:",
        choices = c("Day & Night Combined" = "combined", "Daytime" = "day", "Nighttime" = "night"),
        selected = "day"),
      selectInput("surface", "Surface Type:",
        choices = c("All Surfaces" = "all", "Land Only" = "land", "Ocean / Water Only" = "water"),
        selected = "land"),
      conditionalPanel(
        "input.view_tab == 'Altitude Trends'",
        selectInput("month", "Month:", choices = month_choices, selected = "all"),
        selectInput("plot_style", "Plot Style:",
          choices = c("Dots Only" = "markers", "Lines Only" = "lines", "Lines + Dots" = "lines+markers"),
          selected = "lines+markers")
      ),
      conditionalPanel(
        "input.view_tab == 'Geographic Map'",
        selectInput("map_month", "Month:",
          choices = setNames(as.character(1:12), month.name), selected = "1"),
        sliderInput("map_pressure_index", "Atmospheric Altitude:",
          min = 1, max = 2, value = 1, step = 1, ticks = FALSE,
          animate = animationOptions(interval = 900, loop = FALSE)),
        tags$div(style = "margin: 8px 0 14px; font-size: 15px; font-weight: 600;",
          textOutput("map_pressure_label")),
        selectInput("map_basemap", "Map Style:",
          choices = c("Terrain" = "terrain", "Satellite" = "satellite", "Light / Simple" = "light"),
          selected = "terrain")
      ),
      helpText("Monthly trend estimates use the sampled AIRS observations; pressure is a proxy for altitude."),
      tags$div(
        style = "margin-top: 20px; padding-top: 15px; border-top: 1px solid #dddddd;",
        tags$a(
          href = paste0(
            "https://docs.google.com/presentation/d/",
            "1PDlWuj_W0Z8IL2xRHmcPQSQ-zILl6o9jHYNG5nlIVGw/",
            "edit?slide=id.h39f49cc29aa86a15_0_0",
            "#slide=id.h39f49cc29aa86a15_0_0"
          ),
          target = "_blank", rel = "noopener noreferrer",
          style = "font-size: 15px; font-weight: 500; color: #337ab7; text-decoration: none;",
          "Project Details"
        ),
        tags$div(
          style = "margin-top: 18px; font-size: 13px; color: #666666;",
          tags$strong("Created by:"),
          tags$div(style = "margin-top: 5px; line-height: 1.6;",
            "Gaelle Muller-Greven", tags$br(), "Juan Ortiz")
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "view_tab",
        tabPanel("Altitude Trends",
          plotlyOutput("altitude_plot", height = "calc(80vh - 55px)")),
        tabPanel("Seasonal Heatmap",
          plotlyOutput("heatmap_plot", height = "calc(80vh - 55px)")),
        tabPanel("Geographic Map",
          shinycssloaders::withSpinner(
            leaflet::leafletOutput("map_plot", height = "calc(80vh - 55px)"),
            type = 4, color = "#2166AC", size = 1.5
          ),
          helpText(paste(
            "Colored squares represent 5 x 5 degree geographic grid cells.",
            "Values are not interpolated and may extend beyond coastlines.",
            "Terrain is a background layer; colored cells show NASA trend measurements."
          ))
        )
      )
    )
  )
)
