TEAM STARFLOCK - ATMOSPHERIC EXPLORER
DATA README
=====================================
Updated: November 2026

PURPOSE
-------
This folder contains derived NASA Aqua AIRS atmospheric trend data for
Team Starflock's R Shiny dashboard. The app explores trends by pressure,
month, observation type, surface type, and geographic cell.

SOURCE AND DATA FILES
---------------------
The discussed workflow converts source HDF4 data to Parquet (script 03)
and produces Shiny-ready CSV summaries (script 07). Exact AIRS product
identifiers, versions, quality criteria, trend methods and source units
must be verified from the processing scripts and source metadata.

shiny_monthly_trends.csv
  Monthly trend estimates by variable, surface, observation and pressure.

shiny_yearly_variables.csv (if present)
  Yearly summaries; confirm fields and aggregation against script 07.

shiny_spatial_trends_month_01.csv through month_12.csv
  Month-specific geographic trends (January through December).

IMPORTANT COLUMNS
-----------------
variable              Atmospheric variable identifier
surface_type          Land/water or other source surface category
observation_type      Day/night/combined observation category
sampling_month        Calendar month, 1-12
pressure_level_index  Pressure-level identifier
pressure_hPa          Pressure in hectopascals
n_years               Number of contributing years
first_year, last_year First and last contributing years
trend_per_decade      Trend per decade in the variable's units
ci_low, ci_high       Confidence-interval bounds
p_value               Significance-test p-value
lat_bin, lon_bin      Spatial grid-bin identifiers
latitude_center       Center latitude in degrees
longitude_center      Center longitude in degrees
grid_degrees          Width of geographic cell in degrees

DASHBOARD VIEWS
---------------
1. ALTITUDE TRENDS
   Displays trend per decade versus atmospheric pressure (hPa), using a
   reversed logarithmic pressure axis as a proxy for altitude. Supports
   dots, lines, and lines+dots. Axis ranges stay fixed across months for
   the same variable/surface/observation selection. Atmospheric-layer
   labels appear inside the plot. The legend appears for All Months.

2. SEASONAL HEATMAP
   Displays trend per decade by calendar month and pressure level.

3. GEOGRAPHIC MAP
   Displays the ORIGINAL geographic trend cells, with no interpolation
   or smoothing. The current implementation uses a Plotly Cartesian
   heatmap with state and country boundary overlays, not scattergeo.

   The tested source grid uses 5-degree by 5-degree cells:
     Latitude centers:    22.5 to 47.5 degrees north
     Longitude centers: -122.5 to -67.5 degrees
     Cell boundaries:   latitude 20 to 50, longitude -125 to -65

   A tested January selection contained 42 valid cells. Other selections
   may contain different numbers of cells. Missing cells remain blank,
   not zero. The map shows the original data values.

   IMPORTANT PLOTTING FIX:
     x = grid$lon_centers
     y = grid$lat_centers
     z = grid$z

   x and y must align with the columns and rows of z, respectively.
   Using edge coordinates with a center-based z matrix caused the
   map to display incorrectly (sometimes only one square).

MAP COLOR SCALE
---------------
Blue indicates negative trends; near-white indicates values near zero;
red indicates positive trends. The scale is symmetric around zero.

The color limit is the 95th percentile of absolute trend values pooled
from all 12 months for the SAME variable, observation, surface and
pressure-level index. This permits month-to-month color comparison
within that selection. Changing those filters can change the scale.

Extreme values may saturate at the end colors, but hover information
shows the original trend values. The map footer shows original and
plotted cell counts, the observed range, and the shared color limit.

VARIABLES AND UNITS
-------------------
temperature_K      Temperature; trend units K/decade
water_vapor_mmr    Water-vapor mass mixing ratio; verify units
ozone_vmr          Ozone volume mixing ratio; verify units
co_vmr             Carbon monoxide volume mixing ratio; verify units
methane_vmr        Methane volume mixing ratio; verify units
cloud_fraction     Cloud fraction; may not have a pressure axis

INTERPRETATION
--------------
- Atmospheric pressure is not geometric altitude; lower pressure
  generally corresponds to higher altitude.
- Missing cells do not mean zero trend.
- Grid cells are analysis bins, not political boundaries.
- Colors alone do not establish statistical significance; consult
  confidence intervals, p-values, sample counts and data quality.
- The current server.R keeps one row per lat_bin/lon_bin after filtering,
  preferring larger n_years where available. Review duplicates if needed.

RUN LOCALLY
-----------
shiny::runApp("/Users/juanortiz/altitudes")

DEPLOY
------
rsconnect::deployApp(
  appDir = "/Users/juanortiz/altitudes",
  appName = "Starflock-Altitudes",
  launch.browser = TRUE
)

The app requires R packages including shiny, plotly, data.table and maps.
Keep all required CSVs inside the app directory when deploying.

TROUBLESHOOTING
---------------
Only one map square:
  Verify that lon_centers and lat_centers, not edge arrays, are passed
  to the heatmap. Check ncol(z) == length(x), nrow(z) == length(y).

Mostly blank map:
  Inspect the plotted-cell count and filters. Missing source cells
  are intentionally blank.

Uniform map colors:
  Compare the trend range with the shared color limit in the footer.

No altitude trends:
  Verify global.R loads data/shiny_monthly_trends.csv and check filters.

REPRODUCIBILITY
---------------
Before publication, record the exact AIRS product/version and citation,
download dates, spatial/temporal filters, quality criteria, variable units,
trend method, sample thresholds, confidence intervals, and script versions.

This README documents the current discussed implementation; it is not an
independent validation of the underlying NASA data or processing.
