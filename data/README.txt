TEAM STARFLOCK - ATMOSPHERIC EXPLORER
DATA README
===============================

PURPOSE
-------
This directory contains derived atmospheric trend data prepared for the
Team Starflock R Shiny dashboard. The dashboard explores changes in
atmospheric variables by pressure level, month, observation type,
surface type, and geographic grid cell.

SOURCE AND PROVENANCE
---------------------
The project uses data derived from NASA Aqua Atmospheric Infrared Sounder
(AIRS) observations. The processing workflow discussed for this project
includes extracting source HDF4 data to Parquet (script 03) and producing
Shiny-ready CSV summaries (script 07).

These CSVs are derived analysis products, not original NASA AIRS files.
Consult the processing scripts and source-data metadata for the exact
AIRS product/version, retrieval quality filters, geographic coverage,
trend estimation method, uncertainty calculation, and processing dates.
Those details have not been independently verified for this README.

EXPECTED FILES
--------------
1. shiny_monthly_trends.csv
   Monthly trend estimates by variable, surface type, observation type,
   and atmospheric pressure level. Used for the altitude trend plot and
   seasonal heatmap.

2. shiny_yearly_variables.csv
   Yearly variable summary produced by the processing workflow.
   Its exact columns and aggregation method should be checked against
   the CSV header and script 07 before reuse.

3. shiny_spatial_trends_month_01.csv through
   shiny_spatial_trends_month_12.csv
   Month-specific spatial trend estimates. The two-digit suffix denotes
   calendar month (01 = January, 12 = December). Used by the geographic
   map and pressure-level slider.

MONTHLY TRENDS: COLUMN DEFINITIONS
----------------------------------
variable              Atmospheric variable identifier.
surface_type          Surface classification (for example, land/water).
observation_type      Observation classification (for example, day/night).
sampling_month        Calendar month as integer 1-12.
pressure_level_index  Index identifying an atmospheric pressure level.
pressure_hPa          Atmospheric pressure in hectopascals (hPa).
n_years               Number of years contributing to a trend estimate.
first_year            First contributing year.
last_year             Last contributing year.
trend_per_decade      Estimated change per decade in the variable's units.
ci_low                Lower bound of the reported trend confidence interval.
ci_high               Upper bound of the reported trend confidence interval.
p_value               Reported statistical p-value for the trend.

SPATIAL TRENDS: ADDITIONAL COLUMNS
----------------------------------
The geographic CSVs include the fields above and also:

lat_bin               Latitude grid-bin identifier.
lon_bin               Longitude grid-bin identifier.
latitude_center       Center latitude of the geographic grid cell, degrees.
longitude_center      Center longitude of the geographic grid cell, degrees.
grid_degrees          Grid-cell size in degrees.

The dashboard draws geographic cells centered at latitude_center and
longitude_center, using grid_degrees to determine cell width. The
project has been using approximately 5-degree by 5-degree cells.

VARIABLES AND UNITS
-------------------
Known identifiers discussed in the project include:

temperature_K         Temperature; trend units K/decade.
water_vapor_mmr       Water-vapor mass mixing ratio; verify source units.
ozone_vmr             Ozone volume mixing ratio; verify source units.
co_vmr                Carbon monoxide volume mixing ratio; verify units.
methane_vmr           Methane volume mixing ratio; verify source units.
cloud_fraction        Cloud fraction; may not have a pressure-level axis.

For variables other than temperature_K, consult the extraction script
or source metadata for the numeric units and any scaling factors before
labeling axes or interpreting trend magnitudes. A trend is expressed
per decade in the units of its corresponding variable.

HOW THE SHINY APP USES THE DATA
-------------------------------
- Altitude plot: filters shiny_monthly_trends.csv by variable,
  observation type, surface type, and month; plots trend versus hPa.
- Seasonal heatmap: plots monthly trends by pressure level.
- Geographic map: reads the selected monthly spatial CSV and filters
  by variable, observation type, surface type, and pressure-level index.
- Pressure slider: chooses a discrete pressure-level index; the
  displayed hPa is a representative pressure for that index.
- Map colors: blue for negative trends, near-white for zero, and red
  for positive trends. The current map implementation scales colors
  symmetrically around zero using the 95th percentile of absolute
  displayed trends. This affects visualization, not the stored values.

INTERPRETATION AND CAUTIONS
---------------------------
- Pressure is not altitude: lower pressure generally corresponds to
  higher altitude, but the relationship varies with atmospheric state.
- Trend estimates and confidence intervals should be interpreted with
  the underlying regression method and sampling criteria in mind.
- Missing/NA values indicate unavailable or uncomputed values; do not
  treat them as zero.
- Not every variable has measurements at every pressure level.
- Distinct rows may legitimately share a geographic cell because they
  represent different variables, observation types, surfaces, or levels.
- If multiple rows remain for the same cell after all selections, review
  the aggregation logic before plotting or dropping duplicates.
- Grid cells are analysis bins, not precise political boundaries.
- The map may clip extreme values for color display; hover values
  should retain the actual unmodified trend estimates.

DIRECTORY LAYOUT (EXAMPLE)
--------------------------
altitudes/
  global.R
  ui.R
  server.R
  data/
    README.txt
    shiny_monthly_trends.csv
    shiny_yearly_variables.csv
    shiny_spatial_trends_month_01.csv
    ...
    shiny_spatial_trends_month_12.csv

REPRODUCIBILITY CHECKLIST
-------------------------
Before publication or redistribution, record:
- Exact AIRS dataset/product ID, version, and download dates.
- Source-data citation and NASA data-access URL.
- Geographic and temporal filters.
- Observation-type and surface-type classification rules.
- Quality-control criteria and missing-value handling.
- Variable units and transformations.
- Trend model, significance test, confidence level, and sample thresholds.
- Processing script versions, software versions, and execution date.

This README documents the file structure discussed for the current
Starflock dashboard. It is not a substitute for the source metadata
or an audit of the data-generation scripts.
