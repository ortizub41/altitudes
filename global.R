# Team Starflock - Atmospheric Explorer
library(shiny)
library(plotly)
library(data.table)
library(maps)
library(shinycssloaders)
library(leaflet)

trends_path <- 'data/shiny_monthly_trends.csv'
if (!file.exists(trends_path)) stop('Missing: ', trends_path)
trends <- fread(trends_path)
required <- c('variable','surface_type','observation_type','sampling_month',
              'pressure_hPa','trend_per_decade','ci_low','ci_high','p_value')
missing <- setdiff(required, names(trends))
if (length(missing)) stop('Missing trend columns: ', paste(missing, collapse=', '))
for (nm in c('sampling_month','pressure_hPa','trend_per_decade','ci_low','ci_high','p_value'))
  set(trends, j=nm, value=as.numeric(trends[[nm]]))
# Retain all variables, including cloud_fraction.
# Geographic map data is loaded separately in server.R.
profile_variables <- sort(unique(trends[is.finite(pressure_hPa) & pressure_hPa>0 &
                                         is.finite(trend_per_decade), variable]))
all_variables <- sort(unique(na.omit(trends$variable)))
month_choices <- c('All Months'='all', setNames(as.character(1:12), month.name))
message('Loaded ', nrow(trends), ' monthly trend records')
