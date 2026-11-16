# 07 AIRS multi-variable early-month trends and Shiny-ready summaries
# Run after 03 extraction; does NOT alter existing temperature analysis outputs.
# Requires: arrow, data.table, ggplot2. Memory-conscious per-granule caching.
INPUT_ROOT <- '/Users/gaellemuller-greven/spaceapps2026_data_parquet'
ANALYSIS_ROOT <- '/Users/gaellemuller-greven/spaceapps2026_analysis'
OUT <- file.path(ANALYSIS_ROOT,'multivariable')
CACHE <- file.path(OUT,'granule_cache_landfrac_70_30')
SPATIAL_YEARLY <- file.path(OUT,'spatial_yearly_cache_landfrac_70_30')
dir.create(CACHE, recursive=TRUE, showWarnings=FALSE)
dir.create(SPATIAL_YEARLY, recursive=TRUE, showWarnings=FALSE)
QC_ALLOWED <- 0L   # provisional QC selection; verify per-variable QC meanings
GRID_DEGREES <- 5
LAND_MIN <- 0.70  # landFrac >= 0.70 => land
WATER_MAX <- 0.30 # landFrac <= 0.30 => water; intermediate footprints only in 'all'
MIN_YEARS <- 10L
MAX_GRANULES <- Inf
# Keep FALSE unless validated; no arbitrary numerical range filter for gas mixing ratios.
REQUIRE_BOTH_DAY_NIGHT <- FALSE
suppressPackageStartupMessages({library(arrow);library(data.table);library(ggplot2)})
vars <- c(temperature_K='temperature_K',water_vapor_mmr='water_vapor_mmr',
          ozone_vmr='ozone_vmr',co_vmr='co_vmr',methane_vmr='methane_vmr')
profile_files <- sort(list.files(INPUT_ROOT,pattern='_profiles\\.parquet$',recursive=TRUE,full.names=TRUE))
if (!length(profile_files)) stop('No profile Parquet files in ',INPUT_ROOT)
profile_files <- head(profile_files,MAX_GRANULES)
cat('Processing / resuming',length(profile_files),'granules\n')
# Per-granule files contain sums and counts in 5-degree spatial cells.
# Cache version changed: use distinct directory to avoid reusing unsplit caches.
for (i in seq_along(profile_files)) {
  pf <- profile_files[i]
  stem <- sub('_profiles\\.parquet$','',basename(pf))
  folder <- basename(dirname(pf))
  dest <- file.path(CACHE,paste0(folder,'__',stem,'.rds'))
  if (file.exists(dest)) next
  of <- file.path(dirname(pf),paste0(stem,'_observations.parquet'))
  if (!file.exists(of)) {warning('Missing observations: ',of);next}
  tryCatch({
    o <- as.data.table(read_parquet(of))
    need <- c('GeoTrack','GeoXTrack','year','sampling_month','time_of_day','latitude','longitude')
    if (!all(need %in% names(o))) stop('Missing observation fields')
    o <- o[is.finite(latitude)&is.finite(longitude)&latitude>=24&latitude<=50&
             longitude>=-125&longitude<=-66 & !is.na(year) & !is.na(sampling_month)]
    # Land fraction must be available at footprint level, not pressure level.
    # Check observations first, then a matching cloud Parquet file if needed.
    land_candidates <- c('landFrac','land_fraction','LandFrac','landfrac')
    land_col <- intersect(land_candidates,names(o))
    if (!length(land_col)) {
      cf <- file.path(dirname(pf),paste0(stem,'_clouds.parquet'))
      if (file.exists(cf)) {
        cs <- names(arrow::read_parquet(cf,as_data_frame=FALSE))
        cc <- intersect(land_candidates,cs)
        if (length(cc) && all(c('GeoTrack','GeoXTrack') %in% cs)) {
          lc <- as.data.table(read_parquet(cf,col_select=c('GeoTrack','GeoXTrack',cc[1])))
          if (anyDuplicated(lc,by=c('GeoTrack','GeoXTrack'))) stop('Duplicate cloud landFrac keys')
          setnames(lc,cc[1],'landFrac')
          o <- merge(o,lc,by=c('GeoTrack','GeoXTrack'),all.x=TRUE,sort=FALSE)
          land_col <- 'landFrac'
        }
      }
    }
    if (!length(land_col)) {
      stop('No landFrac in observations or clouds Parquet; cannot create land/water splits. Check extraction.')
    }
    if (anyDuplicated(o,by=c('GeoTrack','GeoXTrack'))) stop('Duplicate observation keys')
    land_values <- as.numeric(o[[land_col[1]]])
    # Fraction 0..1; do not guess whether 0..100 percentage was used.
    if (any(is.finite(land_values) & (land_values < 0 | land_values > 1)))
      stop('landFrac values outside 0..1; inspect units before classifying')
    o[, landFrac := land_values]
    o[, land_class := fifelse(is.finite(landFrac) & landFrac >= LAND_MIN,'land',
                       fifelse(is.finite(landFrac) & landFrac <= WATER_MAX,'water','mixed'))]
    # 'all' retains mixed and missing landFrac; land/water are disjoint.
    o <- rbindlist(list(copy(o)[,surface_type:='all'],
      copy(o)[land_class %in% c('land','water')][,surface_type:=land_class]),use.names=TRUE,fill=TRUE)
    o[, `:=`(lat_bin=floor(latitude/GRID_DEGREES)*GRID_DEGREES,
             lon_bin=floor(longitude/GRID_DEGREES)*GRID_DEGREES)]
    keys <- c('GeoTrack','GeoXTrack')
    # Cloud fraction is ONE observation per footprint (not per pressure level).
    cloud <- data.table()
    if ('CldFrcTot' %in% names(o)) {
      z <- copy(o)
      if ('CldFrcTot_QC' %in% names(z)) z <- z[is.na(CldFrcTot_QC)|CldFrcTot_QC %in% QC_ALLOWED]
      z <- z[is.finite(CldFrcTot)&CldFrcTot>=0&CldFrcTot<=1]
      cloud <- z[,.(sum_value=sum(CldFrcTot),n=.N),by=.(year,sampling_month,time_of_day,surface_type,lat_bin,lon_bin)]
      cloud[,`:=`(variable='cloud_fraction',pressure_level_index=NA_integer_,pressure_hPa=NA_real_)]
    }
    cols <- c(keys,'pressure_level_index','pressure_hPa',unname(vars))
    # Arrow reads only selected columns; QC fields optional for some products.
    schema_names <- names(arrow::read_parquet(pf,as_data_frame=FALSE))
    selected <- c(cols,paste0(unname(vars),'_QC'))
    p <- as.data.table(read_parquet(pf,col_select=intersect(selected,schema_names)))
    p <- merge(p,o[,c(keys,'year','sampling_month','time_of_day','surface_type','lat_bin','lon_bin'),with=FALSE],by=keys)
    parts <- list()
    for (v in unname(vars)) {
      if (!v %in% names(p)) next
      qc <- paste0(v,'_QC')
      z <- p[is.finite(get(v)) & get(v)!=-9999]
      if (qc %in% names(z)) z <- z[!is.na(get(qc)) & get(qc) %in% QC_ALLOWED]
      if (v=='temperature_K') z <- z[get(v)>100 & get(v)<400]
      else z <- z[get(v)>=0]
      if (!nrow(z)) next
      s <- z[,.(sum_value=sum(get(v)),n=.N),by=.(year,sampling_month,time_of_day,
                        pressure_level_index,pressure_hPa,surface_type,lat_bin,lon_bin)]
      s[,variable:=v]
      parts[[length(parts)+1L]] <- s
    }
    if (nrow(cloud)) parts[[length(parts)+1L]] <- cloud
    result <- if(length(parts)) rbindlist(parts,use.names=TRUE,fill=TRUE) else data.table()
    saveRDS(result,dest,compress='gzip')
    rm(o,p,result,parts);gc(verbose=FALSE)
  },error=function(e) warning('FAILED ',stem,': ',conditionMessage(e)))
  if (i %% 50L == 0L || i==length(profile_files)) cat('Granules:',i,'/',length(profile_files),'\n')
}
files <- list.files(CACHE,pattern='\\.rds$',full.names=TRUE)
if (is.finite(MAX_GRANULES)) {
  wanted <- paste0(basename(dirname(profile_files)),'__',sub('_profiles\\.parquet$','',basename(profile_files)),'.rds')
  files <- files[basename(files) %in% wanted]
}
if (!length(files)) stop('No cache files available')
# Group cache files by year/month/day-night from their names, avoiding one huge rbind.
base <- basename(files)
year <- as.integer(sub('.*AIRS\\.([0-9]{4})\\..*','\\1',base))
month <- as.integer(sub('.*AIRS\\.[0-9]{4}\\.([0-9]{2})\\..*','\\1',base))
period <- sub('^([0-9]{2}_(day|night))__.*','\\2',base)
manifest <- data.table(file=files,year=year,month=month,period=period)
manifest <- manifest[is.finite(year)&is.finite(month)&period %in% c('day','night')]
if (!nrow(manifest)) stop('Could not parse cache filenames')
annual_parts <- list()
groups <- unique(manifest[,.(year,month,period)])
for (k in seq_len(nrow(groups))) {
  g <- groups[k]
  ff <- manifest[year==g$year & month==g$month & period==g$period,file]
  dt <- rbindlist(lapply(ff,readRDS),use.names=TRUE,fill=TRUE)
  if (!nrow(dt)) next
  # Within a year/month/day-night, average each occupied cell, then average cells equally.
  cell <- dt[,.(sum_value=sum(sum_value),n=sum(n)),by=.(year,sampling_month,time_of_day,
                      variable,pressure_level_index,pressure_hPa,surface_type,lat_bin,lon_bin)]
  cell[,value:=sum_value/n]
  # Persist spatial annual means before collapsing geography. This is reusable for the map.
  spatial_file <- file.path(SPATIAL_YEARLY,sprintf('%04d_%02d_%s.csv',g$year,g$month,g$period))
  fwrite(cell[,.(year,sampling_month,time_of_day,variable,pressure_level_index,pressure_hPa,surface_type,
                 lat_bin,lon_bin,mean_value=value,n_measurements=n)],spatial_file)
  a <- cell[,.(mean_value=mean(value),n_cells=.N,n_measurements=sum(n)),
    by=.(year,sampling_month,time_of_day,variable,surface_type,pressure_level_index,pressure_hPa)]
  annual_parts[[length(annual_parts)+1L]] <- a
  if (k %% 20L==0L) cat('Aggregated groups:',k,'/',nrow(groups),'\n')
}
if (!length(annual_parts)) stop('No usable measurements; check QC and geographic coverage')
annual <- rbindlist(annual_parts,use.names=TRUE,fill=TRUE)
annual[,observation_type:=tolower(time_of_day)]
# The combined series gives day and night equal weight where both exist.
combined <- annual[,.(mean_value=mean(mean_value),n_cells=sum(n_cells),
  n_measurements=sum(n_measurements),n_daynight=.N),
  by=.(year,sampling_month,variable,surface_type,pressure_level_index,pressure_hPa)]
if (REQUIRE_BOTH_DAY_NIGHT) combined <- combined[n_daynight==2L]
combined[,observation_type:='combined']
annual[,n_daynight:=1L]
all_yearly <- rbindlist(list(annual[,setdiff(names(annual),'time_of_day'),with=FALSE],combined),fill=TRUE)
setcolorder(all_yearly,c('variable','surface_type','observation_type','year','sampling_month','pressure_level_index','pressure_hPa',
                       'mean_value','n_cells','n_measurements','n_daynight'))
setorder(all_yearly,variable,observation_type,sampling_month,pressure_level_index,year)
fwrite(all_yearly,file.path(OUT,'shiny_yearly_variables.csv'))
fit <- function(d) {
  d <- d[is.finite(mean_value)&is.finite(year)]
  n <- uniqueN(d$year)
  res <- data.table(n_years=n,first_year=if(n) min(d$year) else NA_integer_,
    last_year=if(n) max(d$year) else NA_integer_,trend_per_decade=NA_real_,
    ci_low=NA_real_,ci_high=NA_real_,p_value=NA_real_)
  if (n < MIN_YEARS) return(res)
  m <- lm(mean_value~year,data=d)
  ci <- confint(m,'year')*10
  res[,`:=`(trend_per_decade=unname(coef(m)['year']*10),
    ci_low=unname(ci[1]),ci_high=unname(ci[2]),
    p_value=unname(coef(summary(m))['year','Pr(>|t|)']))]
  res
}
trends <- all_yearly[,fit(.SD),by=.(variable,surface_type,observation_type,sampling_month,pressure_level_index,pressure_hPa)]
fwrite(trends,file.path(OUT,'shiny_monthly_trends.csv'))
# Cloud-adjusted sensitivity analysis on matched year/month/observation-type summaries.
# Important: national-level cloud adjustment is ecological and NOT causal.
temp <- all_yearly[variable=='temperature_K']
cld <- all_yearly[variable=='cloud_fraction',.(year,sampling_month,surface_type,observation_type,cloud_fraction=mean_value)]
matched <- merge(temp,cld,by=c('year','sampling_month','surface_type','observation_type'))
adjust <- function(d) {
  d <- d[is.finite(mean_value)&is.finite(cloud_fraction)]
  if (uniqueN(d$year)<MIN_YEARS || uniqueN(d$cloud_fraction)<3L) return(data.table(
    n_years=uniqueN(d$year),trend_C_per_decade=NA_real_,ci_low=NA_real_,ci_high=NA_real_,p_value=NA_real_))
  m <- lm(mean_value~year+cloud_fraction,data=d)
  ci <- tryCatch(confint(m,'year')*10,error=function(e)c(NA_real_,NA_real_))
  data.table(n_years=uniqueN(d$year),trend_C_per_decade=unname(coef(m)['year']*10),
    ci_low=unname(ci[1]),ci_high=unname(ci[2]),p_value=unname(coef(summary(m))['year','Pr(>|t|)']))
}
if (nrow(matched)) {
  adj <- matched[,adjust(.SD),by=.(surface_type,observation_type,sampling_month,pressure_level_index,pressure_hPa)]
  fwrite(adj,file.path(OUT,'temperature_cloud_adjusted_exploratory.csv'))
}
# Spatial trends: load one calendar month and observation type at a time, never all cells/years at once.
# A 5x5 degree cell is a coarse rectangular geographic bin, NOT an equal-area US polygon.
# Only estimates with >= MIN_YEARS have non-missing slopes. Outputs include cloud fraction
# with pressure_hPa=NA, so the map should not apply an altitude filter to cloud fraction.
spatial_trend_files <- character()
for (mm in 1:12) {
  month_files <- list.files(SPATIAL_YEARLY,pattern=sprintf('^[0-9]{4}_%02d_(day|night)\\.csv$',mm),full.names=TRUE)
  if (!length(month_files)) next
  byperiod <- rbindlist(lapply(month_files,fread),fill=TRUE,use.names=TRUE)
  if (!nrow(byperiod)) next
  byperiod[,observation_type:=tolower(time_of_day)]
  # Combine day/night at the SAME cell/year/pressure; do not mix national averages.
  combo <- byperiod[,.(mean_value=mean(mean_value),n_measurements=sum(n_measurements),
                        n_daynight=.N),
    by=.(year,sampling_month,variable,pressure_level_index,pressure_hPa,surface_type,lat_bin,lon_bin)]
  if (REQUIRE_BOTH_DAY_NIGHT) combo <- combo[n_daynight==2L]
  combo[,observation_type:='combined']
  byperiod[,n_daynight:=1L]
  z <- rbindlist(list(byperiod[,.(year,sampling_month,variable,surface_type,pressure_level_index,pressure_hPa,
                                 lat_bin,lon_bin,mean_value,n_measurements,n_daynight,observation_type)],
                       combo),fill=TRUE)
  # Fit using year-level means in each geographic cell; no satellite-footprint pseudoreplication.
  maptrends <- z[,fit(.SD),by=.(variable,surface_type,observation_type,sampling_month,
                                pressure_level_index,pressure_hPa,lat_bin,lon_bin)]
  maptrends[,`:=`(latitude_center=lat_bin+GRID_DEGREES/2,
                  longitude_center=lon_bin+GRID_DEGREES/2,grid_degrees=GRID_DEGREES)]
  mapfile <- file.path(OUT,sprintf('shiny_spatial_trends_month_%02d.csv',mm))
  fwrite(maptrends,mapfile)
  spatial_trend_files <- c(spatial_trend_files,mapfile)
  rm(byperiod,combo,z,maptrends);gc(verbose=FALSE)
  cat('Spatial trend month:',mm,'/ 12\n')
}
cat('Spatial map trend files:',length(spatial_trend_files),'(one per month)\n')
# Map frontend should use the bin bounds [lat_bin, lat_bin+5], [lon_bin, lon_bin+5].
# These rectangles include some ocean/Canada/Mexico outside CONUS; use a land mask
# for a publication-quality US map. Cells with sparse or changing year coverage need caution.
# Simple Plotly-ready month/pressure heatmaps for each variable, one per observation type.
for (v in unique(trends$variable)) for (obs in unique(trends$observation_type)) {
  z <- trends[variable==v & observation_type==obs & surface_type=='all' & is.finite(trend_per_decade) &
                is.finite(pressure_hPa)&pressure_hPa>0]
  if (!nrow(z)) next
  z[,month_label:=factor(month.abb[sampling_month],levels=month.abb)]
  z[,height_axis:=-log10(pressure_hPa)]
  g <- ggplot(z,aes(x=month_label,y=height_axis,fill=trend_per_decade))+
    geom_tile()+scale_fill_gradient2(low='#2563eb',mid='white',high='#dc2626',midpoint=0)+
    labs(title=paste(v,'trend:',obs),subtitle='Early-month AIRS; provisional QC; preliminary OLS',
      x='Month',y='Pressure axis: higher altitude upward',fill='Change / decade')+
    theme_bw(base_size=11)
  ggsave(file.path(OUT,paste0('heatmap_',v,'_',obs,'.png')),g,width=11,height=6,dpi=140)
}
cat('\nDONE: ',OUT,'\n', 'Yearly rows: ',nrow(all_yearly),
    '\nTrend rows: ',nrow(trends),'\n',sep='')
cat('WARNING: gas variable units are the source Parquet units; do not compare magnitudes across variables.\n')
cat('Cloud fraction assumed 0..1; verify source units. QC codes must be validated per variable.\n')
cat('Cloud-adjusted model is ecological exploratory sensitivity only, not a causal confounder estimate.\n')
cat('Surface filters: all / land (>=70% landFrac) / water (<=30% landFrac); mixed only in all.\n')
cat('Map: shiny_spatial_trends_month_01.csv ... _12.csv; coarse 5-degree bins, unmasked.\n')
cat('OLS intervals do not account for serial correlation, coverage shifts, or retrieval drift.\n')
